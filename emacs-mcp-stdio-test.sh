#!/bin/bash
# emacs-mcp-stdio-test.sh - Test the MCP stdio adapter

set -eu -o pipefail

check_log_contains() {
	local log="$1"
	local pattern="$2"
	local err_msg="$3"

	if ! grep -q "$pattern" "$log"; then
		echo "$TEST_CASE"
		echo "FAIL: $err_msg: $log"
		exit 1
	fi
}

TESTS_RUN=0

readonly TEST_SERVER_NAME="mcp-test-server-$$"
readonly STDIO_CMD="./emacs-mcp-stdio.sh --socket=$TEST_SERVER_NAME"

emacs -Q --daemon="$TEST_SERVER_NAME" --load "$(pwd)/mcp.el" --eval "(mcp-start)" 2>/dev/null &
readonly SERVER_PID=$!

trap 'emacsclient -s "$TEST_SERVER_NAME" -e "(kill-emacs)" >/dev/null 2>&1 || true; wait "$SERVER_PID" 2>/dev/null || true' EXIT

readonly MAX_TRIES=50
COUNT=0
while ! emacsclient -s "$TEST_SERVER_NAME" -e 't' >/dev/null 2>&1; do
	sleep 0.2
	COUNT=$((COUNT + 1))
	if [ $COUNT -ge $MAX_TRIES ]; then
		echo "ERROR: Server failed to start after 10 seconds"
		exit 1
	fi
done

TEST_CASE="Test case 1: Basic functionality test"

RESPONSE=$(echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' |
	$STDIO_CMD)
readonly RESPONSE

readonly EXPECTED='{"jsonrpc":"2.0","id":1,"result":{"tools":[]}}'

if [ "$RESPONSE" = "$EXPECTED" ]; then
	TESTS_RUN=$((TESTS_RUN + 1))
else
	echo "$TEST_CASE"
	echo "FAIL: Response from stdio adapter doesn't match expected"
	echo "Expected: $EXPECTED"
	echo "Actual: $RESPONSE"
	exit 1
fi

TEST_CASE="Test case 2: Debug logging with init and stop functions"

# Define test parameters for explicit init and stop
readonly INIT_FUNCTION="mcp-start"
readonly STOP_FUNCTION="mcp-stop"
REQUEST='{"jsonrpc":"2.0","method":"tools/list","id":2}'

# Stop MCP for this test as we want to test explicit init/stop functions
output=$(emacsclient -s "$TEST_SERVER_NAME" -e "(mcp-stop)")
RETURN_CODE=$?
if [ $RETURN_CODE -ne 0 ] || [ "$output" != "t" ]; then
	echo "FAIL: Failed to stop MCP, got output: $output, return code: $RETURN_CODE"
	exit 1
fi

debug_log_file=$(mktemp /tmp/mcp-debug-XXXXXX.log)

echo "$REQUEST" | EMACS_MCP_DEBUG_LOG="$debug_log_file" \
	$STDIO_CMD --init-function="$INIT_FUNCTION" --stop-function="$STOP_FUNCTION" >stdio-response.txt

if [ ! -f "$debug_log_file" ]; then
	echo "$TEST_CASE"
	echo "FAIL: Debug log file was not created: $debug_log_file"
	exit 1
fi

check_log_contains "$debug_log_file" "MCP-REQUEST.*$REQUEST" "Debug log doesn't contain the request"
check_log_contains "$debug_log_file" "MCP-BASE64-RESPONSE" "Debug log doesn't contain the Base64 response from emacsclient"
check_log_contains "$debug_log_file" "MCP-RESPONSE" "Debug log doesn't contain the formatted response"

# Verify basic entries that should always exist
if ! grep -q "MCP-INFO:.*init function\|No init function specified" "$debug_log_file"; then
	echo "$TEST_CASE"
	echo "FAIL: Debug log doesn't contain the init function info: $debug_log_file"
	exit 1
fi

if ! grep -q "MCP-INFO:.*Stopping MCP with function\|No stop function specified" "$debug_log_file"; then
	echo "$TEST_CASE"
	echo "FAIL: Debug log doesn't contain the stop function info: $debug_log_file"
	exit 1
fi

# Verify that call/response cycle works
if ! grep -q "MCP-REQUEST:" "$debug_log_file"; then
	echo "$TEST_CASE"
	echo "FAIL: Debug log doesn't contain the request: $debug_log_file"
	exit 1
fi

if ! grep -q "MCP-RESPONSE:" "$debug_log_file"; then
	echo "$TEST_CASE"
	echo "FAIL: Debug log doesn't contain the response: $debug_log_file"
	exit 1
fi

if ! grep -q -E '\[[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\]' "$debug_log_file"; then
	echo "$TEST_CASE"
	echo "FAIL: Debug log doesn't contain timestamps: $debug_log_file"
	exit 1
fi

rm "$debug_log_file"
TESTS_RUN=$((TESTS_RUN + 1))

# Start MCP again after the test
output=$(emacsclient -s "$TEST_SERVER_NAME" -e "(mcp-start)")
RETURN_CODE=$?
if [ $RETURN_CODE -ne 0 ] || [ "$output" != "t" ]; then
	echo "FAIL: Failed to restart MCP, got output: $output, return code: $RETURN_CODE"
	exit 1
fi

TEST_CASE="Test case 3: Debug logging without init and stop functions"

# Test without init and stop functions
REQUEST='{"jsonrpc":"2.0","method":"tools/list","id":3}'
debug_log_file=$(mktemp /tmp/mcp-debug-XXXXXX.log)

echo "$REQUEST" | EMACS_MCP_DEBUG_LOG="$debug_log_file" \
	$STDIO_CMD >stdio-response.txt

if [ ! -f "$debug_log_file" ]; then
	echo "$TEST_CASE"
	echo "FAIL: Debug log file was not created: $debug_log_file"
	exit 1
fi

check_log_contains "$debug_log_file" "MCP-REQUEST" "Debug log doesn't contain the request"
check_log_contains "$debug_log_file" "MCP-BASE64-RESPONSE" "Debug log doesn't contain the Base64 response from emacsclient"
check_log_contains "$debug_log_file" "MCP-RESPONSE" "Debug log doesn't contain the formatted response"

# Verify we don't see init/stop function calls
if grep -q "MCP-INIT-CALL:" "$debug_log_file"; then
	echo "$TEST_CASE"
	echo "FAIL: Debug log contains init function call when it shouldn't: $debug_log_file"
	exit 1
fi

if grep -q "MCP-STOP-CALL:" "$debug_log_file"; then
	echo "$TEST_CASE"
	echo "FAIL: Debug log contains stop function call when it shouldn't: $debug_log_file"
	exit 1
fi

# Verify info messages about skipping init or logging without init
if ! grep -q "MCP-INFO:.*Skipping init function call\|No init function specified" "$debug_log_file"; then
	echo "$TEST_CASE"
	echo "FAIL: Debug log doesn't contain the init function skip message: $debug_log_file"
	exit 1
fi

# Verify info messages about skipping stop or stopping with function
if ! grep -q "MCP-INFO:.*Skipping stop function call\|Stopping MCP with function" "$debug_log_file"; then
	echo "$TEST_CASE"
	echo "FAIL: Debug log doesn't contain the stop function skip/use message: $debug_log_file"
	exit 1
fi

if ! grep -q -E '\[[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\]' "$debug_log_file"; then
	echo "$TEST_CASE"
	echo "FAIL: Debug log doesn't contain timestamps: $debug_log_file"
	exit 1
fi

rm "$debug_log_file"
TESTS_RUN=$((TESTS_RUN + 1))

TEST_CASE="Test case 4: Debug logging with invalid path"

if echo "$REQUEST" | EMACS_MCP_DEBUG_LOG="/non-existent-dir/mcp-debug.log" \
	$STDIO_CMD >stdio-response.txt 2>/dev/null; then
	echo "$TEST_CASE"
	echo "FAIL: Script should exit with error when log path is invalid"
	exit 1
fi
TESTS_RUN=$((TESTS_RUN + 1))

TEST_CASE="Test case 5: Special character escaping test"

emacsclient -s "$TEST_SERVER_NAME" -e "
(progn
  (defun mcp-test-quote-string ()
    \"Return a test string with a double quote and newline.\"
    \"\\\"\n\")

  (mcp-register-tool #'mcp-test-quote-string
    :id \"test-quote-string\" 
    :description \"Returns a test string with special characters\")
)
" >/dev/null
REQUEST="{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":4,\"params\":{\"name\":\"test-quote-string\"}}"

echo "$REQUEST" | $STDIO_CMD >stdio-response.txt

if ! grep -q '"text":"\\"\\n"' stdio-response.txt; then
	echo "$TEST_CASE"
	echo "FAIL: Final response doesn't have properly unescaped quote and newline"
	exit 1
fi
TESTS_RUN=$((TESTS_RUN + 1))

TEST_CASE="Test case 6: Original failing payload test"

emacsclient -s "$TEST_SERVER_NAME" -e "
(progn
  (defun mcp-test-original-payload ()
    \"Return the exact original payload that caused the issue.\"
    \"** aaa bbbbbbbbb ccccccc ddd eeeee ffffff                                :@ggggggggg:\\n   https://hhhhhh.iii/jjjjjjjjjjj/kkkkkkkkk/llllll/387\\n   mmmmmmmmm nnn oooooą pp qqqqq\\n** rr s tt uuuu++ vvvvvv wwww xxxx                                       :@yyyyyyyyy:\\n:zzzzzzz:\\n- aaaaa \\\"bbbb\\\"       cccc \\\"dddd\\\"       [1234-56-78 eee 90:12]\\n- fffff \\\"gggg\\\"       hhhh \\\"iiii\\\"       [1234-56-78 jjj 90:12]\\n- kkkkk \\\"llll\\\"       mmmm              [3456-78-90 nnn 12:34]\\n- nnnnn \\\"oooo\\\"       pppp              [5678-90-12 qqq 34:56]\\nrrrrr: [7890-12-34 sss 56:78]--[9012-34-56 ttt 78:90] =>  1:23\\n:uuu:\\nvvvvvv wwwwww xxxxxx yyyyyyyy zzz ~aaa::bbbbb~\\n\")

  (mcp-register-tool #'mcp-test-original-payload
    :id \"test-original-payload\" 
    :description \"Returns the exact original payload that caused the issue\")
)
" >/dev/null

REQUEST="{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":5,\"params\":{\"name\":\"test-original-payload\"}}"

# Run test 6 (multibyte character test)
debug_log_file="/tmp/test6-debug-$$.log"
echo "$REQUEST" |
	EMACS_MCP_DEBUG_LOG="$debug_log_file" $STDIO_CMD >stdio-response.txt 2>/dev/null

# Check for valid content (should have multibyte character)
# and absence of unwanted output (unknown message errors)
if grep -q "oooooą pp qqqqq" stdio-response.txt; then
	# Check for absence of the error message that base64 encoding is supposed to prevent
	if grep -q "\*ERROR\*: Unknown message" stdio-response.txt; then
		echo "$TEST_CASE"
		echo "ISSUE DETECTED: Base64 encoding failed to prevent unknown message errors"
		exit 1
	fi
else
	echo "$TEST_CASE"
	echo "ISSUE DETECTED: Response doesn't contain expected content with multibyte character"
	exit 1
fi

TESTS_RUN=$((TESTS_RUN + 1))

# Stop the MCP server at the end
output=$(emacsclient -s "$TEST_SERVER_NAME" -e "(mcp-stop)")
RETURN_CODE=$?
if [ $RETURN_CODE -ne 0 ] || [ "$output" != "t" ]; then
	echo "FAIL: Failed to stop MCP at end, got output: $output, return code: $RETURN_CODE"
	exit 1
fi

echo "$TESTS_RUN tests run OK!"
exit 0
