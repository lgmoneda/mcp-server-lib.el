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

run_emacs_function() {
	local func_name="$1"
	local err_msg="$2"
	local func_output
	local func_return_code

	func_output=$(emacsclient -s "$TEST_SERVER_NAME" -e "($func_name)")
	func_return_code=$?

	if [ $func_return_code -ne 0 ] || [ "$func_output" != "t" ]; then
		echo "FAIL: $err_msg, got output: $func_output, return code: $func_return_code"
		exit 1
	fi
}

TESTS_RUN=0

readonly TEST_SERVER_NAME="mcp-test-server-$$"
readonly STDIO_CMD="./emacs-mcp-stdio.sh --socket=$TEST_SERVER_NAME"

emacs -Q --daemon="$TEST_SERVER_NAME" --load "$(pwd)/mcp.el" --eval "(mcp-start)" 2>/dev/null &
readonly SERVER_PID=$!

# shellcheck disable=SC2317  # Called by trap
cleanup() {
	[ -n "${debug_log_file:-}" ] && [ -f "$debug_log_file" ] && rm -f "$debug_log_file"
	emacsclient -s "$TEST_SERVER_NAME" -e "(kill-emacs)" >/dev/null 2>&1 || true
	wait "$SERVER_PID" 2>/dev/null || true
}
trap cleanup EXIT

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

REQUEST='{"jsonrpc":"2.0","method":"tools/list","id":1}'
RESPONSE=$(echo "$REQUEST" | $STDIO_CMD)
readonly RESPONSE

readonly EXPECTED='{"jsonrpc":"2.0","id":1,"result":{"tools":[]}}'

if [ "$RESPONSE" != "$EXPECTED" ]; then
	echo "$TEST_CASE"
	echo "FAIL: Response from stdio adapter doesn't match expected"
	echo "Expected: $EXPECTED"
	echo "Actual: $RESPONSE"
	exit 1
fi

TESTS_RUN=$((TESTS_RUN + 1))

TEST_CASE="Test case 2: Debug logging with init and stop functions"

readonly INIT_FUNCTION="mcp-start"
readonly STOP_FUNCTION="mcp-stop"

REQUEST='{"jsonrpc":"2.0","method":"tools/list","id":2}'

# Stop MCP for this test as we want to test explicit init/stop functions
run_emacs_function "mcp-stop" "Failed to stop MCP"

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
check_log_contains "$debug_log_file" "MCP-INFO:.*init function\|No init function specified" "Debug log doesn't contain the init function info"
check_log_contains "$debug_log_file" "MCP-INFO:.*Stopping MCP with function\|No stop function specified" "Debug log doesn't contain the stop function info"

# Verify that call/response cycle works
check_log_contains "$debug_log_file" "MCP-REQUEST:" "Debug log doesn't contain the request"
check_log_contains "$debug_log_file" "MCP-RESPONSE:" "Debug log doesn't contain the response"

if ! grep -q -E '\[[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\]' "$debug_log_file"; then
	echo "$TEST_CASE"
	echo "FAIL: Debug log doesn't contain timestamps: $debug_log_file"
	exit 1
fi

rm "$debug_log_file"
TESTS_RUN=$((TESTS_RUN + 1))

# Start MCP again after the test
run_emacs_function "mcp-start" "Failed to restart MCP"

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

rm "$debug_log_file"
TESTS_RUN=$((TESTS_RUN + 1))

# Stop the MCP server at the end
run_emacs_function "mcp-stop" "Failed to stop MCP at end"

echo "$TESTS_RUN tests run OK!"
exit 0
