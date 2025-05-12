#!/bin/bash
# emacs-mcp-stdio-test.sh - Test the MCP stdio adapter

set -eu -o pipefail

TEST_SERVER_NAME="mcp-test-server-$$"

echo "Starting test Emacs server..."
emacs -Q --daemon="$TEST_SERVER_NAME" --load "$(pwd)/mcp.el" &
SERVER_PID=$!

MAX_TRIES=50
COUNT=0
while ! emacsclient -s "$TEST_SERVER_NAME" -e 't' >/dev/null 2>&1; do
	sleep 0.2
	COUNT=$((COUNT + 1))
	if [ $COUNT -ge $MAX_TRIES ]; then
		echo "ERROR: Server failed to start after 10 seconds"
		kill -9 $SERVER_PID >/dev/null 2>&1 || true
		exit 1
	fi
done
echo "Server started"

echo "Test case 1: Basic functionality test"

echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' |
	./emacs-mcp-stdio.sh --socket="$TEST_SERVER_NAME" --init-function="mcp-start" >stdio-response.txt

EXPECTED='{"jsonrpc":"2.0","id":1,"result":{"tools":[]}}'

trap 'rm -f stdio-response.txt' EXIT

trap 'emacsclient -s "$TEST_SERVER_NAME" -e "(kill-emacs)" >/dev/null 2>&1 || true; wait "$SERVER_PID" 2>/dev/null || true' EXIT

if [ "$(cat stdio-response.txt)" = "$EXPECTED" ]; then
	echo "PASS: Received expected response from stdio adapter"
else
	echo "FAIL: Response from stdio adapter doesn't match expected"
	echo "Expected: $EXPECTED"
	echo "Actual: $(cat stdio-response.txt)"
	exit 1
fi

echo "Test case 2: Debug logging with init and stop functions"

# Define test parameters for explicit init and stop
INIT_FUNCTION="mcp-start"
STOP_FUNCTION="mcp-stop"
TEST_REQUEST='{"jsonrpc":"2.0","method":"tools/list","id":2}'

debug_log_file=$(mktemp /tmp/mcp-debug-XXXXXX.log)

echo "$TEST_REQUEST" | EMACS_MCP_DEBUG_LOG="$debug_log_file" \
	./emacs-mcp-stdio.sh --socket="$TEST_SERVER_NAME" --init-function="$INIT_FUNCTION" --stop-function="$STOP_FUNCTION" >stdio-response.txt

if [ ! -f "$debug_log_file" ]; then
	echo "FAIL: Debug log file was not created"
	exit 1
fi

echo "Debug log file created: $debug_log_file"

if ! grep -q "MCP-REQUEST.*$TEST_REQUEST" "$debug_log_file"; then
	echo "FAIL: Debug log doesn't contain the request"
	exit 1
fi

if ! grep -q "MCP-BASE64-RESPONSE" "$debug_log_file"; then
	echo "FAIL: Debug log doesn't contain the Base64 response from emacsclient"
	exit 1
fi

if ! grep -q "MCP-RESPONSE" "$debug_log_file"; then
	echo "FAIL: Debug log doesn't contain the formatted response"
	exit 1
fi

# Verify basic entries that should always exist
if ! grep -q "MCP-INFO:.*init function\|No init function specified" "$debug_log_file"; then
	echo "FAIL: Debug log doesn't contain the init function info"
	exit 1
fi

if ! grep -q "MCP-INFO:.*Stopping MCP with function\|No stop function specified" "$debug_log_file"; then
	echo "FAIL: Debug log doesn't contain the stop function info"
	exit 1
fi

# Verify that call/response cycle works
if ! grep -q "MCP-REQUEST:" "$debug_log_file"; then
	echo "FAIL: Debug log doesn't contain the request"
	exit 1
fi

if ! grep -q "MCP-RESPONSE:" "$debug_log_file"; then
	echo "FAIL: Debug log doesn't contain the response"
	exit 1
fi

if ! grep -q -E '\[[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\]' "$debug_log_file"; then
	echo "FAIL: Debug log doesn't contain timestamps"
	exit 1
fi

echo "PASS: Debug logging with init and stop functions completed successfully"
rm "$debug_log_file"

echo "Test case 3: Debug logging without init and stop functions"

# Test without init and stop functions
TEST_REQUEST='{"jsonrpc":"2.0","method":"tools/list","id":3}'
debug_log_file=$(mktemp /tmp/mcp-debug-XXXXXX.log)

echo "$TEST_REQUEST" | EMACS_MCP_DEBUG_LOG="$debug_log_file" \
	./emacs-mcp-stdio.sh --socket="$TEST_SERVER_NAME" >stdio-response.txt

if [ ! -f "$debug_log_file" ]; then
	echo "FAIL: Debug log file was not created"
	exit 1
fi

# Verify essentials
if ! grep -q "MCP-REQUEST" "$debug_log_file"; then
	echo "FAIL: Debug log doesn't contain the request"
	exit 1
fi

if ! grep -q "MCP-BASE64-RESPONSE" "$debug_log_file"; then
	echo "FAIL: Debug log doesn't contain the Base64 response from emacsclient"
	exit 1
fi

if ! grep -q "MCP-RESPONSE" "$debug_log_file"; then
	echo "FAIL: Debug log doesn't contain the formatted response"
	exit 1
fi

# Verify we don't see init/stop function calls
if grep -q "MCP-INIT-CALL:" "$debug_log_file"; then
	echo "FAIL: Debug log contains init function call when it shouldn't"
	exit 1
fi

if grep -q "MCP-STOP-CALL:" "$debug_log_file"; then
	echo "FAIL: Debug log contains stop function call when it shouldn't"
	exit 1
fi

# Verify info messages about skipping init or logging without init
if ! grep -q "MCP-INFO:.*Skipping init function call\|No init function specified" "$debug_log_file"; then
	echo "FAIL: Debug log doesn't contain the init function skip message"
	exit 1
fi

# Verify info messages about skipping stop or stopping with function
if ! grep -q "MCP-INFO:.*Skipping stop function call\|Stopping MCP with function" "$debug_log_file"; then
	echo "FAIL: Debug log doesn't contain the stop function skip/use message"
	exit 1
fi

if ! grep -q -E '\[[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\]' "$debug_log_file"; then
	echo "FAIL: Debug log doesn't contain timestamps"
	exit 1
fi

echo "PASS: Debug logging without init and stop functions completed successfully"
rm "$debug_log_file"

echo "Test case 4: Debug logging with invalid path"

echo "Testing with invalid log path (script should exit with error)..."
if echo "$TEST_REQUEST" | EMACS_MCP_DEBUG_LOG="/non-existent-dir/mcp-debug.log" \
	./emacs-mcp-stdio.sh --socket="$TEST_SERVER_NAME" >stdio-response.txt 2>/dev/null; then
	echo "FAIL: Script should exit with error when log path is invalid"
	exit 1
else
	echo "PASS: Script correctly exits with error when log path is invalid"
fi

echo "Test case 5: Special character escaping test"

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
TEST_REQUEST="{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":4,\"params\":{\"name\":\"test-quote-string\"}}"

echo "$TEST_REQUEST" | ./emacs-mcp-stdio.sh --socket="$TEST_SERVER_NAME" --init-function="mcp-start" --stop-function="mcp-stop" >stdio-response.txt

if ! grep -q '"text":"\\"\\n"' stdio-response.txt; then
	echo "FAIL: Final response doesn't have properly unescaped quote and newline"
	exit 1
else
	echo "PASS"
fi

echo "Test case 6: Original failing payload test"

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

TEST_REQUEST="{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":5,\"params\":{\"name\":\"test-original-payload\"}}"

# Run test 6 (multibyte character test)
debug_log_file="/tmp/test6-debug-$$.log"
echo "$TEST_REQUEST" |
	EMACS_MCP_DEBUG_LOG="$debug_log_file" ./emacs-mcp-stdio.sh --socket="$TEST_SERVER_NAME" --init-function="mcp-start" --stop-function="mcp-stop" >stdio-response.txt 2>/dev/null

# Check for valid content (should have multibyte character)
# and absence of unwanted output (unknown message errors)
if grep -q "oooooą pp qqqqq" stdio-response.txt; then
	# Check for absence of the error message that base64 encoding is supposed to prevent
	if grep -q "\*ERROR\*: Unknown message" stdio-response.txt; then
		echo "ISSUE DETECTED: Base64 encoding failed to prevent unknown message errors"
		exit 1
	else
		echo "PASS: Response contains the multibyte character correctly and no unwanted messages"
	fi
else
	echo "ISSUE DETECTED: Response doesn't contain expected content with multibyte character"
	exit 1
fi

echo "All tests completed."
exit 0
