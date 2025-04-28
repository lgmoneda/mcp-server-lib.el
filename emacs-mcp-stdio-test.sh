#!/bin/bash
# emacs-mcp-stdio-test.sh - Test the MCP stdio adapter

# Create a temp file for debug logging
DEBUG_LOG_FILE="/tmp/mcp-debug-$$.log"

# Setup - start a clean Emacs server for testing
TEST_SERVER_NAME="mcp-test-server-$$"

# Start a clean Emacs instance with our server name
echo "Starting test Emacs server..."
emacs -Q --daemon="$TEST_SERVER_NAME" --load "$(pwd)/mcp.el" &
SERVER_PID=$!

# Wait for server to be available (retry for up to 10 seconds)
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

# Test case 1: Basic functionality test
echo "Test case 1: Basic functionality test"

# Test the stdio adapter script
echo "Testing stdio adapter script..."
# Pipe the JSON-RPC request to the script
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' |
	./emacs-mcp-stdio.sh --socket="$TEST_SERVER_NAME" --init-function="mcp-start" >stdio-response.txt

# Debug output
echo "Stdio script response:"
cat stdio-response.txt
echo "End of response"

# Expected response for a clean server (empty tools array)
EXPECTED='{"jsonrpc":"2.0","id":1,"result":{"tools":[]}}'

# Make sure test response files are removed on exit
trap 'rm -f stdio-response.txt "$DEBUG_LOG_FILE"' EXIT

# Kill the test server when script exits
trap 'emacsclient -s "$TEST_SERVER_NAME" -e "(kill-emacs)" >/dev/null 2>&1 || true; wait "$SERVER_PID" 2>/dev/null || true' EXIT

# Exact string comparison
if [ "$(cat stdio-response.txt)" = "$EXPECTED" ]; then
	echo "PASS: Received expected response from stdio adapter"
else
	echo "FAIL: Response from stdio adapter doesn't match expected"
	echo "Expected: $EXPECTED"
	echo "Actual: $(cat stdio-response.txt)"
	exit 1
fi

# Test case 2: Debug logging test
echo "Test case 2: Debug logging test"

# Create a test request for debug logging
TEST_REQUEST='{"jsonrpc":"2.0","method":"tools/list","id":2}'

# Use the debug log environment variable
echo "Testing with debug logging enabled..."
echo "$TEST_REQUEST" | EMACS_MCP_DEBUG_LOG="$DEBUG_LOG_FILE" \
	./emacs-mcp-stdio.sh --socket="$TEST_SERVER_NAME" >stdio-response.txt

# Check if log file was created
if [ ! -f "$DEBUG_LOG_FILE" ]; then
	echo "FAIL: Debug log file was not created"
	exit 1
fi

echo "Debug log file created: $DEBUG_LOG_FILE"

# Check log file contents
echo "Debug log file contents:"
cat "$DEBUG_LOG_FILE"
echo "End of debug log"

# Verify log contains request
if ! grep -q "MCP-REQUEST.*$TEST_REQUEST" "$DEBUG_LOG_FILE"; then
	echo "FAIL: Debug log doesn't contain the request"
	exit 1
fi

# Verify log contains raw response from emacsclient
if ! grep -q "MCP-RAW-RESPONSE" "$DEBUG_LOG_FILE"; then
	echo "FAIL: Debug log doesn't contain the raw response from emacsclient"
	exit 1
fi

# Verify log contains formatted response
if ! grep -q "MCP-RESPONSE" "$DEBUG_LOG_FILE"; then
	echo "FAIL: Debug log doesn't contain the formatted response"
	exit 1
fi

# Verify if timestamps are present
if ! grep -q -E '\[[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\]' "$DEBUG_LOG_FILE"; then
	echo "FAIL: Debug log doesn't contain timestamps"
	exit 1
fi

# Test case 3: Debug logging with invalid path
echo "Test case 3: Debug logging with invalid path"

# Use a non-writable path for debug log
# The script should exit with an error status if log file path is invalid
echo "Testing with invalid log path (script should exit with error)..."
if echo "$TEST_REQUEST" | EMACS_MCP_DEBUG_LOG="/non-existent-dir/mcp-debug.log" \
	./emacs-mcp-stdio.sh --socket="$TEST_SERVER_NAME" >stdio-response.txt 2>/dev/null; then
	echo "FAIL: Script should exit with error when log path is invalid"
	exit 1
else
	echo "PASS: Script correctly exits with error when log path is invalid"
fi

echo "All tests PASSED!"
exit 0
