#!/bin/bash
# emacs-mcp-stdio-test.sh - Test the MCP stdio adapter

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

# Test case: tools/list request with our test server

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

# Make sure test response file is removed on exit
trap 'rm -f stdio-response.txt' EXIT

# Kill the test server when script exits
trap 'emacsclient -s "$TEST_SERVER_NAME" -e "(kill-emacs)" >/dev/null 2>&1 || true; wait "$SERVER_PID" 2>/dev/null || true' EXIT

# Exact string comparison
if [ "$(cat stdio-response.txt)" = "$EXPECTED" ]; then
	echo "PASS: Received expected response from stdio adapter"
	exit 0
else
	echo "FAIL: Response from stdio adapter doesn't match expected"
	echo "Expected: $EXPECTED"
	echo "Actual: $(cat stdio-response.txt)"
	exit 1
fi
