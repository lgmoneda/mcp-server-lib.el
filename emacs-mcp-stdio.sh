#!/bin/bash
# emacs-mcp-stdio.sh - Connect to Emacs MCP server via stdio transport

# Default values
INIT_FUNCTION="mcp-start"
STOP_FUNCTION="mcp-stop"
SOCKET=""

# Debug logging setup
if [ -n "$EMACS_MCP_DEBUG_LOG" ]; then
	# Verify log file is writable
	if ! touch "$EMACS_MCP_DEBUG_LOG" 2>/dev/null; then
		echo "Error: Cannot write to debug log file: $EMACS_MCP_DEBUG_LOG" >&2
		exit 1
	fi

	# Helper function for debug logging
	mcp_debug_log() {
		local direction="$1"
		local message="$2"
		local timestamp
		timestamp=$(date "+%Y-%m-%d %H:%M:%S")
		echo "[$timestamp] MCP-${direction}: ${message}" >>"$EMACS_MCP_DEBUG_LOG"
	}

	mcp_debug_log "INFO" "Debug logging enabled"
else
	# No-op function when debug logging is disabled
	mcp_debug_log() {
		:
	}
fi

# Parse command line arguments
while [ $# -gt 0 ]; do
	case "$1" in
	--init-function=*)
		INIT_FUNCTION="${1#--init-function=}"
		shift
		;;
	--stop-function=*)
		STOP_FUNCTION="${1#--stop-function=}"
		shift
		;;
	--socket=*)
		SOCKET="${1#--socket=}"
		shift
		;;
	*)
		echo "Unknown option: $1" >&2
		echo "Usage: $0 [--init-function=name] [--stop-function=name] [--socket=path]" >&2
		exit 1
		;;
	esac
done

# Set socket arguments if provided
SOCKET_OPTIONS=()
if [ -n "$SOCKET" ]; then
	SOCKET_OPTIONS=("-s" "$SOCKET")
fi

mcp_debug_log "INFO" "Starting MCP with init function: $INIT_FUNCTION, socket: $SOCKET"

# Initialize MCP
if [ -n "$SOCKET" ]; then
	# Construct full command for logging
	INIT_CMD="emacsclient ${SOCKET_OPTIONS[*]} -e \"($INIT_FUNCTION)\""
	mcp_debug_log "INIT-CALL" "$INIT_CMD"

	# Execute the command and capture output and return code
	INIT_OUTPUT=$(eval "$INIT_CMD" 2>&1)
	INIT_RC=$?

	# Log results
	mcp_debug_log "INIT-RC" "$INIT_RC"
	mcp_debug_log "INIT-OUTPUT" "$INIT_OUTPUT"
else
	# Construct full command for logging
	INIT_CMD="emacsclient -e \"($INIT_FUNCTION)\""
	mcp_debug_log "INIT-CALL" "$INIT_CMD"

	# Execute the command and capture output and return code
	INIT_OUTPUT=$(eval "$INIT_CMD" 2>&1)
	INIT_RC=$?

	# Log results
	mcp_debug_log "INIT-RC" "$INIT_RC"
	mcp_debug_log "INIT-OUTPUT" "$INIT_OUTPUT"
fi

# Process input and print response
while read -r line; do
	# Log the incoming request
	mcp_debug_log "REQUEST" "$line"

	# Base64 encode the raw JSON to avoid emacsclient transport issues
	# with a specific combination of length, UTF-8 characters, and quoting
	# that occurs in Test 5 with the Lithuanian letter 'ą'
	base64_input=$(echo -n "$line" | base64)
	mcp_debug_log "BASE64-INPUT" "$base64_input"

	# Construct elisp expression that:
	# 1. Decodes the Base64 input to get the raw JSON
	# 2. Processes with mcp-process-jsonrpc
	# 3. Base64 encodes the response
	elisp_expr="(base64-encode-string (mcp-process-jsonrpc (base64-decode-string \"$base64_input\")))"

	# Get response from emacsclient - capture stderr for debugging
	stderr_file="/tmp/mcp-stderr.$$"
	if [ -n "$SOCKET" ]; then
		base64_response=$(emacsclient "${SOCKET_OPTIONS[@]}" -e "$elisp_expr" 2>"$stderr_file")
	else
		base64_response=$(emacsclient -e "$elisp_expr" 2>"$stderr_file")
	fi

	# Check for stderr output
	if [ -s "$stderr_file" ]; then
		mcp_debug_log "EMACSCLIENT-STDERR" "$(cat "$stderr_file")"
	fi
	rm -f "$stderr_file"

	mcp_debug_log "BASE64-RESPONSE" "$base64_response"

	# Write the Base64 response to a temp file and process it
	temp_file="/tmp/mcp-response.$$"
	echo "$base64_response" >"$temp_file"

	# Extract the actual Base64 string from Elisp response and decode it in Emacs
	formatted_response=$(emacs -Q --batch --eval "(progn 
            (insert-file-contents \"$temp_file\") 
            (when (> (buffer-size) 0)
              (princ (base64-decode-string (car (read-from-string (buffer-string)))))))")
	mcp_debug_log "RESPONSE" "$formatted_response"

	# Output the response
	echo "$formatted_response"

	# Clean up temp file
	rm -f "$temp_file"
done

mcp_debug_log "INFO" "Stopping MCP with function: $STOP_FUNCTION"

# Stop MCP when done
if [ -n "$SOCKET" ]; then
	# Construct full command for logging
	STOP_CMD="emacsclient ${SOCKET_OPTIONS[*]} -e \"($STOP_FUNCTION)\""
	mcp_debug_log "STOP-CALL" "$STOP_CMD"

	# Execute the command and capture output and return code
	STOP_OUTPUT=$(eval "$STOP_CMD" 2>&1)
	STOP_RC=$?

	# Log results
	mcp_debug_log "STOP-RC" "$STOP_RC"
	mcp_debug_log "STOP-OUTPUT" "$STOP_OUTPUT"
else
	# Construct full command for logging
	STOP_CMD="emacsclient -e \"($STOP_FUNCTION)\""
	mcp_debug_log "STOP-CALL" "$STOP_CMD"

	# Execute the command and capture output and return code
	STOP_OUTPUT=$(eval "$STOP_CMD" 2>&1)
	STOP_RC=$?

	# Log results
	mcp_debug_log "STOP-RC" "$STOP_RC"
	mcp_debug_log "STOP-OUTPUT" "$STOP_OUTPUT"
fi
