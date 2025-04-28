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

	# Escape quotes for elisp (replace " with \")
	escaped_line=${line//\"/\\\"}

	# Get response from emacsclient
	if [ -n "$SOCKET" ]; then
		response=$(emacsclient "${SOCKET_OPTIONS[@]}" -e "(mcp-process-jsonrpc \"$escaped_line\")")
	else
		response=$(emacsclient -e "(mcp-process-jsonrpc \"$escaped_line\")")
	fi

	# Log the raw response from emacsclient
	mcp_debug_log "RAW-RESPONSE" "$response"

	# Write the response to a temp file
	temp_file="/tmp/mcp-response.$$"
	echo "$response" >"$temp_file"

	# Use Emacs to properly unquote the response with a single eval command
	formatted_response=$(emacs -Q --batch --eval "(progn 
            (insert-file-contents \"$temp_file\") 
            (when (> (buffer-size) 0)
              (princ (car (read-from-string (buffer-string))))))")

	# Log the response
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
