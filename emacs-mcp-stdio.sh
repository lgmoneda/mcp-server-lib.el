#!/bin/bash
# emacs-mcp-stdio.sh - Connect to Emacs MCP server via stdio transport

set -eu -o pipefail

# Default values
INIT_FUNCTION=""
STOP_FUNCTION=""
SOCKET=""
EMACS_MCP_DEBUG_LOG=${EMACS_MCP_DEBUG_LOG:-""}

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
if [ -n "$SOCKET" ]; then
	readonly SOCKET_OPTIONS=("-s" "$SOCKET")
	mcp_debug_log "INFO" "Using socket: $SOCKET"
else
	readonly SOCKET_OPTIONS=()
fi

# Log init function info if provided
if [ -n "$INIT_FUNCTION" ]; then
	mcp_debug_log "INFO" "Using init function: $INIT_FUNCTION"
else
	mcp_debug_log "INFO" "No init function specified"
fi

# Initialize MCP if init function is provided
if [ -n "$INIT_FUNCTION" ]; then
	readonly INIT_CMD="emacsclient ${SOCKET_OPTIONS[*]} -e \"($INIT_FUNCTION)\""

	mcp_debug_log "INIT-CALL" "$INIT_CMD"

	# Execute the command and capture output and return code
	INIT_OUTPUT=$(eval "$INIT_CMD" 2>&1)
	INIT_RC=$?

	# Log results
	mcp_debug_log "INIT-RC" "$INIT_RC"
	mcp_debug_log "INIT-OUTPUT" "$INIT_OUTPUT"
else
	mcp_debug_log "INFO" "Skipping init function call (none provided)"
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

	# Process JSON-RPC request and return the result with proper UTF-8 encoding
	# Encode the response to base64 to avoid any character encoding issues
	elisp_expr="(base64-encode-string (encode-coding-string (mcp-process-jsonrpc (base64-decode-string \"$base64_input\")) 'utf-8 t) t)"

	# Get response from emacsclient - capture stderr for debugging
	readonly stderr_file="/tmp/mcp-stderr.$$"
	base64_response=$(emacsclient "${SOCKET_OPTIONS[@]}" -e "$elisp_expr" 2>"$stderr_file")

	# Check for stderr output
	if [ -s "$stderr_file" ]; then
		mcp_debug_log "EMACSCLIENT-STDERR" "$(cat "$stderr_file")"
	fi
	rm -f "$stderr_file"

	mcp_debug_log "BASE64-RESPONSE" "$base64_response"

	# Handle the base64 response - first strip quotes if present
	if [[ "$base64_response" == \"* && "$base64_response" == *\" ]]; then
		# Remove the surrounding quotes
		base64_response="${base64_response:1:${#base64_response}-2}"
		# Unescape any quotes inside
		base64_response="${base64_response//\\\"/\"}"
	fi

	# Decode the base64 content
	formatted_response=$(echo -n "$base64_response" | base64 -d)

	mcp_debug_log "RESPONSE" "$formatted_response"

	# Output the response
	echo "$formatted_response"
done

# Stop MCP if stop function is provided
if [ -n "$STOP_FUNCTION" ]; then
	mcp_debug_log "INFO" "Stopping MCP with function: $STOP_FUNCTION"

	readonly STOP_CMD="emacsclient ${SOCKET_OPTIONS[*]} -e \"($STOP_FUNCTION)\""

	mcp_debug_log "STOP-CALL" "$STOP_CMD"

	# Execute the command and capture output and return code
	STOP_OUTPUT=$(eval "$STOP_CMD" 2>&1)
	STOP_RC=$?

	# Log results
	mcp_debug_log "STOP-RC" "$STOP_RC"
	mcp_debug_log "STOP-OUTPUT" "$STOP_OUTPUT"
else
	mcp_debug_log "INFO" "Skipping stop function call (none provided)"
fi
