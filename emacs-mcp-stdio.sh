#!/bin/bash
# emacs-mcp-stdio.sh - Connect to Emacs MCP server via stdio transport

# Default values
INIT_FUNCTION="mcp-start"
STOP_FUNCTION="mcp-stop"
SOCKET=""

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

# Initialize MCP
if [ -n "$SOCKET" ]; then
	emacsclient "${SOCKET_OPTIONS[@]}" -e "($INIT_FUNCTION)" >/dev/null
else
	emacsclient -e "($INIT_FUNCTION)" >/dev/null
fi

# Process input and print response
while read -r line; do
	# Escape quotes for elisp (replace " with \")
	escaped_line=${line//\"/\\\"}

	# Get response from emacsclient
	if [ -n "$SOCKET" ]; then
		response=$(emacsclient "${SOCKET_OPTIONS[@]}" -e "(mcp-process-jsonrpc \"$escaped_line\")")
	else
		response=$(emacsclient -e "(mcp-process-jsonrpc \"$escaped_line\")")
	fi

	# Write the response to a temp file
	temp_file="/tmp/mcp-response.$$"
	echo "$response" >"$temp_file"

	# Use Emacs to properly unquote the response with a single eval command
	emacs -Q --batch --eval "(progn 
            (insert-file-contents \"$temp_file\") 
            (when (> (buffer-size) 0)
              (princ (car (read-from-string (buffer-string))))))"

	# Clean up temp file
	rm -f "$temp_file"
done

# Stop MCP when done
if [ -n "$SOCKET" ]; then
	emacsclient "${SOCKET_OPTIONS[@]}" -e "($STOP_FUNCTION)" >/dev/null
else
	emacsclient -e "($STOP_FUNCTION)" >/dev/null
fi
