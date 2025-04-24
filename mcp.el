;;; mcp.el --- Model Context Protocol implementation -*- lexical-binding: t; -*-

;; Author: Laurynas Biveinis
;; Keywords: comm, ai, tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (simple-httpd "1.5.1"))
;; URL: https://github.com/laurynas-biveinis/mcp.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; An Emacs Lisp implementation of the Model Context Protocol (MCP),
;; an open standard for communication between AI applications and language models.
;; See https://modelcontextprotocol.io/ for the protocol specification.

;;; Code:

(require 'json)
(require 'simple-httpd)

;;; Variables

(defgroup mcp nil
  "Model Context Protocol for Emacs."
  :group 'comm
  :prefix "mcp-")

(defcustom mcp-default-port 8000
  "Default port for the MCP HTTP server.
This follows the Python SDK default."
  :type 'integer
  :group 'mcp)

;; Internal variables
(defvar mcp--servers (make-hash-table :test 'equal)
  "Hash table of active MCP servers.")

;;; Core Functions

(defun mcp-respond-with-result (request-context result-data)
  "Send RESULT-DATA as response to the client through REQUEST-CONTEXT.

Arguments:
  REQUEST-CONTEXT  The MCP request context from the handler
  RESULT-DATA      The data to return to the client (any Elisp value)

The RESULT-DATA will be automatically converted to JSON-compatible format:
  - Strings, numbers, booleans are sent as-is
  - Symbols are converted to strings
  - Lists are converted to JSON arrays
  - Alists with string keys are converted to JSON objects
  - Other Elisp types are stringified appropriately

This function should be called exactly once per request handler invocation
to provide the response data to the client.

Example:
  (defun my-weather-handler (request-context tool-args)
    (let ((location (alist-get \\='location tool-args)))
      (mcp-respond-with-result
        request-context
        `((temperature . 72)
          (conditions . \"sunny\")
          (location . ,location)))))"
  (let ((id (plist-get request-context :id)))
    (mcp--jsonrpc-response id result-data)))

;;; Server

(defun mcp-create-server (name &optional port)
  "Create a new MCP server instance with NAME and optional PORT.

This function creates a new Model Context Protocol server that can register
tools and resources to be exposed to client applications.

Arguments:
  NAME  A string name for the server
  PORT  Port number for the HTTP server (defaults to `mcp-default-port')

The server is not started immediately.
Call `mcp-start-server' to begin accepting client connections.

Example:
  (setq my-server (mcp-create-server \"my-tool-server\"))
  (setq custom-server (mcp-create-server \"custom-server\" 9000))"
  (let ((server (list :name name
                      :tools (make-hash-table :test 'equal)
                      :port (or port mcp-default-port)
                      :running nil)))
    (puthash name server mcp--servers)
    server))

(defun mcp-start-server (server)
  "Start the MCP SERVER and begin handling client requests.

Arguments:
  SERVER  The MCP server instance created with `mcp-create-server'

This function starts the MCP server with the HTTP transport
and begins handling client requests asynchronously.  Once started,
the server will dispatch incoming requests to the appropriate tool
handlers that have been registered with `mcp-register-tool'.

Example:
  (mcp-start-server my-server)"
  (let ((port (plist-get server :port)))
    ;; Set up the HTTP server
    (setq httpd-port port)
    (httpd-start)

    ;; Define the MCP endpoint
    (defservlet mcp application/json (_path _query request)
      (condition-case err
          (let* ((body (cadr (assoc "Content" request)))
                 (response (mcp--handle-jsonrpc-request server body)))
            (insert response))
        (error
         (message "MCP servlet error: %s" (error-message-string err))
         (insert (json-encode
                  `((jsonrpc . "2.0")
                    (id . nil)
                    (error . ((code . -32603)
                              (message . ,(format "Internal error: %s" (error-message-string err)))))))))))

    ;; Mark server as running
    (plist-put server :running t)
    server))

(defun mcp-stop-server (server)
  "Stop the MCP SERVER and release all associated resources.

Arguments:
  SERVER  The MCP server instance to stop

Stops the server, closes all client connections, and performs
necessary cleanup.  After stopping, the server instance should
not be reused.

Example:
  (mcp-stop-server my-server)"
  ;; Stop the HTTP server
  (httpd-stop)

  ;; Undefine the servlet
  (fmakunbound 'httpd/mcp)

  ;; Mark server as not running
  (plist-put server :running nil)

  ;; Remove from active servers
  (remhash (plist-get server :name) mcp--servers)
  server)

;;; Resources

;;; Tools

(defun mcp-register-tool (server tool-id tool-description handler &optional schema)
  "Register a tool with the MCP SERVER.

Arguments:
  SERVER           The MCP server instance
  TOOL-ID          String identifier for the tool (e.g., \"list-files\")
  TOOL-DESCRIPTION String describing what the tool does
  HANDLER          Function to handle tool invocations
  SCHEMA           Optional JSON schema for tool parameters as Elisp alist

The HANDLER function must accept two arguments:
  REQUEST-CONTEXT  An MCP request context with response methods
  TOOL-ARGS        Tool arguments as native Elisp values

The handler should call (mcp-respond-with-result request-context result-data)
to return information to the client.

Example:
  (mcp-register-tool
    my-server
    \"org-list-files\"
    \"Lists all available Org mode files for task management\"
    #\\='my-org-files-handler)"
  (let ((tools (plist-get server :tools))
        (tool (list :id tool-id
                    :description tool-description
                    :handler handler
                    :schema schema)))
    (puthash tool-id tool tools)
    tool))

;;; Prompts

;;; Transport Layer

;;; JSON-RPC Handling

(defun mcp--handle-jsonrpc-request (server request-body)
  "Handle a JSON-RPC REQUEST-BODY for SERVER.
Returns a JSON-RPC response string."
  (let* ((request (json-read-from-string request-body))
         (jsonrpc (alist-get 'jsonrpc request))
         (id (alist-get 'id request))
         (method (alist-get 'method request))
         (params (alist-get 'params request)))
    (unless (equal jsonrpc "2.0")
      (mcp--jsonrpc-error id -32600 "Invalid Request: Not JSON-RPC 2.0"))

    (cond
     ;; Server status - for debugging, not part of official MCP
     ((equal method "mcp.server.status")
      (mcp--jsonrpc-response id `((name . ,(plist-get server :name))
                                  (version . "0.1.0"))))
     ;; Server description
     ((equal method "mcp.server.describe")
      (mcp--jsonrpc-response id `((name . ,(plist-get server :name))
                                  (version . "0.1.0")
                                  (protocol_version . "0.1.0")
                                  (capabilities . ,(vector "tools")))))
     ;; List available tools
     ((equal method "mcp.server.list_tools")
      (let ((tools-hash (plist-get server :tools))
            (tool-list (vector)))
        (maphash (lambda (id tool)
                   (setq tool-list
                         (vconcat tool-list
                                  (vector `((id . ,id)
                                            (description . ,(plist-get tool :description))
                                            (schema . ,(plist-get tool :schema)))))))
                 tools-hash)
        (mcp--jsonrpc-response id `((tools . ,tool-list)))))
     ;; Tool invocation
     ((string-match "^mcp\\.tool\\.\\(.+\\)" method)
      (let* ((tool-id (match-string 1 method))
             (tools (plist-get server :tools))
             (tool (gethash tool-id tools)))
        (if tool
            (let ((handler (plist-get tool :handler))
                  (context (list :id id :server server)))
              (condition-case err
                  (funcall handler context params)
                (error
                 (mcp--jsonrpc-error id -32603 (format "Internal error: %s" (error-message-string err))))))
          (mcp--jsonrpc-error id -32601 (format "Tool not found: %s" tool-id)))))
     ;; Method not found
     (t (mcp--jsonrpc-error id -32601 (format "Method not found: %s" method))))))

(defun mcp--jsonrpc-response (id result)
  "Create a JSON-RPC response with ID and RESULT."
  (json-encode `((jsonrpc . "2.0")
                 (id . ,id)
                 (result . ,result))))

(defun mcp--jsonrpc-error (id code message)
  "Create a JSON-RPC error response with ID, error CODE and MESSAGE."
  (json-encode `((jsonrpc . "2.0")
                 (id . ,id)
                 (error . ((code . ,code)
                           (message . ,message))))))

(provide 'mcp)
;;; mcp.el ends here
