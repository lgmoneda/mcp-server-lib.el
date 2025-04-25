;;; mcp.el --- Model Context Protocol implementation -*- lexical-binding: t; -*-

;; Author: Laurynas Biveinis
;; Keywords: comm, ai, tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
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
;; an open standard for communication between AI applications and
;; language models.
;; See https://modelcontextprotocol.io/ for the protocol specification.

;;; Code:

(require 'json)

;;; Variables

(defgroup mcp nil
  "Model Context Protocol for Emacs."
  :group 'comm
  :prefix "mcp-")

;; Global state variables for singleton architecture
(defvar mcp--running nil
  "Whether the MCP server is currently running.")

(defvar mcp--tools (make-hash-table :test 'equal)
  "Hash table of registered MCP tools.")

(defvar mcp--name "emacs-mcp"
  "Name of the MCP server.")

(defvar mcp--protocol-version "2024-11-05"
  "Current MCP protocol version supported by this server.
Uses date-based versioning format to match current MCP practices.")

(defvar mcp--client-capabilities nil
  "Store client capabilities received during initialization.")

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

(defun mcp-start ()
  "Start the MCP server and begin handling client requests.

This function starts the MCP server that can process JSON-RPC
requests via `mcp-process-jsonrpc`.  Once started, the server
will dispatch incoming requests to the appropriate tool
handlers that have been registered with `mcp-register-tool'.

Example:
  (mcp-start)"
  (when mcp--running
    (error "MCP server is already running, stop it first with `mcp-stop`"))

  ;; Mark server as running
  (setq mcp--running t)
  t)

(defun mcp-stop ()
  "Stop the MCP server and release all associated resources.

Stops the server and performs necessary cleanup.

Example:
  (mcp-stop)"
  (unless mcp--running
    (error "MCP server is not running"))

  ;; Mark server as not running
  (setq mcp--running nil)
  t)

;;; Resources

;;; Tools

(defun mcp--generate-schema-from-function (func)
  "Generate JSON schema by analyzing FUNC's signature.
Returns a schema object suitable for tool registration.
Supports functions with zero or one argument only."
  (let ((arglist (help-function-arglist func)))
    (cond
     ;; No arguments case
     ((null arglist)
      '((type . "object")))

     ;; One argument case
     ((and (= 1 (length arglist))
           (symbolp (car arglist))
           (not (memq (car arglist) '(&optional &rest))))
      `((type . "object")
        (properties . ((file . ((type . "string")))))
        (required . [,(symbol-name (car arglist))])))

     ;; Everything else is unsupported
     (t
      (error "Only functions with zero or one argument are supported")))))

(defun mcp-register-tool (tool-id tool-description handler)
  "Register a tool with the MCP server.

Arguments:
  TOOL-ID          String identifier for the tool (e.g., \"list-files\")
  TOOL-DESCRIPTION String describing what the tool does
  HANDLER          Function to handle tool invocations

The HANDLER function's signature determines its input schema.
Currently only no-argument handlers are supported.

The handler should call (mcp-respond-with-result request-context result-data)
to return information to the client.

Example:
  (mcp-register-tool
    \"org-list-files\"
    \"Lists all available Org mode files for task management\"
    #\\='my-org-files-handler)"
  (let* ((schema (mcp--generate-schema-from-function handler))
         (tool (list :id tool-id
                     :description tool-description
                     :handler handler
                     :schema schema)))
    (puthash tool-id tool mcp--tools)
    tool))

(defun mcp-unregister-tool (tool-id)
  "Unregister a tool with ID TOOL-ID from the MCP server.

Arguments:
  TOOL-ID  String identifier for the tool to unregister

Returns t if the tool was found and removed, nil otherwise.

Example:
  (mcp-unregister-tool \"org-list-files\")"
  (when (gethash tool-id mcp--tools)
    (remhash tool-id mcp--tools)
    t))

;;; Prompts

(defvar mcp--prompts (make-hash-table :test 'equal)
  "Hash table of registered MCP prompts.")

;; Define custom error type for tool errors
(define-error 'mcp-tool-error "MCP tool error" 'user-error)

(defun mcp-tool-throw (error-message)
  "Signal a tool error with ERROR-MESSAGE.
The error will be properly formatted and sent to the client.

Arguments:
  ERROR-MESSAGE  String describing the error"
  (signal 'mcp-tool-error (list error-message)))

;;; Transport Layer

;;;; Stdio Transport

(defun mcp-process-jsonrpc (json-string)
  "Process a JSON-RPC message JSON-STRING and return the response.
This is the main entry point for stdio transport in MCP.

The function accepts a JSON-RPC 2.0 message string and returns
a JSON-RPC response string suitable for returning to clients via stdout.

When using the MCP server with emacsclient, invoke this function like:
emacsclient -e \\='(mcp-process-jsonrpc \"[JSON-RPC message]\")\\='

Example:
  (mcp-process-jsonrpc
   \"{\\\"jsonrpc\\\":\\\"2.0\\\",
     \\\"method\\\":\\\"mcp.server.describe\\\",\\\"id\\\":1}\")"
  (unless mcp--running
    (error "No active MCP server, start server with `mcp-start` first"))

  (condition-case err
      (mcp--handle-jsonrpc-request-internal json-string)
    (error
     (mcp--handle-error err))))

(defun mcp--handle-error (err)
  "Handle error ERR in MCP process by logging and creating an error response.
Returns a JSON-RPC error response string for internal errors."
  (json-encode
   `((jsonrpc . "2.0")
     (id . nil)
     (error . ((code . -32603)
               (message . ,(format "Internal error: %s"
                                   (error-message-string err))))))))

;;; JSON-RPC Handling

(defun mcp--handle-jsonrpc-request-internal (request-body)
  "Handle a JSON-RPC REQUEST-BODY for the global MCP server.
Returns a JSON-RPC response string."
  (let* ((request (json-read-from-string request-body))
         (jsonrpc (alist-get 'jsonrpc request))
         (id (alist-get 'id request))
         (method (alist-get 'method request))
         (params (alist-get 'params request)))
    (unless (equal jsonrpc "2.0")
      (mcp--jsonrpc-error id -32600 "Invalid Request: Not JSON-RPC 2.0"))

    (cond
     ;; Initialize handshake
     ((equal method "initialize")
      (mcp--handle-initialize id params))
     ;; Initialized notification
     ((equal method "initialized")
      (mcp--handle-initialized)
      (mcp--jsonrpc-response id nil))
     ;; Notifications/initialized format
     ((equal method "notifications/initialized")
      (mcp--handle-initialized)
      "")
     ;; Notifications/cancelled format
     ((equal method "notifications/cancelled")
      "")
     ;; List available tools
     ((equal method "tools/list")
      (let ((tool-list (vector)))
        (maphash (lambda (id tool)
                   (let ((tool-description (plist-get tool :description))
                         (tool-schema (or (plist-get tool :schema)
                                          '((type . "object")))))
                     (setq tool-list
                           (vconcat tool-list
                                    (vector `((name . ,id)
                                              (description . ,tool-description)
                                              (inputSchema . ,tool-schema)))))))
                 mcp--tools)
        (mcp--jsonrpc-response id `((tools . ,tool-list)))))
     ;; List available prompts
     ((equal method "prompts/list")
      (mcp--jsonrpc-response id `((prompts . ,(vector)))))
     ;; Tool invocation
     ((equal method "tools/call")
      (let* ((tool-name (alist-get 'name params))
             (tool (gethash tool-name mcp--tools))
             (tool-args (alist-get 'arguments params)))
        (if tool
            (let ((handler (plist-get tool :handler))
                  (context (list :id id)))
              (condition-case err
                  (let* ((result
                          ;; Pass first arg value for single-string-arg tools
                          ;; when arguments are present
                          (if (and tool-args (not (equal tool-args '())))
                              (let ((first-arg-value (cdr (car tool-args))))
                                (funcall handler first-arg-value))
                            (funcall handler)))
                         ;; Wrap the handler result in the MCP format
                         (formatted-result
                          `((content . ,(vector
                                         `((type . "text")
                                           (text . ,result))))
                            (isError . :json-false))))
                    (mcp-respond-with-result context formatted-result))
                ;; Handle tool-specific errors thrown with mcp-tool-throw
                (mcp-tool-error
                 (let ((formatted-error
                        `((content . ,(vector
                                       `((type . "text")
                                         (text . ,(cadr err)))))
                          (isError . t))))
                   (mcp-respond-with-result context formatted-error)))
                ;; Keep existing handling for all other errors
                (error
                 (mcp--jsonrpc-error id -32603
                                     (format "Internal error executing tool: %s"
                                             (error-message-string err))))))
          (mcp--jsonrpc-error id -32601
                              (format "Tool not found: %s" tool-name)))))
     ;; Method not found
     (t (mcp--jsonrpc-error id -32601
                            (format "Method not found: %s" method))))))

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

;;; MCP Protocol Methods

(defun mcp--handle-initialize (id params)
  "Handle initialize request with ID and PARAMS.

This implements the MCP initialize handshake, which negotiates protocol
version and capabilities between the client and server."
  (let ((client-capabilities (alist-get 'capabilities params)))
    ;; TODO: Add proper protocol version compatibility check
    ;; For now, accept any protocol version for compatibility
    ;; Store client capabilities for future use
    (setq mcp--client-capabilities client-capabilities)

    ;; Determine if we need to include tools capabilities
    ;; Include listChanged:true when tools are registered
    (let ((tools-capability (if (> (hash-table-count mcp--tools) 0)
                                '((listChanged . t))
                              (make-hash-table))))
      ;; Respond with server capabilities
      (mcp--jsonrpc-response
       id
       `((protocolVersion . ,mcp--protocol-version)
         (serverInfo . ((name . ,mcp--name)
                        (version . ,mcp--protocol-version)))
         ;; Format server capabilities according to MCP spec
         (capabilities . ((tools . ,tools-capability)
                          (resources . ,(make-hash-table))
                          (prompts . ,(make-hash-table)))))))))

(defun mcp--handle-initialized ()
  "Handle initialized notification from client.

This is called after successful initialization to complete the handshake.
The client sends this notification to acknowledge the server's response
to the initialize request."
  ;; Currently just a placeholder for future functionality
  nil)

(provide 'mcp)
;;; mcp.el ends here