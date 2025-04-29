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

(defcustom mcp-log-io nil
  "If non-nil, log all JSON-RPC messages to the *mcp-log* buffer."
  :group 'mcp
  :type 'boolean)

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

(defun mcp--extract-param-descriptions (func)
  "Extract parameter descriptions from FUNC's docstring.
The docstring should contain an \"MCP Parameters:\" section at the end,
with each parameter described as \"parameter-name - description\".
Returns an alist mapping parameter names to their descriptions.
Signals an error if a parameter is described multiple times,
doesn't match function arguments, or if any parameter is not documented."
  (let ((docstring (documentation func))
        (arglist (help-function-arglist func))
        (descriptions nil))
    (when docstring
      (when (string-match
             "MCP Parameters:[\n\r]+\\(\\(?:[ \t]+[^ \t\n\r].*[\n\r]*\\)*\\)"
             docstring)
        (let ((params-text (match-string 1 docstring))
              (param-regex
               "[ \t]+\\([^ \t\n\r]+\\)[ \t]*-[ \t]*\\(.*\\)[\n\r]*"))
          (with-temp-buffer
            (insert params-text)
            (goto-char (point-min))
            (while (re-search-forward param-regex nil t)
              (let ((param-name (match-string 1))
                    (param-desc (match-string 2)))
                ;; Check for duplicate parameter names
                (when (assoc param-name descriptions)
                  (error "Duplicate parameter '%s' in MCP Parameters"
                         param-name))
                ;; Check parameter name matches function arguments
                (unless (and (= 1 (length arglist))
                             (symbolp (car arglist))
                             (string= param-name (symbol-name (car arglist))))
                  (error "Parameter '%s' in MCP Parameters not in function args"
                         param-name))
                ;; Add to descriptions
                (push (cons param-name (string-trim param-desc))
                      descriptions))))))
      ;; Check that all function parameters have descriptions
      (when (and (= 1 (length arglist))
                 (symbolp (car arglist))
                 (not (memq (car arglist) '(&optional &rest))))
        (let ((arg-name (symbol-name (car arglist))))
          (unless (assoc arg-name descriptions)
            (error "Function parameter '%s' missing from MCP Parameters"
                   arg-name)))))
    descriptions))

(defun mcp--generate-schema-from-function (func)
  "Generate JSON schema by analyzing FUNC's signature.
Returns a schema object suitable for tool registration.
Supports functions with zero or one argument only.
Extracts parameter descriptions from the docstring if available."
  (let ((arglist (help-function-arglist func))
        (param-descriptions (mcp--extract-param-descriptions func)))
    (cond
     ;; No arguments case
     ((null arglist)
      '((type . "object")))

     ;; One argument case
     ((and (= 1 (length arglist))
           (symbolp (car arglist))
           (not (memq (car arglist) '(&optional &rest))))
      (let* ((arg-name (symbol-name (car arglist)))
             (description (cdr (assoc arg-name param-descriptions)))
             ;; Build property schema with type
             (property-schema `((type . "string")))
             ;; Add description if provided
             (property-schema
              (if description
                  (cons `(description . ,description) property-schema)
                property-schema)))
        `((type . "object")
          (properties . ((,arg-name . ,property-schema)))
          (required . [,arg-name]))))

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

;; Standard error codes as defined by the MCP
(defconst mcp--error-parse -32700
  "Error code for Parse Error.")

(defconst mcp--error-invalid-request -32600
  "Error code for Invalid Request.")

(defconst mcp--error-method-not-found -32601
  "Error code for Method Not Found.")

(defconst mcp--error-internal -32603
  "Error code for Internal Error.")

;; Define custom error type for tool errors
(define-error 'mcp-tool-error "MCP tool error" 'user-error)

(defun mcp-tool-throw (error-message)
  "Signal a tool error with ERROR-MESSAGE.
The error will be properly formatted and sent to the client.

Arguments:
  ERROR-MESSAGE  String describing the error"
  (signal 'mcp-tool-error (list error-message)))

;;; Logging

(defun mcp--log-json-rpc (direction json-message)
  "Log JSON-RPC message in DIRECTION with JSON-MESSAGE.
DIRECTION should be \"in\" for incoming, \"out\" for outgoing."
  (when mcp-log-io
    (let ((buffer (get-buffer-create "*mcp-log*"))
          (direction-prefix (if (string= direction "in") "->" "<-"))
          (direction-name (if (string= direction "in")
                              "(request)"
                            "(response)")))
      (with-current-buffer buffer
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (view-mode 1)
          (insert (format "%s %s [%s]\n"
                          direction-prefix
                          direction-name
                          json-message)))))))

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

  (mcp--log-json-rpc "in" json-string)

  ;; Step 1: Try to parse the JSON, handle parsing errors
  (let ((json-object nil)
        (response nil))
    ;; Attempt to parse the JSON
    (condition-case json-err
        (setq json-object (json-read-from-string json-string))
      (json-error
       ;; If JSON parsing fails, create a parse error response
       (setq response
             (mcp--jsonrpc-error
              nil
              mcp--error-parse
              (format "Parse error: %s"
                      (error-message-string json-err))))))
    ;; Step 2: Process the request if JSON parsing succeeded
    (unless response
      (condition-case err
          (setq response (mcp--handle-jsonrpc-request-internal json-object))
        (error
         (setq response (mcp--handle-error err)))))
    (mcp--log-json-rpc "out" response)
    response))

(defun mcp--handle-error (err)
  "Handle error ERR in MCP process by logging and creating an error response.
Returns a JSON-RPC error response string for internal errors."
  (mcp--jsonrpc-error
   nil
   mcp--error-internal
   (format "Internal error: %s"
           (error-message-string err))))

;;; JSON-RPC Handling

(defun mcp--dispatch-jsonrpc-method (id method params)
  "Dispatch a JSON-RPC request to the appropriate handler.
ID is the JSON-RPC request ID to use in response.
METHOD is the JSON-RPC method name to dispatch.
PARAMS is the JSON-RPC params object from the request.
Returns a JSON-RPC response string for the request."
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
                                  (vector
                                   `((name . ,id)
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
               (mcp--jsonrpc-error
                id mcp--error-internal
                (format "Internal error executing tool: %s"
                        (error-message-string err))))))
        (mcp--jsonrpc-error id mcp--error-invalid-request
                            (format "Tool not found: %s" tool-name)))))
   ;; Method not found
   (t (mcp--jsonrpc-error id mcp--error-method-not-found
                          (format "Method not found: %s" method)))))

(defun mcp--handle-jsonrpc-request-internal (request)
  "Handle a JSON-RPC REQUEST object for the global MCP server.
REQUEST is a parsed JSON object (alist).
Returns a JSON-RPC response string."
  (let* ((jsonrpc (alist-get 'jsonrpc request))
         (id (alist-get 'id request))
         (method (alist-get 'method request))
         (params (alist-get 'params request)))
    ;; Check for JSON-RPC 2.0 compliance first
    (if (not (equal jsonrpc "2.0"))
        ;; Return the error immediately for non-2.0 requests
        (mcp--jsonrpc-error
         id mcp--error-invalid-request "Invalid Request: Not JSON-RPC 2.0")

      ;; Only process further if it's a valid 2.0 request
      (mcp--dispatch-jsonrpc-method id method params))))

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