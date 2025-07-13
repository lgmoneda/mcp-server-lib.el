;;; mcp-server-lib-test.el --- Tests for mcp-server-lib.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; Version: 0.1.0
;; URL: https://github.com/laurynas-biveinis/mcp-server-lib.el

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

;; ERT tests for mcp-server-lib.el.

;;; Code:

(require 'ert)
(require 'mcp-server-lib)
(require 'mcp-server-lib-commands)
(require 'mcp-server-lib-metrics)
(require 'mcp-server-lib-ert)
(require 'json)

;;; Test data

(defconst mcp-server-lib-test--string-list-result "item1 item2 item3"
  "Test data for string list tool.")

(defconst mcp-server-lib-test--nonexistent-tool-id "non-existent-tool"
  "Tool ID for a non-existent tool used in tests.")

(defconst mcp-server-lib-test--unregister-tool-id "test-unregister"
  "Tool ID used for testing tool unregistration.")

(defconst mcp-server-lib-test--resource-read-request-id 777
  "Request ID used for resource read operations in tests.")

;;; JSON-RPC Error Codes for Testing

(defconst mcp-server-lib-test--error-parse -32700
  "JSON-RPC 2.0 Parse Error code for test assertions.")

(defconst mcp-server-lib-test--error-invalid-request -32600
  "JSON-RPC 2.0 Invalid Request error code for test assertions.")

(defconst mcp-server-lib-test--error-method-not-found -32601
  "JSON-RPC 2.0 Method Not Found error code for test assertions.")

(defconst mcp-server-lib-test--error-invalid-params -32602
  "JSON-RPC 2.0 Invalid Params error code for test assertions.")

(defconst mcp-server-lib-test--error-internal -32603
  "JSON-RPC 2.0 Internal Error code for test assertions.")

;;; Generic test handlers

(defun mcp-server-lib-test--return-string ()
  "Generic handler to return a test string.
Can be used for both tool and resource testing."
  "test result")

(defun mcp-server-lib-test--generic-error-handler ()
  "Generic handler that throws an error for testing error handling."
  (error "Generic error occurred"))

(defun mcp-server-lib-test--handler-to-be-undefined ()
  "Generic handler that will be undefined after registration.
Used for testing behavior when handlers no longer exist."
  "Handler was defined when called")

(defun mcp-server-lib-test--return-nil ()
  "Generic handler to return nil.
Can be used for both tool and resource testing."
  nil)

;;; Test tool handlers

(defun mcp-server-lib-test--tool-handler-mcp-server-lib-tool-throw ()
  "Test tool handler that always fails with `mcp-server-lib-tool-throw'."
  (mcp-server-lib-tool-throw "This tool intentionally fails"))

(defun mcp-server-lib-test--tool-handler-string-list ()
  "Test tool handler function to return a string with items."
  mcp-server-lib-test--string-list-result)

(defun mcp-server-lib-test--tool-handler-empty-string ()
  "Test tool handler function to return an empty string."
  "")

(defun mcp-server-lib-test--tool-handler-string-arg (input-string)
  "Test tool handler that accepts a string argument.
INPUT-STRING is the string argument passed to the tool.

MCP Parameters:
  input-string - test parameter for string input"
  (concat "Echo: " input-string))

(defun mcp-server-lib-test--tool-handler-duplicate-param
    (input-string)
  "Test handler with duplicate parameter.
INPUT-STRING is the string argument.

MCP Parameters:
  input-string - first description
  input-string - second description"
  (concat "Test: " input-string))

(defun mcp-server-lib-test--tool-handler-mismatched-param
    (input-string)
  "Test handler with mismatched parameter name.
INPUT-STRING is the string argument.

MCP Parameters:
  wrong-param-name - description for non-existent parameter"
  (concat "Test: " input-string))

(defun mcp-server-lib-test--tool-handler-missing-param (input-string)
  "Test handler with missing parameter documentation.
INPUT-STRING is the string argument.

MCP Parameters:"
  (concat "Test: " input-string))

;; Bytecode handler function that will be loaded during tests
(declare-function mcp-server-lib-test-bytecode-handler--handler
                  "mcp-server-lib-bytecode-handler-test")

;;; Test resource handlers

;;; Test helpers

(defmacro mcp-server-lib-test--with-undefined-function (function-symbol &rest body)
  "Execute BODY with FUNCTION-SYMBOL undefined, then restore it.
FUNCTION-SYMBOL should be a quoted symbol.
The original function definition is saved and restored after BODY executes."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((original-def (symbol-function ,function-symbol)))
     (unwind-protect
         (progn
           (fmakunbound ,function-symbol)
           ,@body)
       (fset ,function-symbol original-def))))

(defmacro mcp-server-lib-test--with-metrics-tracking
    (metrics-specs &rest body)
  "Execute BODY and verify metrics changed as expected.
METRICS-SPECS is a list of (METRICS-KEY EXPECTED-CALLS EXPECTED-ERRORS) lists.
Returns the result of the last form in BODY."
  (declare (indent 1) (debug t))
  (let ((before-bindings '())
        (after-checks '())
        (result-var (gensym "result")))
    ;; Build bindings and checks for each metric
    (dolist (spec metrics-specs)
      (let* ((key (car spec))
             (expected-calls (cadr spec))
             (expected-errors (caddr spec))
             (metrics-var (gensym "metrics"))
             (calls-var (gensym "calls"))
             (errors-var (gensym "errors")))
        ;; Add before bindings
        (push `(,metrics-var (mcp-server-lib-metrics-get ,key)) before-bindings)
        (push `(,calls-var (mcp-server-lib-metrics-calls ,metrics-var)) before-bindings)
        (push `(,errors-var (mcp-server-lib-metrics-errors ,metrics-var)) before-bindings)
        ;; Add after checks
        (push `(let ((metrics-after (mcp-server-lib-metrics-get ,key)))
                 (should (= (+ ,calls-var ,expected-calls)
                            (mcp-server-lib-metrics-calls metrics-after)))
                 (should (= (+ ,errors-var ,expected-errors)
                            (mcp-server-lib-metrics-errors metrics-after))))
              after-checks)))
    `(let* (,@(nreverse before-bindings)
            (,result-var (progn ,@body)))
       ,@(nreverse after-checks)
       ,result-var)))

(defmacro mcp-server-lib-test--verify-req-success (method &rest body)
  "Execute BODY and verify METHOD metrics show success (+1 call, +0 errors).
Captures metrics before BODY execution and asserts after that:
- calls increased by 1
- errors stayed the same

Note: This macro assumes the MCP server is already running.  If server
start/stop is required, use `mcp-server-lib-test--with-request' instead.

IMPORTANT: Any request-issuing test MUST use this macro or
`mcp-server-lib-test--with-request' to ensure proper metric tracking and
verification."
  (declare (indent defun) (debug t))
  `(mcp-server-lib-test--with-metrics-tracking
       ((,method 1 0))
     ,@body))

(defun mcp-server-lib-test--get-success-result (method request)
  "Process REQUEST and return the result from a successful response.
METHOD is the JSON-RPC method name for metrics verification.
This function expects the request to succeed and will fail the test if an
error is present in the response.  It verifies that the response contains no
error and that the method metrics show success before returning the result."
  (mcp-server-lib-test--verify-req-success
   method
   (let ((resp-obj
          (mcp-server-lib-process-jsonrpc-parsed request)))
     (should-not (alist-get 'error resp-obj))
     (alist-get 'result resp-obj))))

(defun mcp-server-lib-test--get-initialize-result ()
  "Send an MCP `initialize` request and return its result."
  (mcp-server-lib-test--get-success-result
   "initialize"
   (json-encode
    `(("jsonrpc" . "2.0")
      ("method" . "initialize") ("id" . 15)
      ("params" .
       (("protocolVersion" . "2025-03-26")
        ("capabilities" . ,(make-hash-table))))))))

(defun mcp-server-lib-test--assert-initialize-result (init-result tools resources)
  "Assert the structure of an initialize result.
INIT-RESULT is the result from an initialize request.
TOOLS is a boolean indicating if tools capability is expected.
RESOURCES is a boolean indicating if resources capability is expected."
  (let ((protocol-version (alist-get 'protocolVersion init-result))
        (capabilities (alist-get 'capabilities init-result))
        (server-info (alist-get 'serverInfo init-result)))
    (should (string= "2025-03-26" protocol-version))
    (should (string= mcp-server-lib--name (alist-get 'name server-info)))
    ;; Verify capabilities match expectations
    (when tools
      (should (assoc 'tools capabilities))
      ;; Empty objects {} in JSON are parsed as nil in Elisp
      (should-not (alist-get 'tools capabilities)))
    (when resources
      (should (assoc 'resources capabilities))
      (should-not (alist-get 'resources capabilities)))
    ;; Verify exact count
    (should (= (+ (if tools 1 0) (if resources 1 0))
               (length capabilities)))))

(cl-defmacro mcp-server-lib-test--with-server (&rest body &key tools resources
                                                     &allow-other-keys)
  "Run BODY with MCP server active and initialized.
Starts the server, sends initialize request, then runs BODY.
TOOLS and RESOURCES are booleans indicating expected capabilities.

IMPORTANT: Any test wishing to call `mcp-server-lib-start' MUST use this
macro instead, with very few exceptions - such as stopping and restarting
the server in the middle of a test."
  (declare (indent defun) (debug t))
  `(unwind-protect
       (progn
         (mcp-server-lib-start)
         (mcp-server-lib-test--assert-initialize-result
          (mcp-server-lib-test--get-initialize-result) ,tools ,resources)
         ;; Send initialized notification - should return nil
         (should-not
          (mcp-server-lib-process-jsonrpc
           (json-encode
            '(("jsonrpc" . "2.0")
              ("method" . "notifications/initialized")))))
         ,@body)
     (mcp-server-lib-stop)))

(defmacro mcp-server-lib-test--with-request (method &rest body)
  "Execute BODY with MCP server active and verify METHOD metrics.
This macro:
1. Starts the MCP server
2. Captures metrics before BODY execution
3. Executes BODY
4. Verifies the method was called exactly once with no errors
5. Stops the server

IMPORTANT: This macro or `mcp-server-lib-test--verify-req-success' MUST be used
for any successful request testing to ensure proper metric tracking."
  (declare (indent defun) (debug t))
  `(mcp-server-lib-test--with-server :tools nil :resources nil
     (mcp-server-lib-test--verify-req-success ,method
       ,@body)))

(defmacro mcp-server-lib-test--register-tool (handler &rest props-and-body)
  "Register a tool with HANDLER and properties, execute body, then unregister.
This is a simpler alternative to `mcp-server-lib-test--with-tools' for cases
where you need just one tool registered without starting the server.

The macro separates PROPS-AND-BODY into the tool properties (keywords) and
the body forms (everything after the last keyword-value pair).

IMPORTANT: Tests should use this macro instead of calling
`mcp-server-lib-register-tool' directly, except when testing registration
failures or error conditions.

Arguments:
  HANDLER         Function to handle tool invocations
  PROPS-AND-BODY  Tool properties followed by body forms"
  (declare (indent 1) (debug t))
  ;; Separate properties from body
  (let ((props '())
        (body props-and-body))
    ;; Extract properties (keyword-value pairs)
    (while (and body (keywordp (car body)))
      (push (pop body) props)
      (push (pop body) props))
    (setq props (nreverse props))
    ;; Extract tool ID for unregistration
    (let ((tool-id (plist-get props :id)))
      `(unwind-protect
           (progn
             (mcp-server-lib-register-tool ,handler ,@props)
             ,@body)
         (mcp-server-lib-unregister-tool ,tool-id)))))

(defmacro mcp-server-lib-test--with-tools (tools &rest body)
  "Run BODY with MCP server active and TOOLS registered.
All tools are automatically unregistered after BODY execution.

IMPORTANT: Tests should use this macro instead of calling
`mcp-server-lib-register-tool' directly, except when testing registration
failures or error conditions.

Arguments:
  TOOLS  List of tool registration specs, each a list of arguments for
         `mcp-server-lib-register-tool': (HANDLER &rest PROPERTIES)
  BODY   Forms to execute with server running and tools registered"
  (declare (indent 1) (debug t))
  ;; Build nested mcp-server-lib-test--register-tool calls
  ;; wrapping server start and body execution
  (let ((server-and-body
         `(mcp-server-lib-test--with-server :tools t :resources nil
            ,@body)))
    ;; Process tools in reverse order to build proper nesting
    (dolist (tool-spec (reverse tools))
      (let* ((handler (car tool-spec))
             (props (cdr tool-spec)))
        (setq server-and-body
              `(mcp-server-lib-test--register-tool ,handler ,@props
                 ,server-and-body))))
    server-and-body))

(defmacro mcp-server-lib-test--register-resource (uri handler &rest props-and-body)
  "Register a resource, execute body, then unregister.
Register a resource at URI with HANDLER and properties.
This is a simpler alternative to `mcp-server-lib-test--with-resources' for cases
where you need just one resource registered without starting the server.

The macro separates PROPS-AND-BODY into the resource properties (keywords) and
the body forms (everything after the last keyword-value pair).

IMPORTANT: Tests should use this macro instead of calling
`mcp-server-lib-register-resource' directly, except when testing registration
failures or error conditions.

Arguments:
  URI             Exact URI for the resource
  HANDLER         Function that returns the resource content
  PROPS-AND-BODY  Resource properties followed by body forms"
  (declare (indent 2) (debug t))
  ;; Separate properties from body
  (let ((props '())
        (body props-and-body))
    ;; Extract properties (keyword-value pairs)
    (while (and body (keywordp (car body)))
      (push (pop body) props)
      (push (pop body) props))
    (setq props (nreverse props))
    `(unwind-protect
         (progn
           (mcp-server-lib-register-resource ,uri ,handler ,@props)
           ,@body)
       (mcp-server-lib-unregister-resource ,uri))))

(defmacro mcp-server-lib-test--with-resources (resources &rest body)
  "Run BODY with MCP server active and RESOURCES registered.
All resources are automatically unregistered after BODY execution.

After registering all resources, automatically verifies that the resource list
contains exactly the registered resources with their expected properties.

IMPORTANT: Tests should use this macro instead of calling
`mcp-server-lib-register-resource' directly, except when testing registration
failures or error conditions.

Arguments:
  RESOURCES  List of resource registration specs, each a list of arguments for
             `mcp-server-lib-register-resource': (URI HANDLER &rest PROPERTIES)
  BODY       Forms to execute with server running and resources registered"
  (declare (indent 1) (debug t))
  ;; Build the verification code
  (let ((verification-code
         `(let ((resource-list (mcp-server-lib-test--get-resource-list)))
            ;; Check we have the expected number of resources
            (should (= ,(length resources) (length resource-list)))
            ;; Verify each registered resource appears in the list
            ,@(mapcar
               (lambda (resource-spec)
                 (let* ((uri (car resource-spec))
                        (props (cddr resource-spec))
                        (name (plist-get props :name))
                        (description (plist-get props :description))
                        (mime-type (plist-get props :mime-type)))
                   `(let ((resource (mcp-server-lib-test--find-resource-by-uri ,uri resource-list)))
                      (should resource)
                      (should (equal (alist-get 'uri resource) ,uri))
                      (should (equal (alist-get 'name resource) ,name))
                      ,@(when description
                          `((should (equal (alist-get 'description resource) ,description))))
                      ,@(when mime-type
                          `((should (equal (alist-get 'mimeType resource) ,mime-type)))))))
               resources))))
    ;; Build nested mcp-server-lib-test--register-resource calls
    ;; wrapping server start, verification, and body execution
    (let ((server-and-body
           `(mcp-server-lib-test--with-server :tools nil :resources t
              ;; Add verification after all resources are registered
              ,verification-code
              ,@body)))
      ;; Process resources in reverse order to build proper nesting
      (dolist (resource-spec (reverse resources))
        (let* ((uri (car resource-spec))
               (handler (cadr resource-spec))
               (props (cddr resource-spec)))
          (setq server-and-body
                `(mcp-server-lib-test--register-resource ,uri ,handler ,@props
                   ,server-and-body))))
      server-and-body)))

(defun mcp-server-lib-test--check-error-object (response expected-code expected-message)
  "Check that RESPONSE has error with EXPECTED-CODE and EXPECTED-MESSAGE."
  ;; Check that response contains only standard JSON-RPC fields plus error
  (should (equal 3 (length response))) ; jsonrpc, id, error
  (should (equal "2.0" (alist-get 'jsonrpc response)))
  (should (assq 'id response))
  (let ((error-obj (alist-get 'error response)))
    (should error-obj)
    (should (equal expected-code (alist-get 'code error-obj)))
    (should (equal expected-message (alist-get 'message error-obj)))))

(defun mcp-server-lib-test--check-jsonrpc-error
    (request expected-code expected-message)
  "Test that JSON-RPC REQUEST is rejected with EXPECTED-CODE and EXPECTED-MESSAGE."
  (let ((resp-obj (mcp-server-lib-process-jsonrpc-parsed request)))
    (mcp-server-lib-test--check-error-object resp-obj expected-code expected-message)))

(defun mcp-server-lib-test--check-invalid-jsonrpc-version (version)
  "Test that JSON-RPC request with VERSION is rejected properly."
  (mcp-server-lib-test--with-server :tools nil :resources nil
    (mcp-server-lib-test--check-jsonrpc-error
     (json-encode
      `(("jsonrpc" . ,version) ("method" . "tools/list") ("id" . 42)))
     mcp-server-lib-test--error-invalid-request "Invalid Request: Not JSON-RPC 2.0")))

(defun mcp-server-lib-test--call-tool (tool-id &optional id args)
  "Call a tool with TOOL-ID and return its successful result.
Optional ID is the JSON-RPC request ID (defaults to 1).
Optional ARGS is the association list of arguments to pass to the tool."
  (let* ((tool-metrics-key (format "tools/call:%s" tool-id))
         (tool-metrics (mcp-server-lib-metrics-get tool-metrics-key))
         (tool-calls-before
          (mcp-server-lib-metrics-calls tool-metrics))
         (tool-errors-before
          (mcp-server-lib-metrics-errors tool-metrics))
         (result
          (mcp-server-lib-test--get-success-result
           "tools/call"
           (mcp-server-lib-create-tools-call-request
            tool-id id args))))
    (let ((tool-metrics-after
           (mcp-server-lib-metrics-get tool-metrics-key)))
      (should
       (= (1+ tool-calls-before)
          (mcp-server-lib-metrics-calls tool-metrics-after)))
      (should
       (= tool-errors-before
          (mcp-server-lib-metrics-errors tool-metrics-after))))
    result))

(defun mcp-server-lib-test--verify-tool-not-found (tool-id)
  "Verify a call to non-existent tool with TOOL-ID returning an error."
  (mcp-server-lib-test--check-jsonrpc-error
   (mcp-server-lib-create-tools-call-request tool-id 999)
   mcp-server-lib-test--error-invalid-request
   (format "Tool not found: %s" tool-id)))

(defmacro mcp-server-lib-test--check-tool-call-error
    (tool-id &rest body)
  "Execute BODY and verify both call and error counts increased for TOOL-ID.
Creates a tools/call request and binds it to `request' for use in BODY.
Captures method and tool metrics before execution, executes BODY,
then verifies that both calls and errors increased by 1 at both levels."
  (declare (indent 1) (debug t))
  `(mcp-server-lib-test--with-metrics-tracking
       (("tools/call" 1 1)
        ((format "tools/call:%s" ,tool-id) 1 1))
     (let ((request (mcp-server-lib-create-tools-call-request ,tool-id 999)))
       ,@body)))

(defun mcp-server-lib-test--get-tool-list ()
  "Get the successful response to a standard `tools/list` request."
  (let ((result
         (alist-get
          'tools
          (mcp-server-lib-test--get-success-result
           "tools/list"
           (mcp-server-lib-create-tools-list-request)))))
    (should (arrayp result))
    result))

(defun mcp-server-lib-test--get-resource-list ()
  "Get the successful response to a standard `resources/list` request."
  (let ((result
         (alist-get
          'resources
          (mcp-server-lib-test--get-success-result
           "resources/list"
           (mcp-server-lib-create-resources-list-request)))))
    (should (arrayp result))
    result))

(defun mcp-server-lib-test--read-resource (uri)
  "Send a resources/read request for URI and return the parsed response."
  (let ((request (json-encode
                  `((jsonrpc . "2.0")
                    (id . ,mcp-server-lib-test--resource-read-request-id)
                    (method . "resources/read")
                    (params . ((uri . ,uri)))))))
    (mcp-server-lib-process-jsonrpc-parsed request)))

(defun mcp-server-lib-test--check-no-resources ()
  "Check that the resource list is empty."
  (let ((resources (mcp-server-lib-test--get-resource-list)))
    (should (= 0 (length resources)))))

(defun mcp-server-lib-test--check-single-resource (expected-fields)
  "Check the resource list to contain exactly one resource with EXPECTED-FIELDS.
EXPECTED-FIELDS is an alist of (field . value) pairs to verify."
  (let ((resources (mcp-server-lib-test--get-resource-list)))
    (should (= 1 (length resources)))
    (let ((resource (aref resources 0)))
      (should (= (length expected-fields) (length resource)))
      (dolist (field expected-fields)
        (should (equal (alist-get (car field) resource) (cdr field)))))))

(defun mcp-server-lib-test--find-resource-by-uri (uri resources)
  "Find a resource in RESOURCES array by its URI field."
  (seq-find (lambda (r) (equal (alist-get 'uri r) uri)) resources))

(defun mcp-server-lib-test--verify-resource-read (uri expected-fields)
  "Verify that reading resource at URI succeeds with EXPECTED-FIELDS.
EXPECTED-FIELDS is an alist of (field . value) pairs to verify in the content."
  (mcp-server-lib-test--verify-req-success
   "resources/read"
   (let* ((response (mcp-server-lib-test--read-resource uri))
          (response-keys (mapcar #'car response)))
     ;; Check response has exactly the expected fields
     (should (= 3 (length response-keys)))
     (should (member 'jsonrpc response-keys))
     (should (member 'id response-keys))
     (should (member 'result response-keys))
     ;; Check response field values
     (should (string= "2.0" (alist-get 'jsonrpc response)))
     (should (equal mcp-server-lib-test--resource-read-request-id
                    (alist-get 'id response)))
     ;; Check result structure
     (let* ((result (alist-get 'result response))
            (result-keys (mapcar #'car result)))
       (should (= 1 (length result-keys)))
       (should (member 'contents result-keys))
       ;; Check contents array
       (let ((contents (alist-get 'contents result)))
         (should (arrayp contents))
         (should (= 1 (length contents)))
         ;; Check content item structure
         (let* ((content (aref contents 0))
                (content-keys (mapcar #'car content)))
           ;; Verify exact field count
           (should (= (length expected-fields) (length content-keys)))
           ;; Verify each expected field exists and has correct value
           (dolist (field expected-fields)
             (should (member (car field) content-keys))
             (should (equal (alist-get (car field) content) (cdr field))))))))))

(defun mcp-server-lib-test--read-resource-error (uri expected-code expected-message)
  "Read resource at URI expecting an EXPECTED-CODE with EXPECTED-MESSAGE.
EXPECTED-MESSAGE should be the exact error message string."
  (let ((response (mcp-server-lib-test--read-resource uri)))
    ;; Check specific request ID for this resource read
    (should (equal mcp-server-lib-test--resource-read-request-id (alist-get 'id response)))
    (mcp-server-lib-test--check-error-object response expected-code expected-message)))

(defun mcp-server-lib-test--verify-tool-list-request (expected-tools)
  "Verify a `tools/list` response against EXPECTED-TOOLS.
EXPECTED-TOOLS should be an alist of (tool-name . tool-properties)."
  (let ((tools (mcp-server-lib-test--get-tool-list)))
    (should (= (length expected-tools) (length tools)))
    ;; Check each expected tool
    (dolist (expected expected-tools)
      (let* ((expected-name (car expected))
             (expected-props (cdr expected))
             (found-tool
              (seq-find
               (lambda (tool)
                 (string= expected-name (alist-get 'name tool)))
               tools)))
        (should found-tool)
        ;; Check expected properties
        (dolist (prop expected-props)
          (let ((prop-name (car prop))
                (prop-value (cdr prop)))
            (pcase prop-name
              ;; Special handling for nested annotations
              ('annotations
               (let ((annotations
                      (alist-get 'annotations found-tool)))
                 (should annotations)
                 (dolist (annot prop-value)
                   (should
                    (equal
                     (cdr annot)
                     (alist-get (car annot) annotations))))))
              ;; Regular property check
              (_
               (should
                (equal
                 prop-value (alist-get prop-name found-tool)))))))))))

(defun mcp-server-lib-test--verify-tool-schema-in-single-tool-list
    (&optional param-name param-type param-description)
  "Verify that schema of the only tool in the tool list has correct structure.
When PARAM-NAME is nil, verifies a zero-argument tool schema.
Otherwise, verifies a one-parameter tool schema with:
PARAM-NAME as the name of the parameter to validate.
PARAM-TYPE as the expected type of the parameter.
PARAM-DESCRIPTION as the expected description of the parameter."
  (let* ((tools (mcp-server-lib-test--get-tool-list))
         (tool (aref tools 0))
         (schema (alist-get 'inputSchema tool)))
    (should (equal "object" (alist-get 'type schema)))

    (if param-name
        (progn
          ;; One parameter case - verify required and properties
          (should
           (equal (vector param-name) (alist-get 'required schema)))
          (let ((param-schema
                 (alist-get
                  (intern param-name)
                  (alist-get 'properties schema))))
            (should (equal param-type (alist-get 'type param-schema)))
            (should
             (equal
              param-description
              (alist-get 'description param-schema)))))

      ;; Zero parameter case - schema should be just {type: "object"}
      (should-not (alist-get 'required schema))
      (should-not (alist-get 'properties schema)))))

(defun mcp-server-lib-test--check-mcp-server-lib-content-format
    (result expected-text)
  "Check that RESULT follows the MCP content format with EXPECTED-TEXT."
  (let* ((response `((result . ,result)))
         (text (mcp-server-lib-ert-check-text-response response)))
    (should (string= expected-text text))))

;;; Initialization and server capabilities tests

(ert-deftest mcp-server-lib-test-initialize-no-tools-no-resources ()
  "Test initialize when no tools or resources are registered.
When no tools or resources are registered, the capabilities object
should not include tools or resources fields at all."
  (mcp-server-lib-test--with-server :tools nil :resources nil))

(ert-deftest mcp-server-lib-test-initialize-with-tools-and-resources ()
  "Test initialize when both tools and resources are registered.
When both are registered, capabilities should include both fields."
  (mcp-server-lib-test--register-tool
   #'mcp-server-lib-test--return-string
   :id "test-tool"
   :description "Test tool"

   (mcp-server-lib-test--register-resource
    "test://resource"
    #'mcp-server-lib-test--return-string
    :name "Test Resource"
    (mcp-server-lib-test--with-server :tools t :resources t))))


(ert-deftest mcp-server-lib-test-initialize-old-protocol-version ()
  "Test server responds with its version for older client version."
  (mcp-server-lib-test--with-request "initialize"
    (let* ((init-request
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "initialize") ("id" . 16)
               ("params" .
                (("protocolVersion" . "2024-11-05")
                 ("capabilities" . ,(make-hash-table)))))))
           (result (mcp-server-lib-test--get-success-result
                    "initialize" init-request)))
      (mcp-server-lib-test--assert-initialize-result result nil nil))))

(ert-deftest mcp-server-lib-test-initialize-missing-protocol-version
    ()
  "Test initialize request without protocolVersion field."
  (mcp-server-lib-test--with-request "initialize"
    (let* ((init-request
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "initialize") ("id" . 17)
               ("params" .
                (("capabilities" . ,(make-hash-table)))))))
           (result (mcp-server-lib-test--get-success-result
                    "initialize" init-request)))
      (mcp-server-lib-test--assert-initialize-result result nil nil))))

(ert-deftest
    mcp-server-lib-test-initialize-non-string-protocol-version
    ()
  "Test initialize request with non-string protocolVersion."
  (mcp-server-lib-test--with-request "initialize"
    (let* ((init-request
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "initialize") ("id" . 18)
               ("params" .
                (("protocolVersion" . 123) ; Number instead of string
                 ("capabilities" . ,(make-hash-table)))))))
           (result (mcp-server-lib-test--get-success-result
                    "initialize" init-request)))
      (mcp-server-lib-test--assert-initialize-result result nil nil))))

(ert-deftest mcp-server-lib-test-initialize-malformed-params ()
  "Test initialize request with completely malformed params."
  ;; Test with params as a string instead of object
  (mcp-server-lib-test--with-request "initialize"
    (let* ((init-request
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "initialize")
               ("id" . 19)
               ("params" . "malformed"))))
           (result (mcp-server-lib-test--get-success-result
                    "initialize" init-request)))
      (mcp-server-lib-test--assert-initialize-result result nil nil))))

(ert-deftest mcp-server-lib-test-initialize-missing-params ()
  "Test initialize request without params field."
  (mcp-server-lib-test--with-request "initialize"
    (let* ((init-request
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "initialize")
               ("id" . 20))))
           (result (mcp-server-lib-test--get-success-result
                    "initialize" init-request)))
      (mcp-server-lib-test--assert-initialize-result result nil nil))))

(ert-deftest mcp-server-lib-test-initialize-null-protocol-version ()
  "Test initialize request with null protocolVersion."
  (mcp-server-lib-test--with-request "initialize"
    (let* ((init-request
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "initialize") ("id" . 21)
               ("params" .
                (("protocolVersion" . :json-null)
                 ("capabilities" . ,(make-hash-table)))))))
           (result (mcp-server-lib-test--get-success-result
                    "initialize" init-request)))
      (mcp-server-lib-test--assert-initialize-result result nil nil))))

(ert-deftest mcp-server-lib-test-initialize-empty-protocol-version ()
  "Test initialize request with empty string protocolVersion."
  (mcp-server-lib-test--with-request "initialize"
    (let* ((init-request
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "initialize") ("id" . 22)
               ("params" .
                (("protocolVersion" . "")
                 ("capabilities" . ,(make-hash-table)))))))
           (result (mcp-server-lib-test--get-success-result
                    "initialize" init-request)))
      (mcp-server-lib-test--assert-initialize-result result nil nil))))

(ert-deftest mcp-server-lib-test-initialize-with-valid-client-capabilities ()
  "Test initialize request with valid client capabilities (roots, sampling, experimental)."
  (mcp-server-lib-test--with-request "initialize"
    (let* ((init-request
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "initialize") ("id" . 23)
               ("params" .
                (("protocolVersion" . "2025-03-26")
                 ("capabilities" .
                  (("roots" . ,(make-hash-table))
                   ("sampling" . ,(make-hash-table))
                   ("experimental" . ,(make-hash-table)))))))))
           (result (mcp-server-lib-test--get-success-result
                    "initialize" init-request)))
      ;; Server should respond successfully, ignoring client capabilities
      (mcp-server-lib-test--assert-initialize-result result nil nil))))

;;; `mcp-server-lib-register-tool' tests

(ert-deftest mcp-server-lib-test-register-tool-error-missing-id ()
  "Test that tool registration with missing :id produces an error."
  (should-error
   (mcp-server-lib-register-tool
    #'mcp-server-lib-test--return-string
    :description "Test tool without ID")
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-tool-error-missing-description
    ()
  "Test that tool registration with missing :description produces an error."
  (should-error
   (mcp-server-lib-register-tool
    #'mcp-server-lib-test--return-string
    :id "test-tool-no-desc")
   :type 'error))

(ert-deftest mcp-server-lib-test-register-tool-error-missing-handler
    ()
  "Test that tool registration with non-function handler produces an error."
  (should-error
   (mcp-server-lib-register-tool
    "not-a-function"
    :id "test-tool-bad-handler"
    :description "Test tool with invalid handler")
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-tool-error-duplicate-param-description
    ()
  "Test that duplicate parameter descriptions cause an error."
  (should-error
   (mcp-server-lib-register-tool
    #'mcp-server-lib-test--tool-handler-duplicate-param
    :id "duplicate-param-tool"
    :description "Tool with duplicate parameter")
   :type 'error))

(ert-deftest mcp-server-lib-test-register-tool-error-mismatched-param
    ()
  "Test that parameter names must match function arguments."
  (should-error
   (mcp-server-lib-register-tool
    #'mcp-server-lib-test--tool-handler-mismatched-param
    :id "mismatched-param-tool"
    :description "Tool with mismatched parameter")
   :type 'error))

(ert-deftest mcp-server-lib-test-register-tool-error-missing-param ()
  "Test that all function parameters must be documented."
  (should-error
   (mcp-server-lib-register-tool
    #'mcp-server-lib-test--tool-handler-missing-param
    :id "missing-param-tool"
    :description "Tool with missing parameter docs")
   :type 'error))

(ert-deftest mcp-server-lib-test-register-tool-error-duplicate-id ()
  "Test reference counting behavior when registering a tool with duplicate ID.
With reference counting, duplicate registrations should succeed and increment
the reference count, returning the original tool definition."
  (mcp-server-lib-test--with-server
   :tools nil :resources nil
   (mcp-server-lib-test--register-tool
    #'mcp-server-lib-test--return-string
    :id "duplicate-test"
    :description "First registration"

    (mcp-server-lib-test--register-tool
     #'mcp-server-lib-test--return-string
     :id "duplicate-test"
     :description "Second registration - should be ignored"
     ;; Tool should be callable after registrations (ref count = 2)
     (let ((result (mcp-server-lib-test--call-tool "duplicate-test" 1)))
       (mcp-server-lib-test--check-mcp-server-lib-content-format
        result "test result")))
    
    ;; After inner macro completes, it unregisters once (ref count goes from 2 to 1)
    ;; Tool should still be callable because outer registration is still active
    (let ((result (mcp-server-lib-test--call-tool "duplicate-test" 2)))
      (mcp-server-lib-test--check-mcp-server-lib-content-format
       result "test result")))
   
   ;; After outer macro completes, it unregisters again (ref count = 0)
   ;; Tool should no longer be callable
   (mcp-server-lib-test--verify-tool-not-found "duplicate-test")))

(ert-deftest mcp-server-lib-test-register-tool-bytecode ()
  "Test schema generation for a handler loaded as bytecode.
This test verifies that MCP can correctly extract parameter information
from a function loaded from bytecode rather than interpreted elisp."
  (let* ((source-file
          (expand-file-name
           "mcp-server-lib-bytecode-handler-test.el"))
         (bytecode-file (byte-compile-dest-file source-file)))
    (should (byte-compile-file source-file))

    (should (load bytecode-file nil t t))

    (mcp-server-lib-test--with-tools
        ((#'mcp-server-lib-test-bytecode-handler--handler
          :id "bytecode-handler"
          :description "A tool with a handler loaded from bytecode"))
      (mcp-server-lib-test--verify-tool-schema-in-single-tool-list
       "input-string"
       "string"
       "Input string parameter for bytecode testing"))

    (when (file-exists-p bytecode-file)
      (delete-file bytecode-file))))

;;; `mcp-server-lib-unregister-tool' tests

(ert-deftest mcp-server-lib-test-unregister-tool ()
  "Test that `mcp-server-lib-unregister-tool' removes a tool correctly."
  (mcp-server-lib-test--with-server
   :tools nil :resources nil
   (mcp-server-lib-test--register-tool
    #'mcp-server-lib-test--return-string
    :id mcp-server-lib-test--unregister-tool-id
    :description "Tool for unregister test"

    (mcp-server-lib-test--verify-tool-list-request
     `((,mcp-server-lib-test--unregister-tool-id
        .
        ((description . "Tool for unregister test")
         (inputSchema . ((type . "object")))))))

    (let ((result
           (mcp-server-lib-test--call-tool
            mcp-server-lib-test--unregister-tool-id
            44)))
      (mcp-server-lib-test--check-mcp-server-lib-content-format
       result "test result")))

   ;; After macro cleanup, verify tool is gone
   (mcp-server-lib-test--verify-tool-list-request '())
   (mcp-server-lib-test--verify-tool-not-found
    mcp-server-lib-test--unregister-tool-id)))

(ert-deftest mcp-server-lib-test-unregister-tool-nonexistent ()
  "Test that `mcp-server-lib-unregister-tool' returns nil for missing tools."
  (mcp-server-lib-test--register-tool
   #'mcp-server-lib-test--return-string
   :id "test-other"
   :description "Other test tool"
   (should-not (mcp-server-lib-unregister-tool "nonexistent-tool"))))

(ert-deftest mcp-server-lib-test-unregister-tool-when-no-tools ()
  "Test `mcp-server-lib-unregister-tool' when no tools are registered."
  (should-not (mcp-server-lib-unregister-tool "any-tool")))

;;; Notification tests

(ert-deftest mcp-server-lib-test-notifications-cancelled ()
  "Test the MCP `notifications/cancelled` request handling."
  (mcp-server-lib-test--with-request "notifications/cancelled"
    (let* ((notifications-cancelled
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "notifications/cancelled"))))
           (response
            (mcp-server-lib-process-jsonrpc notifications-cancelled)))
      ;; Notifications are one-way, should return nil
      (should-not response))))

;;; `mcp-server-lib-create-tools-list-request' tests

(ert-deftest mcp-server-lib-test-create-tools-list-request-with-id ()
  "Test `mcp-server-lib-create-tools-list-request' with a specified ID."
  (let* ((id 42)
         (request (mcp-server-lib-create-tools-list-request id))
         (parsed (json-read-from-string request)))
    ;; Verify basic JSON-RPC structure
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "tools/list" (alist-get 'method parsed)))
    (should (equal id (alist-get 'id parsed)))))

(ert-deftest mcp-server-lib-test-create-tools-list-request-default-id
    ()
  "Test `mcp-server-lib-create-tools-list-request' with default ID."
  (let* ((request (mcp-server-lib-create-tools-list-request))
         (parsed (json-read-from-string request)))
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "tools/list" (alist-get 'method parsed)))
    (should (equal 1 (alist-get 'id parsed)))))

;;; `mcp-server-lib-create-resources-list-request' tests

(ert-deftest mcp-server-lib-test-create-resources-list-request-with-id ()
  "Test `mcp-server-lib-create-resources-list-request' with a specified ID."
  (let* ((id 42)
         (request (mcp-server-lib-create-resources-list-request id))
         (parsed (json-read-from-string request)))
    ;; Verify basic JSON-RPC structure
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "resources/list" (alist-get 'method parsed)))
    (should (equal id (alist-get 'id parsed)))))

(ert-deftest mcp-server-lib-test-create-resources-list-request-default-id ()
  "Test `mcp-server-lib-create-resources-list-request' with default ID."
  (let* ((request (mcp-server-lib-create-resources-list-request))
         (parsed (json-read-from-string request)))
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "resources/list" (alist-get 'method parsed)))
    (should (equal 1 (alist-get 'id parsed)))))

;;; tools/list tests

(ert-deftest mcp-server-lib-test-tools-list-one ()
  "Test `tools/list` returning one tool with correct fields and schema."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--return-string
        :id "test-tool"
        :description "A tool for testing"))
    (mcp-server-lib-test--verify-tool-list-request
     '(("test-tool" .
        ((description . "A tool for testing")
         (inputSchema . ((type . "object")))))))))

(ert-deftest mcp-server-lib-test-tools-list-with-title ()
  "Test that `tools/list` includes title in response."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--return-string
        :id "tool-with-title"
        :description "A tool for testing titles"
        :title "Friendly Tool Name"))
    (mcp-server-lib-test--verify-tool-list-request
     '(("tool-with-title" .
        ((description . "A tool for testing titles")
         (annotations . ((title . "Friendly Tool Name")))
         (inputSchema . ((type . "object")))))))))

(ert-deftest mcp-server-lib-test-tools-list-two ()
  "Test the `tools/list` method returning multiple tools with correct fields."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--return-string
        :id "test-tool-1"
        :description "First tool for testing")
       (#'mcp-server-lib-test--return-string
        :id "test-tool-2"
        :description "Second tool for testing"))
    (mcp-server-lib-test--verify-req-success "tools/list"
      (mcp-server-lib-test--verify-tool-list-request
       '(("test-tool-1" .
          ((description . "First tool for testing")
           (inputSchema . ((type . "object")))))
         ("test-tool-2" .
          ((description . "Second tool for testing")
           (inputSchema . ((type . "object"))))))))))

(ert-deftest mcp-server-lib-test-tools-list-zero ()
  "Test the `tools/list` method returning empty array with no tools."
  (mcp-server-lib-test--with-server :tools nil :resources nil
    (mcp-server-lib-test--verify-tool-list-request '())))

(ert-deftest mcp-server-lib-test-tools-list-schema-one-arg-handler ()
  "Test that `tools/list` schema includes parameter descriptions."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-string-arg
        :id "requires-arg"
        :description "A tool that requires an argument"))
    (mcp-server-lib-test--verify-req-success "tools/list"
      (mcp-server-lib-test--verify-tool-schema-in-single-tool-list
       "input-string" "string" "test parameter for string input"))))


(ert-deftest mcp-server-lib-test-tools-list-read-only-hint ()
  "Test that `tools/list` response includes readOnlyHint=true."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--return-string
        :id "read-only-tool"
        :description "A tool that doesn't modify its environment"
        :read-only t))
    (mcp-server-lib-test--verify-req-success "tools/list"
      (mcp-server-lib-test--verify-tool-list-request
       '(("read-only-tool" .
          ((description
            . "A tool that doesn't modify its environment")
           (annotations . ((readOnlyHint . t)))
           (inputSchema . ((type . "object"))))))))))

(ert-deftest mcp-server-lib-test-tools-list-read-only-hint-false ()
  "Test that `tools/list` response includes readOnlyHint=false."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--return-string
        :id "non-read-only-tool"
        :description "Tool that modifies its environment"
        :read-only nil))
    (mcp-server-lib-test--verify-req-success "tools/list"
      (mcp-server-lib-test--verify-tool-list-request
       '(("non-read-only-tool" .
          ((description . "Tool that modifies its environment")
           (annotations . ((readOnlyHint . :json-false)))
           (inputSchema . ((type . "object"))))))))))

(ert-deftest mcp-server-lib-test-tools-list-multiple-annotations ()
  "Test `tools/list` response including multiple annotations."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--return-string
        :id "multi-annotated-tool"
        :description "A tool with multiple annotations"
        :title "Friendly Multi-Tool"
        :read-only t))
    (mcp-server-lib-test--verify-req-success "tools/list"
      (mcp-server-lib-test--verify-tool-list-request
       '(("multi-annotated-tool" .
          ((description . "A tool with multiple annotations")
           (annotations
            . ((title . "Friendly Multi-Tool") (readOnlyHint . t)))
           (inputSchema . ((type . "object"))))))))))

;;; `mcp-server-lib-create-tools-call-request' tests

(ert-deftest mcp-server-lib-test-create-tools-call-request-id-and-args
    ()
  "Test `mcp-server-lib-create-tools-call-request' with ID and arguments."
  (let* ((tool-name "test-tool")
         (id 42)
         (args '(("arg1" . "value1") ("arg2" . "value2")))
         (request
          (mcp-server-lib-create-tools-call-request
           tool-name id args))
         (parsed (json-read-from-string request))
         (params (alist-get 'params parsed)))
    ;; Verify basic JSON-RPC structure
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "tools/call" (alist-get 'method parsed)))
    (should (equal id (alist-get 'id parsed)))
    ;; Verify params structure
    (should params)
    (should (equal tool-name (alist-get 'name params)))
    (should (alist-get 'arguments params))
    (should
     (equal "value1" (alist-get 'arg1 (alist-get 'arguments params))))
    (should
     (equal
      "value2" (alist-get 'arg2 (alist-get 'arguments params))))))

(ert-deftest mcp-server-lib-test-create-tools-call-request-default-id
    ()
  "Test `mcp-server-lib-create-tools-call-request' with default ID."
  (let* ((tool-name "test-tool")
         (request
          (mcp-server-lib-create-tools-call-request tool-name))
         (parsed (json-read-from-string request))
         (params (alist-get 'params parsed)))
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "tools/call" (alist-get 'method parsed)))
    (should (equal 1 (alist-get 'id parsed)))
    (should params)
    (should (equal tool-name (alist-get 'name params)))
    (should (equal '() (alist-get 'arguments params)))))

(ert-deftest mcp-server-lib-test-create-tools-call-request-empty-args
    ()
  "Test `mcp-server-lib-create-tools-call-request' with empty arguments list."
  (let* ((tool-name "test-tool")
         (id 43)
         (request
          (mcp-server-lib-create-tools-call-request tool-name id '()))
         (parsed (json-read-from-string request))
         (params (alist-get 'params parsed)))
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "tools/call" (alist-get 'method parsed)))
    (should (equal id (alist-get 'id parsed)))
    (should params)
    (should (equal tool-name (alist-get 'name params)))
    (should (equal '() (alist-get 'arguments params)))))

;;; tools/call tests

(ert-deftest mcp-server-lib-test-tools-call-mcp-server-lib-tool-throw
    ()
  "Test tool handler calling `mcp-server-lib-tool-throw'."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-mcp-server-lib-tool-throw
        :id "failing-tool"
        :description "A tool that always fails"))
    (mcp-server-lib-test--check-tool-call-error "failing-tool"
      (let* ((resp-obj
              (mcp-server-lib-process-jsonrpc-parsed request))
             (text
              (mcp-server-lib-ert-check-text-response resp-obj t)))
        (should (string= "This tool intentionally fails" text))))))

(ert-deftest mcp-server-lib-test-tools-call-generic-error ()
  "Test that generic errors use standard JSON-RPC error format."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--generic-error-handler
        :id "generic-error-tool"
        :description "A tool that throws a generic error"))
    (mcp-server-lib-test--check-tool-call-error "generic-error-tool"
      (mcp-server-lib-test--check-jsonrpc-error
       request
       mcp-server-lib-test--error-internal "Internal error executing tool: Generic error occurred"))))

(ert-deftest mcp-server-lib-test-tools-call-no-args ()
  "Test the `tools/call` request with a tool that takes no arguments."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-string-list
        :id "string-list-tool"
        :description "A tool that returns a string with items"))
    (let ((result
           (mcp-server-lib-test--call-tool "string-list-tool" 9)))
      (mcp-server-lib-test--check-mcp-server-lib-content-format
       result mcp-server-lib-test--string-list-result))))

(ert-deftest mcp-server-lib-test-tools-call-empty-string ()
  "Test the `tools/call` request with a tool that returns an empty string."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-empty-string
        :id "empty-string-tool"
        :description "A tool that returns an empty string"))
    (mcp-server-lib-test--verify-tool-schema-in-single-tool-list)

    (let ((result
           (mcp-server-lib-test--call-tool "empty-string-tool" 10)))
      (mcp-server-lib-test--check-mcp-server-lib-content-format
       result ""))))

(ert-deftest mcp-server-lib-test-tools-call-with-string-arg ()
  "Test the `tools/call` request with a tool that takes a string argument."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-string-arg
        :id "string-arg-tool"
        :description "A tool that echoes a string argument"))
    (let* ((test-input "Hello, world!")
           (args `(("input" . ,test-input))))

      (let ((result
             (mcp-server-lib-test--call-tool "string-arg-tool"
                                             13
                                             args)))
        (mcp-server-lib-test--check-mcp-server-lib-content-format
         result (concat "Echo: " test-input))))))

(ert-deftest mcp-server-lib-test-tools-call-unregistered-tool ()
  "Test the `tools/call` request with a tool that was never registered."
  (mcp-server-lib-test--with-server :tools nil :resources nil
    (mcp-server-lib-test--verify-tool-not-found
     mcp-server-lib-test--nonexistent-tool-id)))

(ert-deftest mcp-server-lib-test-tools-call-handler-returns-nil ()
  "Test tool handler that returns nil value."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--return-nil
        :id "nil-returning-tool"
        :description "A tool that returns nil"))
    (let* ((result
            (mcp-server-lib-test--call-tool "nil-returning-tool" 14))
           (response `((result . ,result)))
           (text
            (mcp-server-lib-ert-check-text-response response)))
      (should (string= "" text)))))

(ert-deftest mcp-server-lib-test-tools-call-handler-undefined ()
  "Test calling a tool whose handler function no longer exists."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--handler-to-be-undefined
        :id "undefined-handler-tool"
        :description "A tool whose handler will be undefined"))
    (mcp-server-lib-test--with-undefined-function 'mcp-server-lib-test--handler-to-be-undefined
      (mcp-server-lib-test--check-tool-call-error "undefined-handler-tool"
        ;; Try to call the tool - should return an error
        (mcp-server-lib-test--check-jsonrpc-error
         request
         mcp-server-lib-test--error-internal "Internal error executing tool: Symbol’s function definition is void: mcp-server-lib-test--handler-to-be-undefined")))))


;;; `mcp-server-lib-process-jsonrpc' tests

(ert-deftest mcp-server-lib-test-parse-error ()
  "Test that invalid JSON input returns a parse error."
  (mcp-server-lib-test--with-server :tools nil :resources nil
    (mcp-server-lib-test--check-jsonrpc-error
     "This is not valid JSON" mcp-server-lib-test--error-parse "Parse error: JSON readtable error: 84")))

(ert-deftest mcp-server-lib-test-method-not-found ()
  "Test that unknown methods return method-not-found error."
  (mcp-server-lib-test--with-server :tools nil :resources nil
    (mcp-server-lib-test--check-jsonrpc-error
     (json-encode
      '(("jsonrpc" . "2.0")
        ("method" . "unknown/method")
        ("id" . 99)))
     mcp-server-lib-test--error-method-not-found "Method not found: unknown/method")))

(ert-deftest mcp-server-lib-test-invalid-jsonrpc ()
  "Test that valid JSON that is not JSON-RPC returns an invalid request error."
  (mcp-server-lib-test--with-server :tools nil :resources nil
    (mcp-server-lib-test--check-jsonrpc-error
     (json-encode '(("name" . "Test Object") ("value" . 42)))
     mcp-server-lib-test--error-invalid-request
     "Invalid Request: Not JSON-RPC 2.0")))

(ert-deftest mcp-server-lib-test-invalid-jsonrpc-older-version ()
  "Test that JSON-RPC with older version (1.1) is rejected properly."
  (mcp-server-lib-test--check-invalid-jsonrpc-version "1.1"))

(ert-deftest mcp-server-lib-test-invalid-jsonrpc-non-standard-version
    ()
  "Test that JSON-RPC with non-standard version string is rejected properly."
  (mcp-server-lib-test--check-invalid-jsonrpc-version "non-standard"))

(ert-deftest mcp-server-lib-test-invalid-jsonrpc-missing-id ()
  "Test that JSON-RPC request lacking the `id` key is rejected properly."
  (mcp-server-lib-test--with-server :tools nil :resources nil
    (mcp-server-lib-test--check-jsonrpc-error
     (json-encode '(("jsonrpc" . "2.0") ("method" . "tools/list")))
     mcp-server-lib-test--error-invalid-request
     "Invalid Request: Missing required 'id' field")))

(ert-deftest mcp-server-lib-test-invalid-jsonrpc-missing-method ()
  "Test that JSON-RPC request lacking the `method` key is rejected properly."
  (mcp-server-lib-test--with-server :tools nil :resources nil
    (mcp-server-lib-test--check-jsonrpc-error
     (json-encode '(("jsonrpc" . "2.0") ("id" . 42)))
     mcp-server-lib-test--error-invalid-request
     "Invalid Request: Missing required 'method' field")))

;;; `mcp-server-lib-process-jsonrpc-parsed' tests

(ert-deftest mcp-server-lib-test-process-jsonrpc-parsed ()
  "Test that `mcp-server-lib-process-jsonrpc-parsed' returns parsed response."
  (mcp-server-lib-test--with-server :tools nil :resources nil
    (let* ((request (mcp-server-lib-create-tools-list-request))
           (response (mcp-server-lib-process-jsonrpc-parsed request)))
      ;; Response should be a parsed alist, not a string
      (should (listp response))
      (should (alist-get 'result response))
      (should
       (arrayp (alist-get 'tools (alist-get 'result response)))))))

;;; Logging tests

(ert-deftest mcp-server-lib-test-log-io-t ()
  "Test that when `mcp-server-lib-log-io' is t, JSON-RPC messages are logged."
  (setq mcp-server-lib-log-io t)

  (mcp-server-lib-test--with-server :tools nil :resources nil
    (let* ((request (mcp-server-lib-create-tools-list-request))
           (response (mcp-server-lib-process-jsonrpc request)))

      (let ((log-buffer (get-buffer "*mcp-server-lib-log*")))
        (should log-buffer)

        (with-current-buffer log-buffer
          (let ((content (buffer-string))
                (expected-suffix
                 (concat
                  "-> (request) ["
                  request
                  "]\n"
                  "<- (response) ["
                  response
                  "]\n")))
            (should (string-suffix-p expected-suffix content)))))))

  (setq mcp-server-lib-log-io nil))

(ert-deftest mcp-server-lib-test-log-io-nil ()
  "Test that when `mcp-server-lib-log-io' is nil, messages are not logged."
  (setq mcp-server-lib-log-io nil)

  (mcp-server-lib-test--with-server :tools nil :resources nil
    (let ((request (mcp-server-lib-create-tools-list-request)))
      (mcp-server-lib-process-jsonrpc request)
      (should-not (get-buffer "*mcp-server-lib-log*")))))

;;; Misc tests

(ert-deftest mcp-server-lib-test-server-restart-preserves-tools ()
  "Test that server restart preserves registered tools."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--return-string
        :id "persistent-tool"
        :description "Test persistence across restarts"))
    (mcp-server-lib-stop)
    (mcp-server-lib-start)

    (let ((tools (mcp-server-lib-test--get-tool-list)))
      (should (= 1 (length tools)))
      (should
       (string=
        "persistent-tool" (alist-get 'name (aref tools 0)))))))

(ert-deftest mcp-server-lib-test-interactive-commands ()
  "Verify that all package commands are interactive."
  (should (commandp #'mcp-server-lib-start))
  (should (commandp #'mcp-server-lib-stop))
  (should (commandp #'mcp-server-lib-install))
  (should (commandp #'mcp-server-lib-uninstall))
  (should (commandp #'mcp-server-lib-reset-metrics))
  (should (commandp #'mcp-server-lib-show-metrics)))

;;; `mcp-server-lib-with-error-handling' tests

(ert-deftest mcp-server-lib-test-with-error-handling-success ()
  "Test that `mcp-server-lib-with-error-handling' executes BODY normally."
  (let ((result (mcp-server-lib-with-error-handling (+ 1 2))))
    (should (= 3 result))))

(ert-deftest mcp-server-lib-test-with-error-handling-catches-error ()
  "Test that `mcp-server-lib-with-error-handling' catches errors."
  (should-error
   (mcp-server-lib-with-error-handling (error "Test error"))
   :type 'mcp-server-lib-tool-error))

(ert-deftest mcp-server-lib-test-with-error-handling-error-message ()
  "Test that `mcp-server-lib-with-error-handling' formats errors correctly."
  (condition-case err
      (mcp-server-lib-with-error-handling
       (error "Original error message"))
    (mcp-server-lib-tool-error
     (should
      (string-match
       "Error: (error \"Original error message\")" (cadr err))))))

(ert-deftest
    mcp-server-lib-test-with-error-handling-multiple-expressions
    ()
  "Test that `mcp-server-lib-with-error-handling' handles multiple forms."
  (let ((result
         (mcp-server-lib-with-error-handling
          (let ((test-var 42))
            (+ test-var 8)))))
    (should (= 50 result))))

;;; Script installation tests

(ert-deftest mcp-server-lib-test-install ()
  "Test script installation to temporary directory."
  (let* ((temp-dir (make-temp-file "mcp-test-" t))
         (mcp-server-lib-install-directory temp-dir))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            (mcp-server-lib-install))
          (should
           (file-exists-p (mcp-server-lib--installed-script-path)))
          (should
           (file-executable-p
            (mcp-server-lib--installed-script-path))))
      (delete-directory temp-dir t))))

(ert-deftest mcp-server-lib-test-install-overwrite ()
  "Test script installation when file already exists."
  (let* ((temp-dir (make-temp-file "mcp-test-" t))
         (mcp-server-lib-install-directory temp-dir)
         (target (mcp-server-lib--installed-script-path)))
    (unwind-protect
        (progn
          (write-region "existing content" nil target)
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            (mcp-server-lib-install))
          (should (file-exists-p target))
          (should (file-executable-p target))
          (should
           (> (file-attribute-size (file-attributes target)) 20)))
      (delete-directory temp-dir t))))

(ert-deftest mcp-server-lib-test-install-cancel ()
  "Test cancelling installation when file exists."
  (let* ((temp-dir (make-temp-file "mcp-test-" t))
         (mcp-server-lib-install-directory temp-dir)
         (target (mcp-server-lib--installed-script-path)))
    (unwind-protect
        (progn
          (write-region "existing content" nil target)
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
            (should-error (mcp-server-lib-install) :type 'user-error))
          (should
           (string=
            "existing content"
            (with-temp-buffer
              (insert-file-contents target)
              (buffer-string)))))
      (delete-directory temp-dir t))))

(ert-deftest mcp-server-lib-test-uninstall ()
  "Test script removal from temporary directory."
  (let* ((temp-dir (make-temp-file "mcp-test-" t))
         (mcp-server-lib-install-directory temp-dir)
         (target (mcp-server-lib--installed-script-path)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            (mcp-server-lib-install)
            (should (file-exists-p target))
            (mcp-server-lib-uninstall))
          (should-not (file-exists-p target)))
      (delete-directory temp-dir t))))

(ert-deftest mcp-server-lib-test-uninstall-missing ()
  "Test uninstalling when script doesn't exist."
  (let* ((temp-dir (make-temp-file "mcp-test-" t))
         (mcp-server-lib-install-directory temp-dir))
    (unwind-protect
        (should-error (mcp-server-lib-uninstall) :type 'user-error)
      (delete-directory temp-dir t))))

(ert-deftest mcp-server-lib-test-uninstall-cancel ()
  "Test cancelling uninstall."
  (let* ((temp-dir (make-temp-file "mcp-test-" t))
         (mcp-server-lib-install-directory temp-dir)
         (target (mcp-server-lib--installed-script-path)))
    (unwind-protect
        (progn
          (write-region "test content" nil target)
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
            (mcp-server-lib-uninstall))
          (should (file-exists-p target)))
      (delete-directory temp-dir t))))

;;; Metrics tests

(ert-deftest mcp-server-lib-test-metrics ()
  "Test metrics collection and reset."
  (mcp-server-lib-test--with-tools
      ( ;; Register a test tool
       (#'mcp-server-lib-test--return-string
        :id "metrics-test-tool"
        :description "Tool for testing metrics"))
    ;; Make some operations to generate metrics
    (mcp-server-lib-process-jsonrpc
     (mcp-server-lib-create-tools-list-request 100))
    (mcp-server-lib-test--call-tool "metrics-test-tool" 101)
    (mcp-server-lib-test--call-tool "metrics-test-tool" 102)

    ;; Verify non-zero before reset
    (let ((summary-before (mcp-server-lib-metrics-summary)))
      (should (stringp summary-before))
      (should-not
       (string-match "^MCP metrics: 0 calls" summary-before)))

    ;; Reset
    (mcp-server-lib-reset-metrics)

    ;; Verify zero after reset
    (let ((summary-after (mcp-server-lib-metrics-summary)))
      (should (stringp summary-after))
      (should (string-match "^MCP metrics: 0 calls" summary-after)))))

(ert-deftest mcp-server-lib-test-show-metrics ()
  "Test metrics display command."
  (mcp-server-lib-test--with-tools
   ((#'mcp-server-lib-test--return-string
     :id "display-test-tool"
     :description "Tool for testing display"))
   ;; Generate some metrics
   (mcp-server-lib-process-jsonrpc
    (mcp-server-lib-create-tools-list-request 200))
   (mcp-server-lib-test--call-tool "display-test-tool" 201)

   ;; Show metrics
   (mcp-server-lib-show-metrics)

   ;; Verify buffer exists and contains expected content
   (with-current-buffer "*MCP Metrics*"
     (let ((content (buffer-string)))
       (should (string-match "MCP Usage Metrics" content))
       (should (string-match "Method Calls:" content))
       ;; Should have at least 1 tools/list call from our test
       (should (string-match "tools/list\\s-+\\([0-9]+\\)" content))
       (let ((tools-list-count
              (string-to-number (match-string 1 content))))
         (should (>= tools-list-count 1)))

       (should (string-match "Tool Usage:" content))
       ;; Should have exactly 1 call to our test tool
       (should
        (string-match
         "display-test-tool\\s-+1\\s-+0\\s-+0\\.0%" content))
       (should (string-match "Summary:" content))
       (should (string-match "Methods: [0-9]+ calls" content))
       (should (string-match "Tools: [0-9]+ calls" content))))))

(ert-deftest mcp-server-lib-test-metrics-reset-on-start ()
  "Test that starting the server resets metrics."
  ;; First part: generate metrics and verify they exist
  (mcp-server-lib-test--with-server :tools nil :resources nil
                                    (mcp-server-lib-process-jsonrpc
                                     (mcp-server-lib-create-tools-list-request 100))
                                    
                                    ;; Verify metrics exist
                                    (let ((summary (mcp-server-lib-metrics-summary)))
                                      (should (stringp summary))
                                      ;; Should show at least 2 calls (initialize + tools/list)
                                      (should (string-match "[2-9][0-9]* calls\\|[0-9][0-9]+ calls" summary))))
  
  ;; Second part: start server again and verify metrics were reset
  (mcp-server-lib-test--with-server :tools nil :resources nil
                                    ;; After server restart, only the initialize call should be counted
                                    (let ((summary (mcp-server-lib-metrics-summary)))
                                      (should (string-match "^MCP metrics: [12] calls" summary)))))

(ert-deftest mcp-server-lib-test-metrics-on-stop ()
  "Test metrics display on server stop."
  ;; Capture messages throughout the entire test
  (cl-letf* ((messages nil)
             ((symbol-function 'message)
              (lambda (fmt &rest args)
                (push (apply #'format fmt args) messages))))
    (mcp-server-lib-test--with-tools
        ((#'mcp-server-lib-test--return-string
          :id "stop-test-tool"
          :description "Tool for testing stop"))
      ;; Generate some metrics
      (mcp-server-lib-test--call-tool "stop-test-tool" 300))

    ;; Check that metrics summary was displayed when server stopped
    (should
     (cl-some
      (lambda (msg)
        (string-match "MCP metrics:.*calls.*errors" msg))
      messages))))

;;; Resource tests

(ert-deftest test-mcp-server-lib-resources-list-empty ()
  "Test resources/list with no registered resources."
  (mcp-server-lib-test--with-server :tools nil :resources nil
    (mcp-server-lib-test--check-no-resources)))

(ert-deftest test-mcp-server-lib-register-resource ()
  "Test registering a direct resource."
  (mcp-server-lib-test--with-resources
      (("test://resource1"
        #'mcp-server-lib-test--return-string
        :name "Test Resource"
        :description "A test resource"
        :mime-type "text/plain"))
    ))

(ert-deftest test-mcp-server-lib-register-resource-minimal ()
  "Test registering a resource with only required fields."
  (mcp-server-lib-test--with-resources
   (("test://minimal"
     #'mcp-server-lib-test--return-string
     :name "Minimal Resource"))
   ;; Verify resource can be read without mime-type
   (mcp-server-lib-test--verify-resource-read
    "test://minimal"
    '((uri . "test://minimal")
      (text . "test result")))))

(ert-deftest test-mcp-server-lib-resources-read ()
  "Test reading a resource."
  (mcp-server-lib-test--with-resources
   (("test://resource1"
     #'mcp-server-lib-test--return-string
     :name "Test Resource"
     :mime-type "text/plain"))
   ;; Read the resource
   (mcp-server-lib-test--verify-resource-read
    "test://resource1"
    '((uri . "test://resource1")
      (mimeType . "text/plain")
      (text . "test result")))))

(ert-deftest test-mcp-server-lib-resources-read-handler-nil ()
  "Test that resource handler returning nil produces valid response with empty text."
  (mcp-server-lib-test--with-resources
   (("test://nil-resource"
     #'mcp-server-lib-test--return-nil
     :name "Nil Resource"))
   ;; Read the resource
   (mcp-server-lib-test--verify-resource-read
    "test://nil-resource"
    '((uri . "test://nil-resource")
      (text . nil)))))

(ert-deftest test-mcp-server-lib-resources-read-not-found ()
  "Test reading a non-existent resource returns error."
  (mcp-server-lib-test--with-server :tools nil :resources nil
   (mcp-server-lib-test--read-resource-error
    "test://nonexistent"
    mcp-server-lib-test--error-invalid-params
    "Resource not found: test://nonexistent")))

(ert-deftest test-mcp-server-lib-register-resource-duplicate ()
  "Test registering the same resource twice increments ref count."
  (mcp-server-lib-test--with-resources
   (("test://resource1"
     #'mcp-server-lib-test--return-string
     :name "Test Resource"))
   ;; The macro automatically verifies the resource is in the list
   ;; Now register the same resource again to test ref counting
   (mcp-server-lib-test--register-resource
    "test://resource1"
    #'mcp-server-lib-test--return-string
    :name "Test Resource"
    ;; Verify it's still listed only once
    (mcp-server-lib-test--check-single-resource
     '((uri . "test://resource1")
       (name . "Test Resource"))))
   
   ;; After inner macro completes, it unregisters once (ref count goes from 2 to 1)
   ;; Resource should still exist because outer registration is still active
   (mcp-server-lib-test--check-single-resource
    '((uri . "test://resource1")
      (name . "Test Resource"))))
  
  ;; After outer macro completes, it unregisters again (ref count = 0)
  ;; Resource should no longer be listed
  (mcp-server-lib-test--with-server :tools nil :resources nil
    (mcp-server-lib-test--check-no-resources)))

(ert-deftest test-mcp-server-lib-register-resource-error-missing-name ()
  "Test that resource registration with missing :name produces an error."
  (mcp-server-lib-test--with-server :tools nil :resources nil
    (should-error
     (mcp-server-lib-register-resource
      "test://resource"
      #'mcp-server-lib-test--return-string
      :description "Resource without name")
     :type 'error)))

(ert-deftest test-mcp-server-lib-register-resource-error-missing-handler ()
  "Test that resource registration with non-function handler produces an error."
  (mcp-server-lib-test--with-server :tools nil :resources nil
    (should-error
     (mcp-server-lib-register-resource
      "test://resource"
      "not-a-function"
      :name "Test Resource")
     :type 'error)))

(ert-deftest test-mcp-server-lib-register-resource-error-missing-uri ()
  "Test that resource registration with nil URI produces an error."
  (mcp-server-lib-test--with-server :tools nil :resources nil
    (should-error
     (mcp-server-lib-register-resource
      nil
      #'mcp-server-lib-test--return-string
      :name "Test Resource")
     :type 'error)))

(ert-deftest test-mcp-server-lib-unregister-resource-nonexistent ()
  "Test that `mcp-server-lib-unregister-resource` returns nil for missing resources."
  (mcp-server-lib-test--with-server :tools nil :resources nil
    (should-not (mcp-server-lib-unregister-resource "test://nonexistent"))))

(ert-deftest test-mcp-server-lib-resources-list-multiple ()
  "Test listing multiple registered resources."
  (mcp-server-lib-test--with-resources
      (("test://resource1"
        #'mcp-server-lib-test--return-string
        :name "First Resource"
        :description "The first test resource")
       ("test://resource2"
        #'mcp-server-lib-test--return-string
        :name "Second Resource"
        :mime-type "text/markdown"))
    ;; Verify both resources are listed
    (let ((resources (mcp-server-lib-test--get-resource-list)))
      (should (= 2 (length resources)))
      ;; Check each resource
      (let ((resource1 (mcp-server-lib-test--find-resource-by-uri "test://resource1" resources))
            (resource2 (mcp-server-lib-test--find-resource-by-uri "test://resource2" resources)))
        ;; Verify first resource
        (should resource1)
        (should (equal (alist-get 'name resource1) "First Resource"))
        (should (equal (alist-get 'description resource1) "The first test resource"))
        (should-not (alist-get 'mimeType resource1))
        ;; Verify second resource
        (should resource2)
        (should (equal (alist-get 'name resource2) "Second Resource"))
        (should-not (alist-get 'description resource2))
        (should (equal (alist-get 'mimeType resource2) "text/markdown"))))))

(ert-deftest test-mcp-server-lib-resources-read-handler-error ()
  "Test that resource handler errors return JSON-RPC error and increment error metrics."
  (mcp-server-lib-test--with-resources
   (("test://error-resource"
     #'mcp-server-lib-test--generic-error-handler
     :name "Error Resource"))
   (mcp-server-lib-test--with-metrics-tracking
    (("resources/read" 1 1))
    ;; Try to read the resource - should return an error
    (mcp-server-lib-test--read-resource-error
     "test://error-resource"
     mcp-server-lib-test--error-internal
     "Error reading resource test://error-resource: Generic error occurred"))))

(ert-deftest mcp-server-lib-test-resources-read-handler-undefined ()
  "Test reading a resource whose handler function no longer exists."
  (mcp-server-lib-test--with-resources
   (("test://undefined-handler"
     #'mcp-server-lib-test--handler-to-be-undefined
     :name "Undefined Handler Resource"))
   (mcp-server-lib-test--with-undefined-function 'mcp-server-lib-test--handler-to-be-undefined
     (mcp-server-lib-test--with-metrics-tracking
      (("resources/read" 1 1))
      ;; Try to read the resource - should return an error
      (mcp-server-lib-test--read-resource-error
       "test://undefined-handler"
       mcp-server-lib-test--error-internal
       "Error reading resource test://undefined-handler: Symbol’s function definition is void: mcp-server-lib-test--handler-to-be-undefined")))))

(provide 'mcp-server-lib-test)

;; Local Variables:
;; package-lint-main-file: "mcp-server-lib.el"
;; End:

;;; mcp-server-lib-test.el ends here
