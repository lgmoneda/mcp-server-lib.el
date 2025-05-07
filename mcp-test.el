;;; mcp-test.el --- Tests for mcp.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
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

;; ERT tests for mcp.el.

;;; Code:

(require 'ert)
(require 'mcp)
(require 'json)

;;; Test data

(defconst mcp-test--string-list-result "item1 item2 item3"
  "Test data for string list tool.")

;;; Test tool handlers

(defun mcp-test--tool-handler-simple ()
  "Test tool handler function for MCP tool testing."
  "test result")

(defun mcp-test--tool-handler-mcp-tool-throw ()
  "Test tool handler that always fails with `mcp-tool-throw'."
  (mcp-tool-throw "This tool intentionally fails"))

(defun mcp-test--tool-handler-error ()
  "Test tool handler that throws a generic error."
  (error "Generic error occurred"))

(defun mcp-test--tool-handler-string-list ()
  "Test tool handler function to return a string with items."
  mcp-test--string-list-result)

(defun mcp-test--tool-handler-empty-string ()
  "Test tool handler function to return an empty string."
  "")

(defun mcp-test--tool-handler-string-arg (input-string)
  "Test tool handler that accepts a string argument.
INPUT-STRING is the string argument passed to the tool.

MCP Parameters:
  input-string - test parameter for string input"
  (concat "Echo: " input-string))

(defun mcp-test--tool-handler-duplicate-param (input-string)
  "Test handler with duplicate parameter.
INPUT-STRING is the string argument.

MCP Parameters:
  input-string - first description
  input-string - second description"
  (concat "Test: " input-string))

(defun mcp-test--tool-handler-mismatched-param (input-string)
  "Test handler with mismatched parameter name.
INPUT-STRING is the string argument.

MCP Parameters:
  wrong-param-name - description for non-existent parameter"
  (concat "Test: " input-string))

(defun mcp-test--tool-handler-missing-param (input-string)
  "Test handler with missing parameter documentation.
INPUT-STRING is the string argument.

MCP Parameters:"
  (concat "Test: " input-string))

;;; Test helpers

(defmacro mcp-test--with-server (&rest body)
  "Run BODY with MCP server active.
Calls `mcp-start' before BODY and `mcp-stop' after BODY."
  (declare (indent defun) (debug t))
  `(progn
     (mcp-start)
     (unwind-protect
         (progn
           ,@body)
       (mcp-stop))))

(defmacro mcp-test--with-tools (tools &rest body)
  "Run BODY with MCP server active and TOOLS registered.
All tools are automatically unregistered after BODY execution.

Arguments:
  TOOLS  List of tool registration specs, each a list of arguments for
         `mcp-register-tool': (HANDLER &rest PROPERTIES)
  BODY   Forms to execute with server running and tools registered

Example:
  (mcp-test--with-tools
   ((#\\='mcp-test--tool-handler-simple
     :id \"test-tool-1\"
     :description \"First tool\")
    (#\\='mcp-test--tool-handler-simple
     :id \"test-tool-2\"
     :description \"Second tool\"))
   (let ((response (mcp-process-jsonrpc (mcp-create-tools-list-request 7))))
     ...))"
  (declare (indent 1) (debug t))
  (let ((tool-registrations '())
        (tool-ids '()))
    ;; Extract tool IDs and build registration forms
    (dolist (tool-spec tools)
      (let* ((handler (car tool-spec))
             (props (cdr tool-spec))
             (id-prop (plist-get props :id)))
        (push id-prop tool-ids)
        (push
         `(mcp-register-tool ,handler ,@props) tool-registrations)))
    ;; Build the macro expansion
    `(progn
       ;; Register all tools first
       ,@
       (nreverse tool-registrations)
       ;; Run with server active
       (mcp-test--with-server
         (unwind-protect
             (progn
               ,@body)
           ;; Unregister all tools
           ,@
           (mapcar
            (lambda (id) `(mcp-unregister-tool ,id))
            (nreverse tool-ids)))))))

(defun mcp-test--initialize-request (id)
  "Create an initialize JSON-RPC request with ID."
  (json-encode
   `(("jsonrpc" . "2.0")
     ("method" . "initialize") ("id" . ,id)
     ("params" .
      (("protocolVersion" . "2025-03-26")
       ("capabilities" .
        (("tools" . t) ("resources" . nil) ("prompts" . nil))))))))

(defun mcp-test--prompts-list-request (id)
  "Create a prompt list JSON-RPC request with ID."
  (json-encode
   `(("jsonrpc" . "2.0") ("method" . "prompts/list") ("id" . ,id))))

(defun mcp-test--verify-jsonrpc-error
    (request-object expected-code expected-message)
  "Test that JSON-RPC REQUEST-OBJECT is rejected with expected error.
EXPECTED-CODE is the expected error code.
EXPECTED-MESSAGE is a regex pattern to match against the error message."
  (mcp-test--with-server
    (let* ((request (json-encode request-object))
           (response (mcp-process-jsonrpc request))
           (response-obj (json-read-from-string response))
           (error-obj (alist-get 'error response-obj)))
      (should error-obj)
      (should (alist-get 'code error-obj))
      (should (= (alist-get 'code error-obj) expected-code))
      (should (alist-get 'message error-obj))
      (should
       (string-match
        expected-message (alist-get 'message error-obj))))))

(defun mcp-test--test-invalid-jsonrpc-version (version)
  "Test that JSON-RPC request with VERSION is rejected properly."
  (mcp-test--verify-jsonrpc-error
   `(("jsonrpc" . ,version) ("method" . "tools/list") ("id" . 42))
   -32600
   "Invalid Request: Not JSON-RPC 2.0"))

(defun mcp-test--verify-tool-list-response (response expected-tools)
  "Verify RESPONSE from tools/list against EXPECTED-TOOLS.
EXPECTED-TOOLS should be an alist of (tool-name . tool-properties)."
  (let* ((resp-obj (json-read-from-string response))
         (result (alist-get 'result resp-obj))
         (tools (alist-get 'tools result)))
    (should (arrayp tools))
    (should (= (length expected-tools) (length tools)))
    ;; Check each expected tool
    (dolist (expected expected-tools)
      (let ((expected-name (car expected))
            (expected-props (cdr expected))
            (found-tool nil))
        ;; Find the tool by name
        (dotimes (i (length tools))
          (let ((tool (aref tools i)))
            (when (string= expected-name (alist-get 'name tool))
              (setq found-tool tool))))
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

(defun mcp-test--check-mcp-content-format (result expected-text)
  "Check that RESULT follows the MCP content format with EXPECTED-TEXT.
Verifies that result has a content array with a proper text item."
  ;; Check for proper MCP format
  (should (alist-get 'content result))
  (should (arrayp (alist-get 'content result)))
  (should (= 1 (length (alist-get 'content result))))
  ;; Check first content item
  (let ((content-item (aref (alist-get 'content result) 0)))
    (should (alist-get 'type content-item))
    (should (string= "text" (alist-get 'type content-item)))
    (should (alist-get 'text content-item))
    ;; Verify the text field contains expected text
    (should (string= expected-text (alist-get 'text content-item))))
  ;; Check isError field
  (should (not (null (alist-get 'isError result nil t))))
  (should (eq :json-false (alist-get 'isError result))))

;;; Initialization and server capabilities tests

(ert-deftest mcp-test-initialize ()
  "Test the MCP initialize request handling."
  (mcp-test--with-server
    ;; Test initialize with valid parameters
    (let* ((req (mcp-test--initialize-request 3))
           (resp (mcp-process-jsonrpc req))
           (result (alist-get 'result (json-read-from-string resp))))
      ;; Verify the server responded with its protocol version
      (should (stringp (alist-get 'protocolVersion result)))
      (should
       (string= "2025-03-26" (alist-get 'protocolVersion result)))
      ;; Verify server capabilities
      (should (alist-get 'capabilities result))

      ;; Verify capability objects are present and properly formatted
      ;; (empty objects deserialize to nil)
      (let ((capabilities (alist-get 'capabilities result)))
        (should (equal nil (alist-get 'tools capabilities)))
        (should (equal nil (alist-get 'resources capabilities)))
        (should (equal nil (alist-get 'prompts capabilities))))
      ;; Verify server info
      (should (alist-get 'serverInfo result))
      (let ((server-name
             (alist-get 'name (alist-get 'serverInfo result))))
        (should (string= mcp--name server-name))))))

(ert-deftest mcp-test-tool-registration-in-capabilities ()
  "Test that registered tool appears in server capabilities."
  (mcp-test--with-tools ((#'mcp-test--tool-handler-simple
                          :id "test-tool"
                          :description "A tool for testing"))
    (let* ((req (mcp-test--initialize-request 1))
           (resp (mcp-process-jsonrpc req))
           (resp-obj (json-read-from-string resp)))

      (should (alist-get 'result resp-obj))

      (let* ((result (alist-get 'result resp-obj))
             (capabilities (alist-get 'capabilities result))
             (tools-capability (alist-get 'tools capabilities)))

        (should tools-capability)
        (should (alist-get 'listChanged tools-capability))
        (should (eq t (alist-get 'listChanged tools-capability)))))))

(ert-deftest mcp-test-notifications-initialized-format ()
  "Test the MCP notifications/initialized format handling."
  (mcp-test--with-server
    (let* ((notifications-initialized
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "notifications/initialized"))))
           (response (mcp-process-jsonrpc notifications-initialized)))
      ;; Notifications are one-way, should return nil
      (should (null response)))))

;;; `mcp-register-tool' tests

(ert-deftest mcp-test-missing-id-error ()
  "Test that tool registration with missing :id produces an error."
  (should-error
   (mcp-register-tool
    #'mcp-test--tool-handler-simple
    :description "Test tool without ID")
   :type 'error))

(ert-deftest mcp-test-missing-description-error ()
  "Test that tool registration with missing :description produces an error."
  (should-error
   (mcp-register-tool
    #'mcp-test--tool-handler-simple
    :id "test-tool-no-desc")
   :type 'error))

(ert-deftest mcp-test-missing-handler-error ()
  "Test that tool registration with non-function handler produces an error."
  (should-error
   (mcp-register-tool
    "not-a-function"
    :id "test-tool-bad-handler"
    :description "Test tool with invalid handler")
   :type 'error))

(ert-deftest mcp-test-duplicate-param-description-error ()
  "Test that duplicate parameter descriptions cause an error."
  (should-error
   (mcp-register-tool
    #'mcp-test--tool-handler-duplicate-param
    :id "duplicate-param-tool"
    :description "Tool with duplicate parameter")
   :type 'error))

(ert-deftest mcp-test-mismatched-param-error ()
  "Test that parameter names must match function arguments."
  (should-error
   (mcp-register-tool
    #'mcp-test--tool-handler-mismatched-param
    :id "mismatched-param-tool"
    :description "Tool with mismatched parameter")
   :type 'error))

(ert-deftest mcp-test-missing-param-error ()
  "Test that all function parameters must be documented."
  (should-error
   (mcp-register-tool
    #'mcp-test--tool-handler-missing-param
    :id "missing-param-tool"
    :description "Tool with missing parameter docs")
   :type 'error))

;;; `mcp-unregister-tool' tests

(ert-deftest mcp-test-unregister-tool ()
  "Test that `mcp-unregister-tool' removes a tool correctly."
  (let ((tools-before (hash-table-count mcp--tools)))
    (mcp-register-tool
     #'mcp-test--tool-handler-simple
     :id "test-unregister"
     :description "Tool for unregister test")
    (should (= (1+ tools-before) (hash-table-count mcp--tools)))
    (should (gethash "test-unregister" mcp--tools))
    (should (mcp-unregister-tool "test-unregister"))
    (should-not (gethash "test-unregister" mcp--tools))
    (should (= tools-before (hash-table-count mcp--tools)))))

(ert-deftest mcp-test-unregister-nonexistent-tool ()
  "Test that `mcp-unregister-tool' returns nil for nonexistent tools."
  (mcp-register-tool
   #'mcp-test--tool-handler-simple
   :id "test-other"
   :description "Other test tool")
  (should-not (mcp-unregister-tool "nonexistent-tool"))
  (mcp-unregister-tool "test-other"))

(ert-deftest mcp-test-unregister-when-no-tools ()
  "Test that `mcp-unregister-tool' works when no tools are registered."
  (should-not (mcp-unregister-tool "any-tool")))

;;; Notification tests

(ert-deftest mcp-test-notifications-cancelled-format ()
  "Test the MCP notifications/cancelled format handling."
  (mcp-test--with-server
    (let* ((notifications-cancelled
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "notifications/cancelled"))))
           (response (mcp-process-jsonrpc notifications-cancelled)))
      ;; Notifications are one-way, should return nil
      (should (null response)))))

;;; `mcp-create-tools-list-request' tests

(ert-deftest mcp-test-create-tools-list-request-with-id ()
  "Test `mcp-create-tools-list-request' with a specified ID."
  (let* ((id 42)
         (request (mcp-create-tools-list-request id))
         (parsed (json-read-from-string request)))
    ;; Verify basic JSON-RPC structure
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "tools/list" (alist-get 'method parsed)))
    (should (equal id (alist-get 'id parsed)))))

(ert-deftest mcp-test-create-tools-list-request-default-id ()
  "Test `mcp-create-tools-list-request' with default ID."
  (let* ((request (mcp-create-tools-list-request))
         (parsed (json-read-from-string request)))
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "tools/list" (alist-get 'method parsed)))
    (should (equal 1 (alist-get 'id parsed)))))

;;; tools/list tests

(ert-deftest mcp-test-tools-list-one ()
  "Test tools/list returns one tool with correct fields and schema."
  (mcp-test--with-tools ((#'mcp-test--tool-handler-simple
                          :id "test-tool"
                          :description "A tool for testing"))
    (let ((resp
           (mcp-process-jsonrpc (mcp-create-tools-list-request 6))))
      (mcp-test--verify-tool-list-response
       resp
       '(("test-tool" .
          ((description . "A tool for testing")
           (inputSchema . ((type . "object"))))))))))

(ert-deftest mcp-test-tools-list-with-title ()
  "Test that tools/list includes title in response."
  (mcp-test--with-tools ((#'mcp-test--tool-handler-simple
                          :id "tool-with-title"
                          :description "A tool for testing titles"
                          :title "Friendly Tool Name"))
    (let ((response
           (mcp-process-jsonrpc (mcp-create-tools-list-request 14))))
      (mcp-test--verify-tool-list-response
       response
       '(("tool-with-title" .
          ((description . "A tool for testing titles")
           (annotations . ((title . "Friendly Tool Name")))
           (inputSchema . ((type . "object"))))))))))

(ert-deftest mcp-test-tools-list-two ()
  "Test the `tools/list` method returns multiple tools with correct fields."
  (mcp-test--with-tools ((#'mcp-test--tool-handler-simple
                          :id "test-tool-1"
                          :description "First tool for testing")
                         (#'mcp-test--tool-handler-simple
                          :id "test-tool-2"
                          :description "Second tool for testing"))
    (let ((response
           (mcp-process-jsonrpc (mcp-create-tools-list-request 7))))
      (mcp-test--verify-tool-list-response
       response
       '(("test-tool-1" .
          ((description . "First tool for testing")
           (inputSchema . ((type . "object")))))
         ("test-tool-2" .
          ((description . "Second tool for testing")
           (inputSchema . ((type . "object"))))))))))

(ert-deftest mcp-test-tools-list-zero ()
  "Test the `tools/list` method returns empty array with no tools."
  (mcp-test--with-server
    (let ((resp
           (mcp-process-jsonrpc (mcp-create-tools-list-request 5))))
      (mcp-test--verify-tool-list-response resp '()))))

(ert-deftest mcp-test-schema-for-one-arg-handler ()
  "Test schema includes parameter descriptions."
  (mcp-test--with-tools
      ((#'mcp-test--tool-handler-string-arg
        :id "requires-arg"
        :description "A tool that requires an argument"))
    ;; Get schema via tools/list
    (let* ((list-req (mcp-create-tools-list-request 42))
           (list-response (mcp-process-jsonrpc list-req))
           (list-obj (json-read-from-string list-response))
           (tool-list (alist-get 'tools (alist-get 'result list-obj)))
           (tool (aref tool-list 0))
           (schema (alist-get 'inputSchema tool)))

      ;; Verify schema base structure
      (should (equal "object" (alist-get 'type schema)))
      (should (alist-get 'properties schema))
      (should (equal ["input-string"] (alist-get 'required schema)))

      ;; Verify parameter includes description
      (let ((param-schema
             (alist-get
              'input-string (alist-get 'properties schema))))
        (should param-schema)
        (should (equal "string" (alist-get 'type param-schema)))
        (should
         (equal
          "test parameter for string input"
          (alist-get 'description param-schema)))))))

(ert-deftest mcp-test-tools-list-with-extra-key ()
  "Test that tools/list request with an extra, unexpected key works correctly.
Per JSON-RPC 2.0 spec, servers should ignore extra/unknown members."
  (mcp-test--with-server
    ;; Create a tools/list request with an extra key
    (let* ((request-with-extra
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "tools/list")
               ("id" . 43)
               ("extra_key" . "unexpected value"))))
           (response (mcp-process-jsonrpc request-with-extra))
           (response-obj (json-read-from-string response))
           (result (alist-get 'result response-obj)))
      ;; Should be processed normally, not rejected as error
      (should (null (alist-get 'error response-obj)))
      (should result)
      (should (alist-get 'tools result))
      (should (arrayp (alist-get 'tools result))))))

(ert-deftest mcp-test-schema-for-bytecode-handler ()
  "Test schema generation for a handler loaded as bytecode.
This test verifies that MCP can correctly extract parameter information
from a function loaded from bytecode rather than interpreted elisp."
  (let* ((source-file
          (expand-file-name "mcp-test-bytecode-handler.el"))
         (bytecode-file
          (expand-file-name "mcp-test-bytecode-handler.elc")))
    (should (file-exists-p source-file))
    (byte-compile-file source-file)

    (let ((load-prefer-newer nil))
      (load bytecode-file nil t))

    ;; Suppress byte-compiler warning about unknown function
    (with-no-warnings
      (mcp-test--with-tools
          ((#'mcp-test-bytecode-handler--handler
            :id "bytecode-handler"
            :description "A tool with a handler loaded from bytecode"))
        (let* ((list-req (mcp-create-tools-list-request 123))
               (list-response (mcp-process-jsonrpc list-req))
               (list-obj (json-read-from-string list-response))
               (tool-list
                (alist-get 'tools (alist-get 'result list-obj)))
               (tool (aref tool-list 0))
               (schema (alist-get 'inputSchema tool)))

          (should (equal "object" (alist-get 'type schema)))
          (should (alist-get 'properties schema))
          (should
           (equal ["input-string"] (alist-get 'required schema)))

          (let ((param-schema
                 (alist-get
                  'input-string (alist-get 'properties schema))))
            (should param-schema)
            (should (equal "string" (alist-get 'type param-schema)))
            (should
             (equal
              "Input string parameter for bytecode testing"
              (alist-get 'description param-schema)))))))

    (when (file-exists-p bytecode-file)
      (delete-file bytecode-file))))

(ert-deftest mcp-test-tools-list-with-read-only-hint ()
  "Test that tools/list includes readOnlyHint=true in response."
  (mcp-test--with-tools
      ((#'mcp-test--tool-handler-simple
        :id "read-only-tool"
        :description "A tool that doesn't modify its environment"
        :read-only t))
    (let ((response
           (mcp-process-jsonrpc (mcp-create-tools-list-request 15))))
      (mcp-test--verify-tool-list-response
       response
       '(("read-only-tool" .
          ((description
            . "A tool that doesn't modify its environment")
           (annotations . ((readOnlyHint . t)))
           (inputSchema . ((type . "object"))))))))))

(ert-deftest mcp-test-tools-list-with-read-only-hint-false ()
  "Test that tools/list includes readOnlyHint=false in response."
  (mcp-test--with-tools
      ((#'mcp-test--tool-handler-simple
        :id "non-read-only-tool"
        :description "Tool that modifies its environment"
        :read-only nil))
    (let ((response
           (mcp-process-jsonrpc (mcp-create-tools-list-request 16))))
      (mcp-test--verify-tool-list-response
       response
       '(("non-read-only-tool" .
          ((description . "Tool that modifies its environment")
           (annotations . ((readOnlyHint . :json-false)))
           (inputSchema . ((type . "object"))))))))))

(ert-deftest mcp-test-tools-list-with-multiple-annotations ()
  "Test that tools/list handles multiple annotations in response."
  (mcp-test--with-tools
      ((#'mcp-test--tool-handler-simple
        :id "multi-annotated-tool"
        :description "A tool with multiple annotations"
        :title "Friendly Multi-Tool"
        :read-only t))
    (let ((response
           (mcp-process-jsonrpc (mcp-create-tools-list-request 17))))
      (mcp-test--verify-tool-list-response
       response
       '(("multi-annotated-tool" .
          ((description . "A tool with multiple annotations")
           (annotations
            . ((title . "Friendly Multi-Tool") (readOnlyHint . t)))
           (inputSchema . ((type . "object"))))))))))

;;; `mcp-create-tools-call-request' tests

(ert-deftest mcp-test-create-tools-call-request-with-id-and-args ()
  "Test `mcp-create-tools-call-request' with specified ID and arguments."
  (let* ((tool-name "test-tool")
         (id 42)
         (args '(("arg1" . "value1") ("arg2" . "value2")))
         (request (mcp-create-tools-call-request tool-name id args))
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

(ert-deftest mcp-test-create-tools-call-request-default-id ()
  "Test `mcp-create-tools-call-request' with default ID."
  (let* ((tool-name "test-tool")
         (request (mcp-create-tools-call-request tool-name))
         (parsed (json-read-from-string request))
         (params (alist-get 'params parsed)))
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "tools/call" (alist-get 'method parsed)))
    (should (equal 1 (alist-get 'id parsed)))
    (should params)
    (should (equal tool-name (alist-get 'name params)))
    (should (equal '() (alist-get 'arguments params)))))

(ert-deftest mcp-test-create-tools-call-request-with-empty-args ()
  "Test `mcp-create-tools-call-request' with empty arguments list."
  (let* ((tool-name "test-tool")
         (id 43)
         (request (mcp-create-tools-call-request tool-name id '()))
         (parsed (json-read-from-string request))
         (params (alist-get 'params parsed)))
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "tools/call" (alist-get 'method parsed)))
    (should (equal id (alist-get 'id parsed)))
    (should params)
    (should (equal tool-name (alist-get 'name params)))
    (should (equal '() (alist-get 'arguments params)))))

;;; tools/call tests

(ert-deftest mcp-test-mcp-tool-throw ()
  "Test `mcp-tool-throw'."
  (mcp-test--with-tools ((#'mcp-test--tool-handler-mcp-tool-throw
                          :id "failing-tool"
                          :description "A tool that always fails"))
    (let* ((response
            (mcp-process-jsonrpc
             (mcp-create-tools-call-request "failing-tool" 11)))
           (response-obj (json-read-from-string response))
           (result (alist-get 'result response-obj)))
      ;; Check for proper MCP format
      (should result)
      (should (alist-get 'content result))
      (should (arrayp (alist-get 'content result)))
      (should (= 1 (length (alist-get 'content result))))
      ;; Check content item
      (let ((content-item (aref (alist-get 'content result) 0)))
        (should (alist-get 'type content-item))
        (should (string= "text" (alist-get 'type content-item)))
        (should (alist-get 'text content-item))
        (should
         (string=
          "This tool intentionally fails"
          (alist-get 'text content-item))))
      ;; Check isError field is true
      (should (alist-get 'isError result))
      (should (eq t (alist-get 'isError result))))))

(ert-deftest mcp-test-tools-call-generic-error ()
  "Test that generic errors use standard JSON-RPC error format."
  (mcp-test--with-tools
      ((#'mcp-test--tool-handler-error
        :id "generic-error-tool"
        :description "A tool that throws a generic error"))
    (let* ((response
            (mcp-process-jsonrpc
             (mcp-create-tools-call-request "generic-error-tool" 12)))
           (response-obj (json-read-from-string response))
           ;; Should have error field at the top level of the response
           (error-obj (alist-get 'error response-obj)))
      ;; Check it has a standard JSON-RPC error
      (should error-obj)
      (should (alist-get 'code error-obj))
      (should (= -32603 (alist-get 'code error-obj)))
      (should (alist-get 'message error-obj))
      (should
       (string-match
        "Internal error executing tool"
        (alist-get 'message error-obj))))))

(ert-deftest mcp-test-tools-call-no-args ()
  "Test the `tools/call` method with a tool that takes no arguments."
  (mcp-test--with-tools
      ((#'mcp-test--tool-handler-string-list
        :id "string-list-tool"
        :description "A tool that returns a string with items"))
    (let* ((response
            (mcp-process-jsonrpc
             (mcp-create-tools-call-request "string-list-tool" 9)))
           (response-obj (json-read-from-string response))
           (result (alist-get 'result response-obj)))
      (should result)
      (mcp-test--check-mcp-content-format
       result mcp-test--string-list-result))))

(ert-deftest mcp-test-tools-call-empty-string ()
  "Test the `tools/call` method with a tool that returns an empty string."
  ;; Clear any previously registered tools to ensure only this tool exists
  (clrhash mcp--tools)

  (mcp-test--with-tools
      ((#'mcp-test--tool-handler-empty-string
        :id "empty-string-tool"
        :description "A tool that returns an empty string"))
    ;; First check the schema for this zero-arg handler
    (let* ((list-req (mcp-create-tools-list-request 100))
           (list-response (mcp-process-jsonrpc list-req))
           (list-obj (json-read-from-string list-response))
           (tools (alist-get 'tools (alist-get 'result list-obj)))
           (tool (aref tools 0))
           (schema (alist-get 'inputSchema tool)))

      ;; Verify schema is correct for a zero-arg function
      (should (equal '((type . "object")) schema)))

    ;; Then test the actual tool execution
    (let* ((req
            (mcp-create-tools-call-request "empty-string-tool" 10))
           (response (mcp-process-jsonrpc req))
           (response-obj (json-read-from-string response))
           (result (alist-get 'result response-obj)))
      (should result)
      (mcp-test--check-mcp-content-format result ""))))

(ert-deftest mcp-test-tools-call-with-string-arg ()
  "Test the `tools/call` method with a tool that takes a string argument."
  (mcp-test--with-tools
      ((#'mcp-test--tool-handler-string-arg
        :id "string-arg-tool"
        :description "A tool that echoes a string argument"))
    (let* ((test-input "Hello, world!")
           (args `(("input" . ,test-input)))
           (req
            (mcp-create-tools-call-request "string-arg-tool" 13 args))
           (response (mcp-process-jsonrpc req))
           (response-obj (json-read-from-string response))
           (result (alist-get 'result response-obj)))
      (should result)
      (mcp-test--check-mcp-content-format
       result (concat "Echo: " test-input)))))

;;; prompts/list tests

(ert-deftest mcp-test-prompts-list-zero ()
  "Test the `prompts/list` method returns empty array with no prompts."
  (mcp-test--with-server
    (let* ((response
            (mcp-process-jsonrpc (mcp-test--prompts-list-request 8)))
           (response-obj (json-read-from-string response))
           (result (alist-get 'result response-obj)))
      (should result)
      (should (alist-get 'prompts result))
      (should (arrayp (alist-get 'prompts result)))
      (should (= 0 (length (alist-get 'prompts result)))))))

;;; `mcp-process-jsonrpc' tests

(ert-deftest mcp-test-parse-error ()
  "Test that invalid JSON input to `mcp-process-jsonrpc' returns a parse error."
  (mcp-test--with-server
    ;; Send an invalid JSON string
    (let* ((invalid-json "This is not valid JSON")
           (response (mcp-process-jsonrpc invalid-json))
           (response-obj (json-read-from-string response))
           (error-obj (alist-get 'error response-obj)))
      ;; Verify it's a proper error response
      (should error-obj)
      (should (alist-get 'code error-obj))
      ;; Check it has the correct error code for parse error (-32700)
      (should (= (alist-get 'code error-obj) -32700))
      ;; Check it has an error message
      (should (alist-get 'message error-obj))
      (should
       (string-match "Parse error" (alist-get 'message error-obj))))))

(ert-deftest mcp-test-invalid-jsonrpc ()
  "Test that valid JSON that is not JSON-RPC returns an invalid request error."
  (mcp-test--verify-jsonrpc-error
   '(("name" . "Test Object") ("value" . 42))
   -32600
   "Invalid Request: Not JSON-RPC 2.0"))

(ert-deftest mcp-test-invalid-jsonrpc-older-version ()
  "Test that JSON-RPC with older version (1.1) is rejected properly."
  (mcp-test--test-invalid-jsonrpc-version "1.1"))

(ert-deftest mcp-test-invalid-jsonrpc-non-standard-version ()
  "Test that JSON-RPC with non-standard version string is rejected properly."
  (mcp-test--test-invalid-jsonrpc-version "non-standard"))

(ert-deftest mcp-test-invalid-jsonrpc-missing-id ()
  "Test that JSON-RPC request lacking the \"id\" key is rejected properly."
  (mcp-test--verify-jsonrpc-error
   '(("jsonrpc" . "2.0") ("method" . "tools/list"))
   -32600
   "Invalid Request: Missing required 'id' field"))

(ert-deftest mcp-test-invalid-jsonrpc-missing-method ()
  "Test that JSON-RPC request lacking the \"method\" key is rejected properly."
  (mcp-test--verify-jsonrpc-error
   '(("jsonrpc" . "2.0") ("id" . 42))
   -32600
   "Invalid Request: Missing required 'method' field"))

;;; Logging tests

(ert-deftest mcp-test-log-io-t ()
  "Test that when `mcp-log-io' is t, JSON-RPC messages are logged."
  ;; Set logging to enabled
  (setq mcp-log-io t)

  (mcp-test--with-server
    ;; Make a tools/list request with a known ID
    (let* ((request (mcp-create-tools-list-request 100))
           (response (mcp-process-jsonrpc request)))

      ;; Check that log buffer exists
      (let ((log-buffer (get-buffer "*mcp-log*")))
        (should log-buffer)

        ;; Check buffer contents exactly - compare with expected content
        (with-current-buffer log-buffer
          (let ((content (buffer-string))
                (expected-content
                 (concat
                  "-> (request) ["
                  request
                  "]\n"
                  "<- (response) ["
                  response
                  "]\n")))
            (should (equal expected-content content)))))))

  ;; Clean up
  (setq mcp-log-io nil))

(ert-deftest mcp-test-log-io-nil ()
  "Test that when `mcp-log-io' is nil, JSON-RPC messages are not logged."
  (setq mcp-log-io nil)

  (mcp-test--with-server
    (let ((request (mcp-create-tools-list-request 101)))
      (mcp-process-jsonrpc request)
      (should-not (get-buffer "*mcp-log*")))))

;;; Misc tests

(ert-deftest mcp-test-server-restart-preserves-tools ()
  "Test that server restart preserves registered tools."
  ;; Register a tool
  (mcp-register-tool
   #'mcp-test--tool-handler-simple
   :id "persistent-tool"
   :description "Test persistence across restarts")

  ;; Start server, stop it, and then start again
  (mcp-start)
  (mcp-stop)
  (mcp-test--with-server
    ;; Tool should be accessible via API
    (let* ((list-request (mcp-create-tools-list-request 1000))
           (list-response (mcp-process-jsonrpc list-request))
           (list-obj (json-read-from-string list-response))
           (tools (alist-get 'tools (alist-get 'result list-obj))))
      (should (= 1 (length tools)))
      (should
       (string= "persistent-tool" (alist-get 'name (aref tools 0))))))

  ;; Cleanup
  (mcp-unregister-tool "persistent-tool"))

(ert-deftest mcp-test-interactive-commands ()
  "Test that `mcp-start' and `mcp-stop' are interactive commands."
  (should (commandp #'mcp-start))
  (should (commandp #'mcp-stop)))

(provide 'mcp-test)
;;; mcp-test.el ends here
