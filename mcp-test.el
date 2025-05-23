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

(defconst mcp-test--nonexistent-tool-id "non-existent-tool"
  "Tool ID for a non-existent tool used in tests.")

(defconst mcp-test--unregister-tool-id "test-unregister"
  "Tool ID used for testing tool unregistration.")

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

;; Bytecode handler function that will be loaded during tests
(declare-function mcp-test-bytecode-handler--handler
                  "mcp-test-bytecode-handler")

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
   (let ((response (mcp-process-jsonrpc (mcp-create-tools-list-request))))
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

(defun mcp-test--prompts-list-request ()
  "Create an MCP prompt list request."
  (json-encode
   `(("jsonrpc" . "2.0") ("method" . "prompts/list") ("id" . 8))))

(defun mcp-test--get-request-result (request)
  "Call `mcp-process-jsonrpc' with REQUEST and return its successful result."
  (let ((resp-obj
         (json-read-from-string (mcp-process-jsonrpc request))))
    (should (null (alist-get 'error resp-obj)))
    (alist-get 'result resp-obj)))

(defun mcp-test--get-initialize-result ()
  "Send an MCP `initialize` request and return its result."
  (mcp-test--get-request-result
   (json-encode
    `(("jsonrpc" . "2.0")
      ("method" . "initialize") ("id" . 15)
      ("params" .
       (("protocolVersion" . "2025-03-26")
        ("capabilities"
         .
         (("tools" . t) ("resources" . nil) ("prompts" . nil)))))))))

(defun mcp-test--check-jsonrpc-error
    (request expected-code expected-message)
  "Test that JSON-RPC REQUEST is rejected with expected error.
REQUEST is a string containing the JSON-RPC request.
EXPECTED-CODE is the expected error code.
EXPECTED-MESSAGE is a regex pattern to match against the error message."
  (let* ((resp (mcp-process-jsonrpc request))
         (resp-obj (json-read-from-string resp))
         (err-obj (alist-get 'error resp-obj))
         (err-code (alist-get 'code err-obj))
         (err-msg (alist-get 'message err-obj)))
    (should (= err-code expected-code))
    (should (string-match expected-message err-msg))))

(defun mcp-test--check-invalid-jsonrpc-version (version)
  "Test that JSON-RPC request with VERSION is rejected properly."
  (mcp-test--with-server
    (mcp-test--check-jsonrpc-error
     (json-encode
      `(("jsonrpc" . ,version) ("method" . "tools/list") ("id" . 42)))
     -32600 "Invalid Request: Not JSON-RPC 2.0")))

(defun mcp-test--call-tool (tool-id &optional id args)
  "Call a tool with TOOL-ID and return its successful result.
Optional ID is the JSON-RPC request ID (defaults to 1).
Optional ARGS is the association list of arguments to pass to the tool."
  (mcp-test--get-request-result
   (mcp-create-tools-call-request tool-id id args)))

(defun mcp-test--verify-tool-not-found (tool-id)
  "Verify a call to non-existent tool with TOOL-ID returning an error."
  (mcp-test--check-jsonrpc-error
   (mcp-create-tools-call-request tool-id 999) -32600
   (format "Tool not found: %s" tool-id)))

(defun mcp-test--get-tool-list-for-request (request)
  "Get the tool list from a tools/list REQUEST.
Return the `tools` array from the result after verifying it is an array."
  (let ((result
         (alist-get 'tools (mcp-test--get-request-result request))))
    (should (arrayp result))
    result))

(defun mcp-test--get-tool-list ()
  "Get the response to a standard `tool/list` request."
  (mcp-test--get-tool-list-for-request
   (mcp-create-tools-list-request)))

(defun mcp-test--verify-tool-list-request (expected-tools)
  "Verify a `tools/list` response against EXPECTED-TOOLS.
EXPECTED-TOOLS should be an alist of (tool-name . tool-properties)."
  (let ((tools (mcp-test--get-tool-list)))
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

(defun mcp-test--verify-tool-schema-in-single-tool-list
    (&optional param-name param-type param-description)
  "Verify that schema of the only tool in the tool list has correct structure.
When PARAM-NAME is nil, verifies a zero-argument tool schema.
Otherwise, verifies a one-parameter tool schema with:
PARAM-NAME as the name of the parameter to validate.
PARAM-TYPE as the expected type of the parameter.
PARAM-DESCRIPTION as the expected description of the parameter."
  (let* ((tools (mcp-test--get-tool-list))
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
      (should (null (alist-get 'required schema)))
      (should (null (alist-get 'properties schema))))))

(defun mcp-test--check-mcp-content-format (result expected-text)
  "Check that RESULT follows the MCP content format with EXPECTED-TEXT."
  ;; Check for proper MCP format
  (let* ((content (alist-get 'content result))
         (content-item (aref content 0)))
    (should (arrayp content))
    (should (= 1 (length content)))
    (should (string= "text" (alist-get 'type content-item)))
    (should (string= expected-text (alist-get 'text content-item))))
  ;; Check isError field
  (should (not (null (alist-get 'isError result nil t))))
  (should (eq :json-false (alist-get 'isError result))))

;;; Initialization and server capabilities tests

(ert-deftest mcp-test-initialize ()
  "Test the MCP `initialize` request handling."
  (mcp-test--with-server
    (let* ((result (mcp-test--get-initialize-result))
           (protocol-version (alist-get 'protocolVersion result))
           (capabilities (alist-get 'capabilities result))
           (server-name
            (alist-get 'name (alist-get 'serverInfo result))))
      ;; Verify version
      (should (stringp protocol-version))
      (should (string= "2025-03-26" protocol-version))
      ;; Verify capability objects are present and properly formatted
      ;; (empty objects deserialize to nil)
      (should (equal nil (alist-get 'tools capabilities)))
      (should (equal nil (alist-get 'resources capabilities)))
      (should (equal nil (alist-get 'prompts capabilities)))
      ;; Verify server info
      (should (string= mcp--name server-name)))))

(ert-deftest mcp-test-initialize-registered-tool ()
  "Test that registered tool appears in server capabilities."
  (mcp-test--with-tools ((#'mcp-test--tool-handler-simple
                          :id "test-tool"
                          :description "A tool for testing"))
    (let* ((result (mcp-test--get-initialize-result))
           (capabilities (alist-get 'capabilities result))
           (tools-capability (alist-get 'tools capabilities))
           (list-changed (alist-get 'listChanged tools-capability)))
      (should (eq t list-changed)))))

(ert-deftest mcp-test-notifications-initialized ()
  "Test the MCP `notifications/initialized` request handling."
  (mcp-test--with-server
    (let* ((notifications-initialized
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "notifications/initialized"))))
           (response (mcp-process-jsonrpc notifications-initialized)))
      ;; Notifications are one-way, should return nil
      (should (null response)))))

(ert-deftest mcp-test-initialize-old-protocol-version ()
  "Test server responds with its version for older client version."
  (mcp-test--with-server
    (let* ((init-request
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "initialize") ("id" . 16)
               ("params" .
                (("protocolVersion" . "2024-11-05")
                 ("capabilities" .
                  (("tools" . t)
                   ("resources" . nil)
                   ("prompts" . nil))))))))
           (response (mcp-process-jsonrpc init-request))
           (resp-obj (json-read-from-string response))
           (result (alist-get 'result resp-obj))
           (protocol-version (alist-get 'protocolVersion result)))
      ;; Server should respond with its supported version, not client's
      (should (string= "2025-03-26" protocol-version))
      ;; Response should not have an error
      (should (null (alist-get 'error resp-obj))))))

;;; `mcp-register-tool' tests

(ert-deftest mcp-test-register-tool-error-missing-id ()
  "Test that tool registration with missing :id produces an error."
  (should-error
   (mcp-register-tool
    #'mcp-test--tool-handler-simple
    :description "Test tool without ID")
   :type 'error))

(ert-deftest mcp-test-register-tool-error-missing-description ()
  "Test that tool registration with missing :description produces an error."
  (should-error
   (mcp-register-tool
    #'mcp-test--tool-handler-simple
    :id "test-tool-no-desc")
   :type 'error))

(ert-deftest mcp-test-register-tool-error-missing-handler ()
  "Test that tool registration with non-function handler produces an error."
  (should-error
   (mcp-register-tool
    "not-a-function"
    :id "test-tool-bad-handler"
    :description "Test tool with invalid handler")
   :type 'error))

(ert-deftest mcp-test-register-tool-error-duplicate-param-description
    ()
  "Test that duplicate parameter descriptions cause an error."
  (should-error
   (mcp-register-tool
    #'mcp-test--tool-handler-duplicate-param
    :id "duplicate-param-tool"
    :description "Tool with duplicate parameter")
   :type 'error))

(ert-deftest mcp-test-register-tool-error-mismatched-param ()
  "Test that parameter names must match function arguments."
  (should-error
   (mcp-register-tool
    #'mcp-test--tool-handler-mismatched-param
    :id "mismatched-param-tool"
    :description "Tool with mismatched parameter")
   :type 'error))

(ert-deftest mcp-test-register-tool-error-missing-param ()
  "Test that all function parameters must be documented."
  (should-error
   (mcp-register-tool
    #'mcp-test--tool-handler-missing-param
    :id "missing-param-tool"
    :description "Tool with missing parameter docs")
   :type 'error))

(ert-deftest mcp-test-register-tool-error-duplicate-id ()
  "Test that registering a tool with duplicate ID produces an error."
  (mcp-register-tool
   #'mcp-test--tool-handler-simple
   :id "duplicate-test"
   :description "First registration")

  (should-error
   (mcp-register-tool
    #'mcp-test--tool-handler-simple
    :id "duplicate-test"
    :description "Second registration")
   :type 'error)

  ;; Clean up
  (mcp-unregister-tool "duplicate-test"))

(ert-deftest mcp-test-register-tool-bytecode ()
  "Test schema generation for a handler loaded as bytecode.
This test verifies that MCP can correctly extract parameter information
from a function loaded from bytecode rather than interpreted elisp."
  (let* ((source-file
          (expand-file-name "mcp-test-bytecode-handler.el"))
         (bytecode-file (byte-compile-dest-file source-file)))
    (should (byte-compile-file source-file))

    (should (load bytecode-file nil t t))

    (mcp-test--with-tools
        ((#'mcp-test-bytecode-handler--handler
          :id "bytecode-handler"
          :description "A tool with a handler loaded from bytecode"))
      (mcp-test--verify-tool-schema-in-single-tool-list
       "input-string"
       "string"
       "Input string parameter for bytecode testing"))

    (when (file-exists-p bytecode-file)
      (delete-file bytecode-file))))

;;; `mcp-unregister-tool' tests

(ert-deftest mcp-test-unregister-tool ()
  "Test that `mcp-unregister-tool' removes a tool correctly."
  (mcp-test--with-server
    (mcp-register-tool
     #'mcp-test--tool-handler-simple
     :id mcp-test--unregister-tool-id
     :description "Tool for unregister test")

    (mcp-test--verify-tool-list-request
     `((,mcp-test--unregister-tool-id
        .
        ((description . "Tool for unregister test")
         (inputSchema . ((type . "object")))))))

    (let ((result
           (mcp-test--call-tool mcp-test--unregister-tool-id 44)))
      (mcp-test--check-mcp-content-format result "test result"))

    (should (mcp-unregister-tool mcp-test--unregister-tool-id))

    (mcp-test--verify-tool-list-request '())
    (mcp-test--verify-tool-not-found mcp-test--unregister-tool-id)))

(ert-deftest mcp-test-unregister-tool-nonexistent ()
  "Test that `mcp-unregister-tool' returns nil for nonexistent tools."
  (mcp-register-tool
   #'mcp-test--tool-handler-simple
   :id "test-other"
   :description "Other test tool")
  (should-not (mcp-unregister-tool "nonexistent-tool"))
  (mcp-unregister-tool "test-other"))

(ert-deftest mcp-test-unregister-tool-when-no-tools ()
  "Test that `mcp-unregister-tool' works when no tools are registered."
  (should-not (mcp-unregister-tool "any-tool")))

;;; Notification tests

(ert-deftest mcp-test-notifications-cancelled ()
  "Test the MCP `notifications/cancelled` request handling."
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
  "Test `tools/list` returning one tool with correct fields and schema."
  (mcp-test--with-tools ((#'mcp-test--tool-handler-simple
                          :id "test-tool"
                          :description "A tool for testing"))
    (mcp-test--verify-tool-list-request
     '(("test-tool" .
        ((description . "A tool for testing")
         (inputSchema . ((type . "object")))))))))

(ert-deftest mcp-test-tools-list-with-title ()
  "Test that `tools/list` includes title in response."
  (mcp-test--with-tools ((#'mcp-test--tool-handler-simple
                          :id "tool-with-title"
                          :description "A tool for testing titles"
                          :title "Friendly Tool Name"))
    (mcp-test--verify-tool-list-request
     '(("tool-with-title" .
        ((description . "A tool for testing titles")
         (annotations . ((title . "Friendly Tool Name")))
         (inputSchema . ((type . "object")))))))))

(ert-deftest mcp-test-tools-list-two ()
  "Test the `tools/list` method returning multiple tools with correct fields."
  (mcp-test--with-tools ((#'mcp-test--tool-handler-simple
                          :id "test-tool-1"
                          :description "First tool for testing")
                         (#'mcp-test--tool-handler-simple
                          :id "test-tool-2"
                          :description "Second tool for testing"))
    (mcp-test--verify-tool-list-request
     '(("test-tool-1" .
        ((description . "First tool for testing")
         (inputSchema . ((type . "object")))))
       ("test-tool-2" .
        ((description . "Second tool for testing")
         (inputSchema . ((type . "object")))))))))

(ert-deftest mcp-test-tools-list-zero ()
  "Test the `tools/list` method returning empty array with no tools."
  (mcp-test--with-server
    (mcp-test--verify-tool-list-request '())))

(ert-deftest mcp-test-tools-list-schema-one-arg-handler ()
  "Test that `tools/list` schema includes parameter descriptions."
  (mcp-test--with-tools
      ((#'mcp-test--tool-handler-string-arg
        :id "requires-arg"
        :description "A tool that requires an argument"))
    (mcp-test--verify-tool-schema-in-single-tool-list
     "input-string" "string" "test parameter for string input")))

(ert-deftest mcp-test-tools-list-extra-key ()
  "Test that `tools/list` request with an extra, unexpected key works correctly.
Per JSON-RPC 2.0 spec, servers should ignore extra/unknown members."
  (mcp-test--with-server
    ;; Create a tools/list request with an extra key
    (let ((request-with-extra
           (json-encode
            `(("jsonrpc" . "2.0")
              ("method" . "tools/list")
              ("id" . 43)
              ("extra_key" . "unexpected value")))))
      ;; Just checking if tool list is an array is enough
      (mcp-test--get-tool-list-for-request request-with-extra))))

(ert-deftest mcp-test-tools-list-read-only-hint ()
  "Test that `tools/list` response includes readOnlyHint=true."
  (mcp-test--with-tools
      ((#'mcp-test--tool-handler-simple
        :id "read-only-tool"
        :description "A tool that doesn't modify its environment"
        :read-only t))
    (mcp-test--verify-tool-list-request
     '(("read-only-tool" .
        ((description . "A tool that doesn't modify its environment")
         (annotations . ((readOnlyHint . t)))
         (inputSchema . ((type . "object")))))))))

(ert-deftest mcp-test-tools-list-read-only-hint-false ()
  "Test that `tools/list` response includes readOnlyHint=false."
  (mcp-test--with-tools
      ((#'mcp-test--tool-handler-simple
        :id "non-read-only-tool"
        :description "Tool that modifies its environment"
        :read-only nil))
    (mcp-test--verify-tool-list-request
     '(("non-read-only-tool" .
        ((description . "Tool that modifies its environment")
         (annotations . ((readOnlyHint . :json-false)))
         (inputSchema . ((type . "object")))))))))

(ert-deftest mcp-test-tools-list-multiple-annotations ()
  "Test `tools/list` response including multiple annotations."
  (mcp-test--with-tools
      ((#'mcp-test--tool-handler-simple
        :id "multi-annotated-tool"
        :description "A tool with multiple annotations"
        :title "Friendly Multi-Tool"
        :read-only t))
    (mcp-test--verify-tool-list-request
     '(("multi-annotated-tool" .
        ((description . "A tool with multiple annotations")
         (annotations
          . ((title . "Friendly Multi-Tool") (readOnlyHint . t)))
         (inputSchema . ((type . "object")))))))))

;;; `mcp-create-tools-call-request' tests

(ert-deftest mcp-test-create-tools-call-request-id-and-args ()
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

(ert-deftest mcp-test-create-tools-call-request-empty-args ()
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

(ert-deftest mcp-test-tools-call-mcp-tool-throw ()
  "Test tool handler calling `mcp-tool-throw'."
  (mcp-test--with-tools ((#'mcp-test--tool-handler-mcp-tool-throw
                          :id "failing-tool"
                          :description "A tool that always fails"))
    (let ((result (mcp-test--call-tool "failing-tool" 11)))
      ;; Check for proper MCP format
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
    (mcp-test--check-jsonrpc-error
     (mcp-create-tools-call-request "generic-error-tool" 12)
     -32603
     "Internal error executing tool")))

(ert-deftest mcp-test-tools-call-no-args ()
  "Test the `tools/call` request with a tool that takes no arguments."
  (mcp-test--with-tools
      ((#'mcp-test--tool-handler-string-list
        :id "string-list-tool"
        :description "A tool that returns a string with items"))
    (let ((result (mcp-test--call-tool "string-list-tool" 9)))
      (mcp-test--check-mcp-content-format
       result mcp-test--string-list-result))))

(ert-deftest mcp-test-tools-call-empty-string ()
  "Test the `tools/call` request with a tool that returns an empty string."
  (mcp-test--with-tools
      ((#'mcp-test--tool-handler-empty-string
        :id "empty-string-tool"
        :description "A tool that returns an empty string"))
    (mcp-test--verify-tool-schema-in-single-tool-list)

    ;; Then test the actual tool execution
    (let ((result (mcp-test--call-tool "empty-string-tool" 10)))
      (mcp-test--check-mcp-content-format result ""))))

(ert-deftest mcp-test-tools-call-with-string-arg ()
  "Test the `tools/call` request with a tool that takes a string argument."
  (mcp-test--with-tools
      ((#'mcp-test--tool-handler-string-arg
        :id "string-arg-tool"
        :description "A tool that echoes a string argument"))
    (let* ((test-input "Hello, world!")
           (args `(("input" . ,test-input)))
           (result (mcp-test--call-tool "string-arg-tool" 13 args)))
      (mcp-test--check-mcp-content-format
       result (concat "Echo: " test-input)))))

(ert-deftest mcp-test-tools-call-unregistered-tool ()
  "Test the `tools/call` request with a tool that was never registered."
  (mcp-test--with-server
    (mcp-test--verify-tool-not-found mcp-test--nonexistent-tool-id)))

;;; prompts/list tests

(ert-deftest mcp-test-prompts-list-zero ()
  "Test the `prompts/list` request returning an empty array with no prompts."
  (mcp-test--with-server
    (let ((result
           (mcp-test--get-request-result
            (mcp-test--prompts-list-request))))
      (should (alist-get 'prompts result))
      (should (arrayp (alist-get 'prompts result)))
      (should (= 0 (length (alist-get 'prompts result)))))))

;;; `mcp-process-jsonrpc' tests

(ert-deftest mcp-test-parse-error ()
  "Test that invalid JSON input to `mcp-process-jsonrpc' returns a parse error."
  (mcp-test--with-server
    (mcp-test--check-jsonrpc-error
     "This is not valid JSON" -32700 "Parse error")))

(ert-deftest mcp-test-method-not-found ()
  "Test that unknown methods return method-not-found error."
  (mcp-test--with-server
    (mcp-test--check-jsonrpc-error
     (json-encode
      '(("jsonrpc" . "2.0")
        ("method" . "unknown/method")
        ("id" . 99)))
     -32601 "Method not found: unknown/method")))

(ert-deftest mcp-test-invalid-jsonrpc ()
  "Test that valid JSON that is not JSON-RPC returns an invalid request error."
  (mcp-test--with-server
    (mcp-test--check-jsonrpc-error
     (json-encode '(("name" . "Test Object") ("value" . 42)))
     -32600
     "Invalid Request: Not JSON-RPC 2.0")))

(ert-deftest mcp-test-invalid-jsonrpc-older-version ()
  "Test that JSON-RPC with older version (1.1) is rejected properly."
  (mcp-test--check-invalid-jsonrpc-version "1.1"))

(ert-deftest mcp-test-invalid-jsonrpc-non-standard-version ()
  "Test that JSON-RPC with non-standard version string is rejected properly."
  (mcp-test--check-invalid-jsonrpc-version "non-standard"))

(ert-deftest mcp-test-invalid-jsonrpc-missing-id ()
  "Test that JSON-RPC request lacking the `id` key is rejected properly."
  (mcp-test--with-server
    (mcp-test--check-jsonrpc-error
     (json-encode '(("jsonrpc" . "2.0") ("method" . "tools/list")))
     -32600
     "Invalid Request: Missing required 'id' field")))

(ert-deftest mcp-test-invalid-jsonrpc-missing-method ()
  "Test that JSON-RPC request lacking the `method` key is rejected properly."
  (mcp-test--with-server
    (mcp-test--check-jsonrpc-error
     (json-encode '(("jsonrpc" . "2.0") ("id" . 42)))
     -32600
     "Invalid Request: Missing required 'method' field")))

;;; Logging tests

(ert-deftest mcp-test-log-io-t ()
  "Test that when `mcp-log-io' is t, JSON-RPC messages are logged."
  (setq mcp-log-io t)

  (mcp-test--with-server
    (let* ((request (mcp-create-tools-list-request))
           (response (mcp-process-jsonrpc request)))

      (let ((log-buffer (get-buffer "*mcp-log*")))
        (should log-buffer)

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

  (setq mcp-log-io nil))

(ert-deftest mcp-test-log-io-nil ()
  "Test that when `mcp-log-io' is nil, JSON-RPC messages are not logged."
  (setq mcp-log-io nil)

  (mcp-test--with-server
    (let ((request (mcp-create-tools-list-request)))
      (mcp-process-jsonrpc request)
      (should-not (get-buffer "*mcp-log*")))))

;;; Misc tests

(ert-deftest mcp-test-server-restart-preserves-tools ()
  "Test that server restart preserves registered tools."
  (mcp-test--with-tools
      ((#'mcp-test--tool-handler-simple
        :id "persistent-tool"
        :description "Test persistence across restarts"))
    (mcp-stop)
    (mcp-start)

    (let ((tools (mcp-test--get-tool-list)))
      (should (= 1 (length tools)))
      (should
       (string=
        "persistent-tool" (alist-get 'name (aref tools 0)))))))

(ert-deftest mcp-test-interactive-commands ()
  "Test that `mcp-start' and `mcp-stop' are interactive commands."
  (should (commandp #'mcp-start))
  (should (commandp #'mcp-stop)))

(provide 'mcp-test)
;;; mcp-test.el ends here
