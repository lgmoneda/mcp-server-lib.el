;;; mcp-test.el --- Tests for mcp.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Laurynas Biveinis

;; Author: Laurynas Biveinis
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

;; ERT tests for mcp.el.

;;; Code:

(require 'ert)
(require 'mcp)
(require 'json)

;;; Test Helpers

(defun mcp-test--tool-handler ()
  "Test tool handler function for MCP tool testing."
  "test result")

(defun mcp-test--tools-list-request (id)
  "Create a tools/list JSON-RPC request with ID."
  (json-encode `(("jsonrpc" . "2.0") ("method" . "tools/list") ("id" . ,id))))

(defun mcp-test--verify-tool-list-response (response expected-tools)
  "Verify RESPONSE from tools/list against EXPECTED-TOOLS.
EXPECTED-TOOLS should be an alist of (tool-name . tool-properties)."
  (let* ((response-obj (json-read-from-string response))
         (result (alist-get 'result response-obj))
         (tools (alist-get 'tools result)))
    (should result)
    (should tools)
    (should (arrayp tools))
    (should (= (length expected-tools) (length tools)))
    ;; Check each expected tool
    (dolist (expected expected-tools)
      (let* ((expected-name (car expected))
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
               (let ((annotations (alist-get 'annotations found-tool)))
                 (should annotations)
                 (dolist (annot prop-value)
                   (should
                    (equal (cdr annot) (alist-get (car annot) annotations))))))
              ;; Regular property check
              (_
               (should
                (equal prop-value (alist-get prop-name found-tool)))))))))))

(ert-deftest mcp-test-tool-registration-in-capabilities ()
  "Test registered tool appears in server capabilities."
  (mcp-register-tool
   #'mcp-test--tool-handler
   :id "test-tool"
   :description "A tool for testing")
  (mcp-start)
  (unwind-protect
      (progn
        (let* ((initialize-request
                (json-encode
                 `(("jsonrpc" . "2.0")
                   ("method" . "initialize") ("id" . 1)
                   ("params" .
                    (("protocolVersion" . "2024-11-05")
                     ("capabilities" . (("tools" . t))))))))
               (initialize-response (mcp-process-jsonrpc initialize-request))
               (response-obj (json-read-from-string initialize-response)))

          (should (alist-get 'result response-obj))

          (let* ((result (alist-get 'result response-obj))
                 (capabilities (alist-get 'capabilities result))
                 (tools-capability (alist-get 'tools capabilities)))

            (should tools-capability)
            (should (alist-get 'listChanged tools-capability))
            (should (eq t (alist-get 'listChanged tools-capability))))))
    (mcp-stop)
    (mcp-unregister-tool "test-tool")))

;;; Transport Tests

(ert-deftest mcp-test-stdio-transport ()
  "Test the stdio transport using `mcp-process-jsonrpc`."
  ;; Start the MCP server using the singleton API
  (mcp-start)
  (unwind-protect
      (progn
        ;; Test listing tools
        (let* ((list-request
                (json-encode
                 `(("jsonrpc" . "2.0") ("method" . "tools/list") ("id" . 1))))
               (list-response (mcp-process-jsonrpc list-request))
               (list-result
                (alist-get 'result (json-read-from-string list-response))))
          (should (arrayp (alist-get 'tools list-result)))))

    ;; Cleanup - always stop server
    (mcp-stop)))

(ert-deftest mcp-test-initialize ()
  "Test the MCP initialize request handling."
  ;; Start the MCP server using the singleton API
  (mcp-start)
  (unwind-protect
      (progn
        ;; Test initialize with valid parameters
        (let* ((initialize-request
                (json-encode
                 `(("jsonrpc" . "2.0")
                   ("method" . "initialize") ("id" . 3)
                   ("params" .
                    (("protocolVersion" . "2024-11-05")
                     ("capabilities" .
                      (("tools" . t)
                       ("resources" . nil)
                       ("prompts" . nil))))))))
               (initialize-response (mcp-process-jsonrpc initialize-request))
               (initialize-result
                (alist-get
                 'result (json-read-from-string initialize-response))))
          ;; Verify the server responded with its protocol version
          (should (stringp (alist-get 'protocolVersion initialize-result)))
          (should
           (string=
            "2024-11-05" (alist-get 'protocolVersion initialize-result)))
          ;; Verify server capabilities
          (should (alist-get 'capabilities initialize-result))

          ;; Verify capability objects are present and properly formatted
          ;; (empty objects deserialize to nil)
          (let ((capabilities (alist-get 'capabilities initialize-result)))
            (should (equal nil (alist-get 'tools capabilities)))
            (should (equal nil (alist-get 'resources capabilities)))
            (should (equal nil (alist-get 'prompts capabilities))))
          ;; Verify server info
          (should (alist-get 'serverInfo initialize-result))
          (let ((server-name
                 (alist-get 'name (alist-get 'serverInfo initialize-result))))
            (should (string= mcp--name server-name)))))
    ;; Cleanup - always stop server
    (mcp-stop)))


(ert-deftest mcp-test-notifications-initialized-format ()
  "Test the MCP notifications/initialized format handling."
  (mcp-start)
  (unwind-protect
      (progn
        (let* ((notifications-initialized
                (json-encode
                 `(("jsonrpc" . "2.0")
                   ("method" . "notifications/initialized"))))
               (response (mcp-process-jsonrpc notifications-initialized)))
          ;; Notifications are one-way, should return nil
          (should (null response))))
    (mcp-stop)))

(ert-deftest mcp-test-notifications-cancelled-format ()
  "Test the MCP notifications/cancelled format handling."
  (mcp-start)
  (unwind-protect
      (progn
        (let* ((notifications-cancelled
                (json-encode
                 `(("jsonrpc" . "2.0") ("method" . "notifications/cancelled"))))
               (response (mcp-process-jsonrpc notifications-cancelled)))
          ;; Notifications are one-way, should return nil
          (should (null response))))
    (mcp-stop)))

(ert-deftest mcp-test-tools-list-zero ()
  "Test the `tools/list` method returns empty array with no tools."
  (mcp-start)
  (unwind-protect
      (progn
        (let* ((response (mcp-process-jsonrpc (mcp-test--tools-list-request 5)))
               (response-obj (json-read-from-string response))
               (result (alist-get 'result response-obj)))
          (should result)
          (should (alist-get 'tools result))
          (should (arrayp (alist-get 'tools result)))
          (should (= 0 (length (alist-get 'tools result))))))
    (mcp-stop)))

(defun mcp-test--failing-tool-handler ()
  "Test tool handler that always fails with `mcp-tool-throw'."
  (mcp-tool-throw "This tool intentionally fails"))

(ert-deftest mcp-test-tools-call-error ()
  "Test that tool errors are properly formatted with isError=true."
  (mcp-start)
  (unwind-protect
      (progn
        (mcp-register-tool
         #'mcp-test--failing-tool-handler
         :id "failing-tool"
         :description "A tool that always fails")
        (let* ((response
                (mcp-process-jsonrpc
                 (mcp-test--tools-call-request 11 "failing-tool")))
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
              "This tool intentionally fails" (alist-get 'text content-item))))
          ;; Check isError field is true
          (should (alist-get 'isError result))
          (should (eq t (alist-get 'isError result)))))
    (mcp-stop)
    (mcp-unregister-tool "failing-tool")))

(defun mcp-test--generic-error-handler ()
  "Test tool handler that throws a generic error."
  (error "Generic error occurred"))

(ert-deftest mcp-test-tools-call-generic-error ()
  "Test that generic errors use standard JSON-RPC error format."
  (mcp-start)
  (unwind-protect
      (progn
        (mcp-register-tool
         #'mcp-test--generic-error-handler
         :id "generic-error-tool"
         :description "A tool that throws a generic error")
        (let* ((response
                (mcp-process-jsonrpc
                 (mcp-test--tools-call-request 12 "generic-error-tool")))
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
            "Internal error executing tool" (alist-get 'message error-obj)))))
    (mcp-stop)
    (mcp-unregister-tool "generic-error-tool")))

(ert-deftest mcp-test-tools-list-one ()
  "Test tools/list returns one tool with correct fields and schema."
  (mcp-start)
  (unwind-protect
      (progn
        ;; Register zero-arg handler tool
        (mcp-register-tool
         #'mcp-test--tool-handler
         :id "test-tool"
         :description "A tool for testing")

        (let ((response (mcp-process-jsonrpc (mcp-test--tools-list-request 6))))
          (mcp-test--verify-tool-list-response
           response
           '(("test-tool" .
              ((description . "A tool for testing")
               (inputSchema . ((type . "object")))))))))
    (mcp-stop)
    (mcp-unregister-tool "test-tool")))

(ert-deftest mcp-test-tools-list-with-title ()
  "Test that tools/list includes title in response."
  (mcp-start)
  (unwind-protect
      (progn
        ;; Register tool with title annotation
        (mcp-register-tool
         #'mcp-test--tool-handler
         :id "tool-with-title"
         :description "A tool for testing titles"
         :title "Friendly Tool Name")

        (let ((response
               (mcp-process-jsonrpc (mcp-test--tools-list-request 14))))
          (mcp-test--verify-tool-list-response
           response
           '(("tool-with-title" .
              ((description . "A tool for testing titles")
               (annotations . ((title . "Friendly Tool Name")))
               (inputSchema . ((type . "object")))))))))
    (mcp-stop)
    (mcp-unregister-tool "tool-with-title")))

(ert-deftest mcp-test-tools-list-two ()
  "Test the `tools/list` method returns multiple tools with correct fields."
  (mcp-start)
  (unwind-protect
      (progn
        (mcp-register-tool
         #'mcp-test--tool-handler
         :id "test-tool-1"
         :description "First tool for testing")
        (mcp-register-tool
         #'mcp-test--tool-handler
         :id "test-tool-2"
         :description "Second tool for testing")
        (let ((response (mcp-process-jsonrpc (mcp-test--tools-list-request 7))))
          (mcp-test--verify-tool-list-response
           response
           '(("test-tool-1" .
              ((description . "First tool for testing")
               (inputSchema . ((type . "object")))))
             ("test-tool-2" .
              ((description . "Second tool for testing")
               (inputSchema . ((type . "object")))))))))
    (mcp-stop)
    (mcp-unregister-tool "test-tool-1")
    (mcp-unregister-tool "test-tool-2")))

(ert-deftest mcp-test-unregister-tool ()
  "Test that `mcp-unregister-tool' removes a tool correctly."
  (let ((tools-before (hash-table-count mcp--tools)))
    (mcp-register-tool
     #'mcp-test--tool-handler
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
   #'mcp-test--tool-handler
   :id "test-other"
   :description "Other test tool")
  (should-not (mcp-unregister-tool "nonexistent-tool"))
  (mcp-unregister-tool "test-other"))

(ert-deftest mcp-test-missing-id-error ()
  "Test that tool registration with missing :id produces an error."
  (should-error
   (mcp-register-tool
    #'mcp-test--tool-handler
    :description "Test tool without ID")
   :type 'error))

(ert-deftest mcp-test-missing-description-error ()
  "Test that tool registration with missing :description produces an error."
  (should-error
   (mcp-register-tool #'mcp-test--tool-handler :id "test-tool-no-desc")
   :type 'error))

(ert-deftest mcp-test-missing-handler-error ()
  "Test that tool registration with non-function handler produces an error."
  (should-error
   (mcp-register-tool
    "not-a-function"
    :id "test-tool-bad-handler"
    :description "Test tool with invalid handler")
   :type 'error))

(ert-deftest mcp-test-unregister-when-no-tools ()
  "Test that `mcp-unregister-tool' works when no tools are registered."
  (should-not (mcp-unregister-tool "any-tool")))

(ert-deftest mcp-test-duplicate-param-description-error ()
  "Test that duplicate parameter descriptions cause an error."
  (should-error
   (mcp-register-tool
    #'mcp-test--duplicate-param-handler
    :id "duplicate-param-tool"
    :description "Tool with duplicate parameter")
   :type 'error))

(ert-deftest mcp-test-mismatched-param-error ()
  "Test that parameter names must match function arguments."
  (should-error
   (mcp-register-tool
    #'mcp-test--mismatched-param-handler
    :id "mismatched-param-tool"
    :description "Tool with mismatched parameter")
   :type 'error))

(ert-deftest mcp-test-missing-param-error ()
  "Test that all function parameters must be documented."
  (should-error
   (mcp-register-tool
    #'mcp-test--missing-param-handler
    :id "missing-param-tool"
    :description "Tool with missing parameter docs")
   :type 'error))

;;; Prompts Tests

(defun mcp-test--prompts-list-request (id)
  "Create a prompt list JSON-RPC request with ID."
  (json-encode `(("jsonrpc" . "2.0") ("method" . "prompts/list") ("id" . ,id))))

(ert-deftest mcp-test-prompts-list-zero ()
  "Test the `prompts/list` method returns empty array with no prompts."
  (mcp-start)
  (unwind-protect
      (progn
        (let* ((response
                (mcp-process-jsonrpc (mcp-test--prompts-list-request 8)))
               (response-obj (json-read-from-string response))
               (result (alist-get 'result response-obj)))
          (should result)
          (should (alist-get 'prompts result))
          (should (arrayp (alist-get 'prompts result)))
          (should (= 0 (length (alist-get 'prompts result))))))
    (mcp-stop)))

;;; Tool Call Tests

(defconst mcp-test--string-list-result "item1 item2 item3"
  "Test data for string list tool.")

(defun mcp-test--string-list-tool-handler ()
  "Test tool handler function to return a string with items."
  mcp-test--string-list-result)

(defun mcp-test--empty-array-tool-handler ()
  "Test tool handler function to return an empty string."
  "")

(defun mcp-test--string-arg-tool-handler (input-string)
  "Test tool handler that accepts a string argument.
INPUT-STRING is the string argument passed to the tool.

MCP Parameters:
  input-string - test parameter for string input"
  (concat "Echo: " input-string))

(defun mcp-test--duplicate-param-handler (input-string)
  "Test handler with duplicate parameter.
INPUT-STRING is the string argument.

MCP Parameters:
  input-string - first description
  input-string - second description"
  (concat "Test: " input-string))

(defun mcp-test--mismatched-param-handler (input-string)
  "Test handler with mismatched parameter name.
INPUT-STRING is the string argument.

MCP Parameters:
  wrong-param-name - description for non-existent parameter"
  (concat "Test: " input-string))

(defun mcp-test--missing-param-handler (input-string)
  "Test handler with missing parameter documentation.
INPUT-STRING is the string argument.

MCP Parameters:"
  (concat "Test: " input-string))

(defun mcp-test--tools-call-request (id tool-name &optional args)
  "Create a tools/call JSON-RPC request with ID for TOOL-NAME.
Optional ARGS is an association list of tool arguments."
  (json-encode
   `(("jsonrpc" . "2.0")
     ("method" . "tools/call")
     ("id" . ,id)
     ("params" . (("name" . ,tool-name) ("arguments" . ,(or args '())))))))

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

(ert-deftest mcp-test-tools-call-no-args ()
  "Test the `tools/call` method with a tool that takes no arguments."
  (mcp-start)
  (unwind-protect
      (progn
        (mcp-register-tool
         #'mcp-test--string-list-tool-handler
         :id "string-list-tool"
         :description "A tool that returns a string with items")
        (let* ((response
                (mcp-process-jsonrpc
                 (mcp-test--tools-call-request 9 "string-list-tool")))
               (response-obj (json-read-from-string response))
               (result (alist-get 'result response-obj)))
          (should result)
          (mcp-test--check-mcp-content-format
           result mcp-test--string-list-result)))
    (mcp-stop)
    (mcp-unregister-tool "string-list-tool")))

(ert-deftest mcp-test-tools-call-empty-string ()
  "Test the `tools/call` method with a tool that returns an empty string."
  (mcp-start)
  (unwind-protect
      (progn
        ;; Clear any previously registered tools to ensure only this tool exists
        (clrhash mcp--tools)

        (mcp-register-tool
         #'mcp-test--empty-array-tool-handler
         :id "empty-string-tool"
         :description "A tool that returns an empty string")

        ;; First check the schema for this zero-arg handler
        (let* ((list-req
                (json-encode
                 `(("jsonrpc" . "2.0") ("method" . "tools/list") ("id" . 100))))
               (list-response (mcp-process-jsonrpc list-req))
               (list-obj (json-read-from-string list-response))
               (tools (alist-get 'tools (alist-get 'result list-obj)))
               (tool (aref tools 0))
               (schema (alist-get 'inputSchema tool)))

          ;; Verify schema is correct for a zero-arg function
          (should (equal '((type . "object")) schema)))

        ;; Then test the actual tool execution
        (let* ((req (mcp-test--tools-call-request 10 "empty-string-tool"))
               (response (mcp-process-jsonrpc req))
               (response-obj (json-read-from-string response))
               (result (alist-get 'result response-obj)))
          (should result)
          (mcp-test--check-mcp-content-format result "")))
    (mcp-stop)
    (mcp-unregister-tool "empty-string-tool")))

(ert-deftest mcp-test-schema-for-one-arg-handler ()
  "Test schema includes parameter descriptions."
  (mcp-start)
  (unwind-protect
      (progn
        ;; Register a tool with a handler with parameter description
        (mcp-register-tool
         #'mcp-test--string-arg-tool-handler
         :id "requires-arg"
         :description "A tool that requires an argument")

        ;; Get schema via tools/list
        (let* ((list-req
                (json-encode
                 `(("jsonrpc" . "2.0") ("method" . "tools/list") ("id" . 42))))
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
                 (alist-get 'input-string (alist-get 'properties schema))))
            (should param-schema)
            (should (equal "string" (alist-get 'type param-schema)))
            (should
             (equal
              "test parameter for string input"
              (alist-get 'description param-schema))))))
    (mcp-stop)
    (mcp-unregister-tool "requires-arg")))

(ert-deftest mcp-test-tools-call-with-string-arg ()
  "Test the `tools/call` method with a tool that takes a string argument."
  (mcp-start)
  (unwind-protect
      (progn
        (mcp-register-tool
         #'mcp-test--string-arg-tool-handler
         :id "string-arg-tool"
         :description "A tool that echoes a string argument")
        (let* ((test-input "Hello, world!")
               (args `(("input" . ,test-input)))
               (req (mcp-test--tools-call-request 13 "string-arg-tool" args))
               (response (mcp-process-jsonrpc req))
               (response-obj (json-read-from-string response))
               (result (alist-get 'result response-obj)))
          (should result)
          (mcp-test--check-mcp-content-format
           result (concat "Echo: " test-input))))
    (mcp-stop)
    (mcp-unregister-tool "string-arg-tool")))

;;; Logging Tests

(ert-deftest mcp-test-log-io-t ()
  "Test that when `mcp-log-io' is t, JSON-RPC messages are logged."
  ;; Set logging to enabled
  (setq mcp-log-io t)

  ;; Start MCP server
  (mcp-start)

  (unwind-protect
      (progn
        ;; Make a tools/list request with a known ID
        (let* ((request (mcp-test--tools-list-request 100))
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
    (mcp-stop)
    (setq mcp-log-io nil)))

(ert-deftest mcp-test-log-io-nil ()
  "Test that when `mcp-log-io' is nil, JSON-RPC messages are not logged."
  (setq mcp-log-io nil)
  (mcp-start)

  (unwind-protect
      (progn
        (let ((request (mcp-test--tools-list-request 101)))
          (mcp-process-jsonrpc request)
          (should-not (get-buffer "*mcp-log*"))))

    (mcp-stop)))

(ert-deftest mcp-test-server-restart-preserves-tools ()
  "Test that server restart preserves registered tools."
  ;; Register a tool
  (mcp-start)
  (mcp-register-tool
   #'mcp-test--tool-handler
   :id "persistent-tool"
   :description "Test persistence across restarts")

  (unwind-protect
      (progn
        ;; Stop the server
        (mcp-stop)

        ;; Start server again
        (mcp-start)

        ;; Tool should be accessible via API
        (let* ((list-request
                (json-encode
                 `(("jsonrpc" . "2.0")
                   ("method" . "tools/list")
                   ("id" . 1000))))
               (list-response (mcp-process-jsonrpc list-request))
               (list-obj (json-read-from-string list-response))
               (tools (alist-get 'tools (alist-get 'result list-obj))))
          (should (= 1 (length tools)))
          (should
           (string= "persistent-tool" (alist-get 'name (aref tools 0))))))

    ;; Cleanup
    (mcp-unregister-tool "persistent-tool")
    (when mcp--running
      (mcp-stop))))

(ert-deftest mcp-test-parse-error ()
  "Test that invalid JSON input to `mcp-process-jsonrpc` returns a parse error."
  ;; Start the MCP server
  (mcp-start)
  (unwind-protect
      (progn
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
          (should (string-match "Parse error" (alist-get 'message error-obj)))))
    ;; Cleanup - always stop server
    (mcp-stop)))

(defun mcp-test--verify-jsonrpc-error
    (request-object expected-code expected-message)
  "Test that JSON-RPC REQUEST-OBJECT is rejected with expected error.
EXPECTED-CODE is the expected error code.
EXPECTED-MESSAGE is a regex pattern to match against the error message."
  (mcp-start)
  (unwind-protect
      (progn
        (let* ((request (json-encode request-object))
               (response (mcp-process-jsonrpc request))
               (response-obj (json-read-from-string response))
               (error-obj (alist-get 'error response-obj)))
          (should error-obj)
          (should (alist-get 'code error-obj))
          (should (= (alist-get 'code error-obj) expected-code))
          (should (alist-get 'message error-obj))
          (should
           (string-match expected-message (alist-get 'message error-obj)))))
    (mcp-stop)))

(ert-deftest mcp-test-invalid-jsonrpc ()
  "Test that valid JSON that is not JSON-RPC returns an invalid request error."
  (mcp-test--verify-jsonrpc-error
   '(("name" . "Test Object") ("value" . 42))
   -32600
   "Invalid Request: Not JSON-RPC 2.0"))

(defun mcp-test--test-invalid-jsonrpc-version (version)
  "Test that JSON-RPC request with VERSION is rejected properly."
  (mcp-test--verify-jsonrpc-error
   `(("jsonrpc" . ,version)
     ("method" . "tools/list")
     ("id" . 42))
   -32600 "Invalid Request: Not JSON-RPC 2.0"))

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

(ert-deftest mcp-test-tools-list-with-extra-key ()
  "Test that tools/list request with an extra, unexpected key works correctly.
Per JSON-RPC 2.0 spec, servers should ignore extra/unknown members."
  (mcp-start)
  (unwind-protect
      (progn
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
          (should (arrayp (alist-get 'tools result)))))
    (mcp-stop)))

(ert-deftest mcp-test-schema-for-bytecode-handler ()
  "Test schema generation for a handler loaded as bytecode.
This test verifies that MCP can correctly extract parameter information
from a function loaded from bytecode rather than interpreted elisp."
  (let* ((source-file (expand-file-name "mcp-test-bytecode-handler.el"))
         (bytecode-file (expand-file-name "mcp-test-bytecode-handler.elc")))
    (should (file-exists-p source-file))
    (byte-compile-file source-file)

    (let ((load-prefer-newer nil))
      (load bytecode-file nil t))

    (mcp-start)
    (unwind-protect
        (progn
          ;; Suppress byte-compiler warning about unknown function
          (with-no-warnings
            (mcp-register-tool
             #'mcp-test-bytecode-handler--handler
             :id "bytecode-handler"
             :description "A tool with a handler loaded from bytecode"))

          (let* ((list-req
                  (json-encode
                   `(("jsonrpc" . "2.0")
                     ("method" . "tools/list")
                     ("id" . 123))))
                 (list-response (mcp-process-jsonrpc list-req))
                 (list-obj (json-read-from-string list-response))
                 (tool-list (alist-get 'tools (alist-get 'result list-obj)))
                 (tool (aref tool-list 0))
                 (schema (alist-get 'inputSchema tool)))

            (should (equal "object" (alist-get 'type schema)))
            (should (alist-get 'properties schema))
            (should (equal ["input-string"] (alist-get 'required schema)))

            (let ((param-schema
                   (alist-get 'input-string (alist-get 'properties schema))))
              (should param-schema)
              (should (equal "string" (alist-get 'type param-schema)))
              (should
               (equal
                "Input string parameter for bytecode testing"
                (alist-get 'description param-schema))))))

      (mcp-stop)
      (mcp-unregister-tool "bytecode-handler")

      (when (file-exists-p bytecode-file)
        (delete-file bytecode-file)))))

(ert-deftest mcp-test-interactive-commands ()
  "Test that `mcp-start' and `mcp-stop' are interactive commands."
  (should (commandp #'mcp-start))
  (should (commandp #'mcp-stop)))

(ert-deftest mcp-test-tools-list-with-read-only-hint ()
  "Test that tools/list includes readOnlyHint=true in response."
  (mcp-start)
  (unwind-protect
      (progn
        ;; Register tool with readOnlyHint annotation set to true
        (mcp-register-tool
         #'mcp-test--tool-handler
         :id "read-only-tool"
         :description "A tool that doesn't modify its environment"
         :read-only t)

        (let ((response
               (mcp-process-jsonrpc (mcp-test--tools-list-request 15))))
          (mcp-test--verify-tool-list-response
           response
           '(("read-only-tool" .
              ((description . "A tool that doesn't modify its environment")
               (annotations . ((readOnlyHint . t)))
               (inputSchema . ((type . "object")))))))))
    (mcp-stop)
    (mcp-unregister-tool "read-only-tool")))

(ert-deftest mcp-test-tools-list-with-read-only-hint-false ()
  "Test that tools/list includes readOnlyHint=false in response."
  (mcp-start)
  (unwind-protect
      (progn
        ;; Register tool with readOnlyHint annotation set to false
        (mcp-register-tool
         #'mcp-test--tool-handler
         :id "non-read-only-tool"
         :description "Tool that modifies its environment"
         :read-only nil)

        (let ((response
               (mcp-process-jsonrpc (mcp-test--tools-list-request 16))))
          (mcp-test--verify-tool-list-response
           response
           '(("non-read-only-tool" .
              ((description . "Tool that modifies its environment")
               (annotations . ((readOnlyHint . :json-false)))
               (inputSchema . ((type . "object")))))))))
    (mcp-stop)
    (mcp-unregister-tool "non-read-only-tool")))

(ert-deftest mcp-test-tools-list-with-multiple-annotations ()
  "Test that tools/list handles multiple annotations in response."
  (mcp-start)
  (unwind-protect
      (progn
        ;; Register tool with multiple annotations
        (mcp-register-tool
         #'mcp-test--tool-handler
         :id "multi-annotated-tool"
         :description "A tool with multiple annotations"
         :title "Friendly Multi-Tool"
         :read-only t)

        (let ((response
               (mcp-process-jsonrpc (mcp-test--tools-list-request 17))))
          (mcp-test--verify-tool-list-response
           response
           '(("multi-annotated-tool" .
              ((description . "A tool with multiple annotations")
               (annotations
                . ((title . "Friendly Multi-Tool") (readOnlyHint . t)))
               (inputSchema . ((type . "object")))))))))
    (mcp-stop)
    (mcp-unregister-tool "multi-annotated-tool")))

(provide 'mcp-test)
;;; mcp-test.el ends here
