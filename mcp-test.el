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
  (json-encode
   `(("jsonrpc" . "2.0")
     ("method" . "tools/list")
     ("id" . ,id))))

(ert-deftest mcp-test-tool-registration-in-capabilities ()
  "Test registered tool appears in server capabilities."
  (mcp-register-tool "test-tool" "A tool for testing" #'mcp-test--tool-handler)
  (mcp-start)
  (unwind-protect
      (progn
        (let* ((initialize-request
                (json-encode
                 `(("jsonrpc" . "2.0")
                   ("method" . "initialize")
                   ("id" . 1)
                   ("params" .
                    (("protocolVersion" . "2024-11-05")
                     ("capabilities" .
                      (("tools" . t))))))))
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
                 `(("jsonrpc" . "2.0")
                   ("method" . "tools/list")
                   ("id" . 1))))
               (list-response (mcp-process-jsonrpc list-request))
               (list-result
                (alist-get 'result
                           (json-read-from-string list-response))))
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
                   ("method" . "initialize")
                   ("id" . 3)
                   ("params" .
                    (("protocolVersion" . "2024-11-05")
                     ("capabilities" .
                      (("tools" . t)
                       ("resources" . nil)
                       ("prompts" . nil))))))))
               (initialize-response (mcp-process-jsonrpc initialize-request))
               (initialize-result
                (alist-get 'result
                           (json-read-from-string initialize-response))))
          ;; Verify the server responded with its protocol version
          (should (stringp (alist-get 'protocolVersion initialize-result)))
          (should (string= "2024-11-05"
                           (alist-get 'protocolVersion initialize-result)))
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
          (let ((server-name (alist-get 'name
                                        (alist-get 'serverInfo
                                                   initialize-result))))
            (should (string= mcp--name server-name)))))
    ;; Cleanup - always stop server
    (mcp-stop)))

(ert-deftest mcp-test-initialized-notification ()
  "Test the MCP initialized notification handling."
  ;; Start the MCP server using the singleton API
  (mcp-start)
  (unwind-protect
      (progn
        ;; First send an initialize request
        (let ((initialize-request
               (json-encode
                `(("jsonrpc" . "2.0")
                  ("method" . "initialize")
                  ("id" . 4)
                  ("params" .
                   (("protocolVersion" . "2024-11-05")
                    ("capabilities" . (("tools" . t)))))))))
          (mcp-process-jsonrpc initialize-request))
        ;; Test notification (no response expected for notifications)
        (let* ((initialized-notification
                (json-encode
                 `(("jsonrpc" . "2.0")
                   ("method" . "initialized")
                   ("params" . (("clientName" . "Test Client"))))))
               (notification-response
                (mcp-process-jsonrpc initialized-notification)))
          ;; For notifications, a valid empty response is expected
          (should (string= "{\"jsonrpc\":\"2.0\",\"id\":null,\"result\":null}"
                           notification-response))))
    ;; Cleanup - always stop server
    (mcp-stop)))

(ert-deftest mcp-test-notifications-initialized-format ()
  "Test the MCP notifications/initialized format handling."
  ;; Start the MCP server using the singleton API
  (mcp-start)
  (unwind-protect
      (progn
        ;; Test notifications/initialized format
        (let* ((notifications-initialized
                (json-encode
                 `(("jsonrpc" . "2.0")
                   ("method" . "notifications/initialized")
                   ("id" . null))))
               (response (mcp-process-jsonrpc notifications-initialized)))
          ;; For true notifications, the server should return an empty string
          ;; or something that indicates no response is needed
          (should (string= "" response))))
    ;; Cleanup - always stop server
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
         "failing-tool" "A tool that always fails"
         #'mcp-test--failing-tool-handler)
        (let* ((response (mcp-process-jsonrpc
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
            (should (string= "This tool intentionally fails"
                             (alist-get 'text content-item))))
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
         "generic-error-tool" "A tool that throws a generic error"
         #'mcp-test--generic-error-handler)
        (let* ((response (mcp-process-jsonrpc
                          (mcp-test--tools-call-request
                           12 "generic-error-tool")))
               (response-obj (json-read-from-string response))
               ;; Should have error field at the top level of the response
               (error-obj (alist-get 'error response-obj)))
          ;; Check it has a standard JSON-RPC error
          (should error-obj)
          (should (alist-get 'code error-obj))
          (should (= -32603 (alist-get 'code error-obj)))
          (should (alist-get 'message error-obj))
          (should (string-match "Internal error executing tool"
                                (alist-get 'message error-obj)))))
    (mcp-stop)
    (mcp-unregister-tool "generic-error-tool")))

(ert-deftest mcp-test-tools-list-one ()
  "Test tools/list returns one tool with correct fields and schema."
  (mcp-start)
  (unwind-protect
      (progn
        ;; Register zero-arg handler tool
        (mcp-register-tool
         "test-tool" "A tool for testing" #'mcp-test--tool-handler)

        (let* ((response (mcp-process-jsonrpc (mcp-test--tools-list-request 6)))
               (response-obj (json-read-from-string response))
               (result (alist-get 'result response-obj))
               (tools (alist-get 'tools result)))
          (should result)
          (should tools)
          (should (arrayp tools))
          (should (= 1 (length tools)))
          (let ((tool (aref tools 0)))
            (should (string= "test-tool" (alist-get 'name tool)))
            (should (string= "A tool for testing"
                             (alist-get 'description tool)))

            ;; Check schema derived from zero-arg function
            (let ((schema (alist-get 'inputSchema tool)))
              (should schema)
              (should (equal "object" (alist-get 'type schema)))
              (should-not (alist-get 'required schema))))))
    (mcp-stop)
    (mcp-unregister-tool "test-tool")))

(ert-deftest mcp-test-tools-list-two ()
  "Test the `tools/list` method returns multiple tools with correct fields."
  (mcp-start)
  (unwind-protect
      (progn
        (mcp-register-tool "test-tool-1" "First tool for testing"
                           #'mcp-test--tool-handler)
        (mcp-register-tool "test-tool-2" "Second tool for testing"
                           #'mcp-test--tool-handler)
        (let* ((response (mcp-process-jsonrpc (mcp-test--tools-list-request 7)))
               (response-obj (json-read-from-string response))
               (result (alist-get 'result response-obj))
               (tools (alist-get 'tools result)))
          (should result)
          (should tools)
          (should (arrayp tools))
          (should (= 2 (length tools)))
          (let ((found-tool-1 nil)
                (found-tool-2 nil))
            (dotimes (i 2)
              (let ((tool (aref tools i)))
                (when (string= "test-tool-1" (alist-get 'name tool))
                  (setq found-tool-1 tool))
                (when (string= "test-tool-2" (alist-get 'name tool))
                  (setq found-tool-2 tool))))
            (should found-tool-1)
            (should found-tool-2)
            (should (string= "First tool for testing"
                             (alist-get 'description found-tool-1)))
            (should (string= "Second tool for testing"
                             (alist-get 'description found-tool-2)))
            (should (alist-get 'inputSchema found-tool-1 nil t))
            (should (alist-get 'inputSchema found-tool-2 nil t)))))
    (mcp-stop)
    (mcp-unregister-tool "test-tool-1")
    (mcp-unregister-tool "test-tool-2")))

(ert-deftest mcp-test-unregister-tool ()
  "Test that `mcp-unregister-tool' removes a tool correctly."
  (let ((tools-before (hash-table-count mcp--tools)))
    (mcp-register-tool "test-unregister" "Tool for unregister test"
                       #'mcp-test--tool-handler)
    (should (= (1+ tools-before) (hash-table-count mcp--tools)))
    (should (gethash "test-unregister" mcp--tools))
    (should (mcp-unregister-tool "test-unregister"))
    (should-not (gethash "test-unregister" mcp--tools))
    (should (= tools-before (hash-table-count mcp--tools)))))

(ert-deftest mcp-test-unregister-nonexistent-tool ()
  "Test that `mcp-unregister-tool' returns nil for nonexistent tools."
  (mcp-register-tool "test-other" "Other test tool" #'mcp-test--tool-handler)
  (should-not (mcp-unregister-tool "nonexistent-tool"))
  (mcp-unregister-tool "test-other"))

(ert-deftest mcp-test-unregister-when-no-tools ()
  "Test that `mcp-unregister-tool' works when no tools are registered."
  (should-not (mcp-unregister-tool "any-tool")))

;;; Prompts Tests

(defun mcp-test--prompts-list-request (id)
  "Create a prompt list JSON-RPC request with ID."
  (json-encode
   `(("jsonrpc" . "2.0")
     ("method" . "prompts/list")
     ("id" . ,id))))

(ert-deftest mcp-test-prompts-list-zero ()
  "Test the `prompts/list` method returns empty array with no prompts."
  (mcp-start)
  (unwind-protect
      (progn
        (let* ((response (mcp-process-jsonrpc
                          (mcp-test--prompts-list-request 8)))
               (response-obj (json-read-from-string response))
               (result (alist-get 'result response-obj)))
          (should result)
          (should (alist-get 'prompts result))
          (should (arrayp (alist-get 'prompts result)))
          (should (= 0 (length (alist-get 'prompts result))))))
    (mcp-stop)))

;;; Tool Call Tests

(defconst mcp-test--string-list-result
  "item1 item2 item3"
  "Test data for string list tool.")

(defun mcp-test--string-list-tool-handler ()
  "Test tool handler function to return a string with items."
  mcp-test--string-list-result)

(defun mcp-test--empty-array-tool-handler ()
  "Test tool handler function to return an empty string."
  "")

(defun mcp-test--string-arg-tool-handler (input-string)
  "Test tool handler that accepts a string argument.
INPUT-STRING is the string argument passed to the tool."
  (concat "Echo: " input-string))

(defun mcp-test--tools-call-request (id tool-name &optional args)
  "Create a tools/call JSON-RPC request with ID for TOOL-NAME.
Optional ARGS is an association list of tool arguments."
  (json-encode
   `(("jsonrpc" . "2.0")
     ("method" . "tools/call")
     ("id" . ,id)
     ("params" . (("name" . ,tool-name)
                  ("arguments" . ,(or args '())))))))

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
         "string-list-tool" "A tool that returns a string with items"
         #'mcp-test--string-list-tool-handler)
        (let* ((response (mcp-process-jsonrpc
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
        (mcp-register-tool
         "empty-string-tool" "A tool that returns an empty string"
         #'mcp-test--empty-array-tool-handler)
        (let* ((req (mcp-test--tools-call-request 10 "empty-string-tool"))
               (response (mcp-process-jsonrpc req))
               (response-obj (json-read-from-string response))
               (result (alist-get 'result response-obj)))
          (should result)
          (mcp-test--check-mcp-content-format result "")))
    (mcp-stop)
    (mcp-unregister-tool "empty-string-tool")))

(ert-deftest mcp-test-schema-for-one-arg-handler ()
  "Test that schema for one-arg handler shows required parameter."
  (mcp-start)
  (unwind-protect
      (progn
        ;; Register a tool with a handler that requires one argument
        (mcp-register-tool
         "requires-arg" "A tool that requires an argument"
         #'mcp-test--string-arg-tool-handler)

        ;; Get schema via tools/list
        (let* ((list-req (json-encode
                          `(("jsonrpc" . "2.0")
                            ("method" . "tools/list")
                            ("id" . 42))))
               (list-response (mcp-process-jsonrpc list-req))
               (list-obj (json-read-from-string list-response))
               (tool-list (alist-get 'tools (alist-get 'result list-obj)))
               (tool (aref tool-list 0))
               (schema (alist-get 'inputSchema tool)))

          ;; Schema should indicate required parameter
          (should schema)
          (should (equal "object" (alist-get 'type schema)))
          (should (alist-get 'properties schema))
          (should (alist-get 'required schema))
          (should (vectorp (alist-get 'required schema)))
          (should (= 1 (length (alist-get 'required schema))))))
    (mcp-stop)
    (mcp-unregister-tool "requires-arg")))

(ert-deftest mcp-test-tools-call-with-string-arg ()
  "Test the `tools/call` method with a tool that takes a string argument."
  (mcp-start)
  (unwind-protect
      (progn
        (mcp-register-tool
         "string-arg-tool" "A tool that echoes a string argument"
         #'mcp-test--string-arg-tool-handler)
        (let* ((test-input "Hello, world!")
               (args `(("input" . ,test-input)))
               (req (mcp-test--tools-call-request 13 "string-arg-tool" args))
               (response (mcp-process-jsonrpc req))
               (response-obj (json-read-from-string response))
               (result (alist-get 'result response-obj)))
          (should result)
          (mcp-test--check-mcp-content-format
           result
           (concat "Echo: " test-input))))
    (mcp-stop)
    (mcp-unregister-tool "string-arg-tool")))

(provide 'mcp-test)
;;; mcp-test.el ends here