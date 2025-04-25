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

(defun mcp-test--tool-handler (request-context _tool-args)
  "Test tool handler function for MCP tool testing.
REQUEST-CONTEXT is the context from which to respond.
_TOOL-ARGS are the arguments passed to the tool (unused)."
  (mcp-respond-with-result request-context '((result . "test result"))))

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
        ;; Test server description
        (let* ((describe-request
                (json-encode
                 `(("jsonrpc" . "2.0")
                   ("method" . "mcp.server.describe")
                   ("id" . 1))))
               (describe-response (mcp-process-jsonrpc describe-request))
               (describe-result
                (alist-get 'result
                           (json-read-from-string describe-response))))
          (should (stringp (alist-get 'name describe-result)))
          (should (stringp (alist-get 'version describe-result)))
          (should (stringp (alist-get 'protocol_version describe-result)))
          (should (arrayp (alist-get 'capabilities describe-result))))

        ;; Test listing tools
        (let* ((list-request
                (json-encode
                 `(("jsonrpc" . "2.0")
                   ("method" . "mcp.server.list_tools")
                   ("id" . 2))))
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
        (let* ((request (json-encode
                         `(("jsonrpc" . "2.0")
                           ("method" . "tools/list")
                           ("id" . 5))))
               (response (mcp-process-jsonrpc request))
               (response-obj (json-read-from-string response))
               (result (alist-get 'result response-obj)))
          
          (should result)
          (should (alist-get 'tools result))
          (should (arrayp (alist-get 'tools result)))
          (should (= 0 (length (alist-get 'tools result))))))
    (mcp-stop)))

(ert-deftest mcp-test-tools-list-one ()
  "Test the `tools/list` method returns one tool with correct fields."
  (mcp-start)
  (unwind-protect
      (progn
        (mcp-register-tool "test-tool" "A tool for testing" #'mcp-test--tool-handler)
        
        (let* ((request (json-encode
                         `(("jsonrpc" . "2.0")
                           ("method" . "tools/list")
                           ("id" . 6))))
               (response (mcp-process-jsonrpc request))
               (response-obj (json-read-from-string response))
               (result (alist-get 'result response-obj))
               (tools (alist-get 'tools result)))
          
          (should result)
          (should tools)
          (should (arrayp tools))
          (should (= 1 (length tools)))
          
          (let ((tool (aref tools 0)))
            (should (string= "test-tool" (alist-get 'name tool)))
            (should (string= "A tool for testing" (alist-get 'description tool)))
            (should (alist-get 'inputSchema tool nil t)))))
    (mcp-stop)
    (mcp-unregister-tool "test-tool")))

(ert-deftest mcp-test-tools-list-two ()
  "Test the `tools/list` method returns multiple tools with correct fields."
  (mcp-start)
  (unwind-protect
      (progn
        (mcp-register-tool "test-tool-1" "First tool for testing" #'mcp-test--tool-handler)
        (mcp-register-tool "test-tool-2" "Second tool for testing" #'mcp-test--tool-handler)
        
        (let* ((request (json-encode
                         `(("jsonrpc" . "2.0")
                           ("method" . "tools/list")
                           ("id" . 7))))
               (response (mcp-process-jsonrpc request))
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
            (should (string= "First tool for testing" (alist-get 'description found-tool-1)))
            (should (string= "Second tool for testing" (alist-get 'description found-tool-2)))
            (should (alist-get 'inputSchema found-tool-1 nil t))
            (should (alist-get 'inputSchema found-tool-2 nil t)))))
    (mcp-stop)
    (mcp-unregister-tool "test-tool-1")
    (mcp-unregister-tool "test-tool-2")))

(ert-deftest mcp-test-unregister-tool ()
  "Test that mcp-unregister-tool removes a tool correctly."
  (let ((tools-before (hash-table-count mcp--tools)))
    (mcp-register-tool "test-unregister" "Tool for unregister test" #'mcp-test--tool-handler)
    (should (= (1+ tools-before) (hash-table-count mcp--tools)))
    (should (gethash "test-unregister" mcp--tools))
    
    (should (mcp-unregister-tool "test-unregister"))
    (should-not (gethash "test-unregister" mcp--tools))
    (should (= tools-before (hash-table-count mcp--tools)))))

(ert-deftest mcp-test-unregister-nonexistent-tool ()
  "Test that mcp-unregister-tool returns nil for nonexistent tools."
  (mcp-register-tool "test-other" "Other test tool" #'mcp-test--tool-handler)
  (should-not (mcp-unregister-tool "nonexistent-tool"))
  (mcp-unregister-tool "test-other"))

(ert-deftest mcp-test-unregister-when-no-tools ()
  "Test that mcp-unregister-tool works when no tools are registered."
  (should-not (mcp-unregister-tool "any-tool")))

(provide 'mcp-test)
;;; mcp-test.el ends here