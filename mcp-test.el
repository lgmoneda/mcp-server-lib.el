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
  ;; Register a test tool
  (mcp-register-tool "test-tool" "A tool for testing" #'mcp-test--tool-handler)

  ;; Start the MCP server
  (mcp-start)
  (unwind-protect
      (progn
        ;; Send initialize request
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

          ;; There should be a result
          (should (alist-get 'result response-obj))

          (let* ((result (alist-get 'result response-obj))
                 (capabilities (alist-get 'capabilities result))
                 (tools-capability (alist-get 'tools capabilities)))

            ;; The tools capability should exist
            (should tools-capability)

            ;; It should have a listChanged property set to true
            (should (alist-get 'listChanged tools-capability))
            (should (eq t (alist-get 'listChanged tools-capability))))))

    ;; Cleanup - always stop server
    (mcp-stop)))

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

(provide 'mcp-test)
;;; mcp-test.el ends here