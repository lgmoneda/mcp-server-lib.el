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
                    (("protocolVersion" . "0.1.0")
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
          (should (string= "0.1.0"
                           (alist-get 'protocolVersion initialize-result)))
          ;; Verify server capabilities
          (should (alist-get 'capabilities initialize-result))
          (should (eq t (alist-get 'tools
                                   (alist-get 'capabilities
                                              initialize-result))))))
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
                   (("protocolVersion" . "0.1.0")
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

(ert-deftest mcp-test-initialize-version-compatibility ()
  "Test protocol version compatibility in initialize request."
  ;; Start the MCP server using the singleton API
  (mcp-start)
  (unwind-protect
      (progn
        ;; Test with incompatible version
        (let* ((initialize-request
                (json-encode
                 `(("jsonrpc" . "2.0")
                   ("method" . "initialize")
                   ("id" . 5)
                   ("params" .
                    (("protocolVersion" . "99.0.0")
                     ("capabilities" . (("tools" . t))))))))
               (initialize-response (mcp-process-jsonrpc initialize-request))
               (response-obj (json-read-from-string initialize-response)))
          ;; Should return an error for incompatible version
          (should (alist-get 'error response-obj))
          (should (eq -32602
                      (alist-get 'code (alist-get 'error response-obj))))))
    ;; Cleanup - always stop server
    (mcp-stop)))

(provide 'mcp-test)
;;; mcp-test.el ends here