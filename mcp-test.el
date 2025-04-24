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

(provide 'mcp-test)
;;; mcp-test.el ends here