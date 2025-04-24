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
(require 'url)

;; Test configuration
(defvar mcp--test-port 8000
  "Port used for MCP server during tests.
Matches the default port from mcp-default-port.")

;;; Server Tests

(ert-deftest mcp-test-minimal-server ()
  "Test creating, starting, and stopping a minimal MCP server.
Tests the basic server lifecycle with no tools or resources."
  ;; Create a test server
  (let ((server (mcp-create-server "test-server")))
    ;; Verify server creation
    (should server)

    ;; Start the server
    (mcp-start-server server)

    ;; Verify server is running
    ;; We'll use a simple HTTP request to check server status
    (with-temp-buffer
      (let ((url-request-method "POST")
            (url-request-data (json-encode
                               `(("jsonrpc" . "2.0")
                                 ("method" . "mcp.server.status")
                                 ("id" . 1))))
            (url-request-extra-headers '(("Content-Type" . "application/json"))))
        (url-insert-file-contents (format "http://localhost:%d/mcp" mcp--test-port))
        (let* ((response (json-read-from-string (buffer-string)))
               (result (alist-get 'result response)))
          ;; Check if server responded with status info
          (should (alist-get 'name result))
          (should (equal (alist-get 'name result) "test-server"))
          (should (alist-get 'version result)))))

    ;; Stop the server
    (mcp-stop-server server)

    ;; Verify server is stopped (request should fail)
    (should-error
     (with-temp-buffer
       (let ((url-request-method "POST")
             (url-request-data (json-encode
                                `(("jsonrpc" . "2.0")
                                  ("method" . "mcp.server.status")
                                  ("id" . 2))))
             (url-request-extra-headers '(("Content-Type" . "application/json"))))
         (url-insert-file-contents (format "http://localhost:%d/mcp" mcp--test-port)))))))

;;; Resource Tests

;;; Tool Tests

;;; Prompt Tests

;;; Transport Tests

;;; Message Handling Tests

(ert-deftest mcp-test-protocol-methods ()
  "Test standard MCP protocol methods with a minimal server."
  (let ((server (mcp-create-server "test-server")))
    (mcp-start-server server)
    (unwind-protect
        (progn
          ;; Test mcp.server.describe method
          (with-temp-buffer
            (let ((url-request-method "POST")
                  (url-request-data (json-encode
                                     `(("jsonrpc" . "2.0")
                                       ("method" . "mcp.server.describe")
                                       ("id" . 1))))
                  (url-request-extra-headers '(("Content-Type" . "application/json"))))
              (url-insert-file-contents (format "http://localhost:%d/mcp" mcp--test-port))
              (let* ((response (json-read-from-string (buffer-string)))
                     (result (alist-get 'result response)))
                ;; Server should return description with capabilities
                (should (alist-get 'name result))
                (should (alist-get 'version result))
                (should (alist-get 'protocol_version result))
                (should (alist-get 'capabilities result)))))

          ;; Test mcp.server.list_tools method
          (with-temp-buffer
            (let ((url-request-method "POST")
                  (url-request-data (json-encode
                                     `(("jsonrpc" . "2.0")
                                       ("method" . "mcp.server.list_tools")
                                       ("id" . 2))))
                  (url-request-extra-headers '(("Content-Type" . "application/json"))))
              (url-insert-file-contents (format "http://localhost:%d/mcp" mcp--test-port))
              (let* ((response (json-read-from-string (buffer-string)))
                     (result (alist-get 'result response)))
                ;; Should return empty tools array for minimal server
                (should (arrayp (alist-get 'tools result)))
                (should (= 0 (length (alist-get 'tools result))))))))

      ;; Cleanup - always stop server
      (mcp-stop-server server))))

(provide 'mcp-test)
;;; mcp-test.el ends here