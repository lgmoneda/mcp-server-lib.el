;;; mcp-server-lib-commands.el --- User commands for MCP server -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; This file is part of mcp-server-lib.el.

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

;; This file provides interactive user commands for the MCP server library.

;;; Code:

(require 'mcp-server-lib)

;;;###autoload
(defun mcp-server-lib-start ()
  "Start the MCP server and begin handling client requests.

This function starts the MCP server that can process JSON-RPC
requests via `mcp-server-lib-process-jsonrpc'.  Once started, the server
will dispatch incoming requests to the appropriate tool
handlers that have been registered with `mcp-server-lib-register-tool'."
  (interactive)
  (when mcp-server-lib--running
    (error "MCP server is already running"))

  (when (called-interactively-p 'any)
    (message "Emacs starting handling MCP requests"))
  (setq mcp-server-lib--running t))

;;;###autoload
(defun mcp-server-lib-stop ()
  "Stop the MCP server from processing client requests.

Sets the server state to stopped, which prevents further processing of
client requests.  Note that this does not release any resources or unregister
tools, it simply prevents `mcp-server-lib-process-jsonrpc' from accepting new
requests."
  (interactive)
  (unless mcp-server-lib--running
    (error "MCP server is not running"))

  (when (called-interactively-p 'any)
    (message "Emacs stopping handling MCP requests"))
  ;; Mark server as not running
  (setq mcp-server-lib--running nil)
  t)

(provide 'mcp-server-lib-commands)

;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:

;;; mcp-server-lib-commands.el ends here
