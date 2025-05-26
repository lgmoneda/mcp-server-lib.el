;;; mcp-server-lib-commands.el --- User commands for MCP server -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; Author: Laurynas Biveinis <laurynas.biveinis@gmail.com>
;; Keywords: comm, tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (mcp-server-lib "0.1.0"))
;; URL: https://github.com/laurynas-biveinis/mcp-server-lib

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

;; This file provides short, convenient command names for interactive use.
;; The commands intentionally don't follow the package prefix convention
;; to make them easier to type interactively.

;;; Code:

(require 'mcp-server-lib)

;;;###autoload
(defun mcp-start ()
  "Start the MCP server and begin handling client requests.
This is a convenience alias for `mcp-server-lib-start'."
  (interactive)
  (mcp-server-lib-start))

;;;###autoload
(defun mcp-stop ()
  "Stop the MCP server from processing client requests.
This is a convenience alias for `mcp-server-lib-stop'."
  (interactive)
  (mcp-server-lib-stop))

(provide 'mcp-server-lib-commands)

;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; package-lint-main-file: nil
;; End:

;;; mcp-server-lib-commands.el ends here
