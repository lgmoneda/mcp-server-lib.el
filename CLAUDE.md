# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Commands

- Run all tests:

  ```
  emacs -batch --eval "(let ((pkg-dir (locate-user-emacs-file \"elpa/simple-httpd-1.5.1\")))
                         (add-to-list 'load-path pkg-dir)
                         (add-to-list 'load-path \".\")
                         (load-file \"mcp.el\")
                         (load-file \"mcp-test.el\")
                         (ert-run-tests-batch-and-exit t))"
  ```

- Run single test:

  ```
  emacs -batch --eval "(let ((pkg-dir (locate-user-emacs-file \"elpa/simple-httpd-1.5.1\")))
                         (add-to-list 'load-path pkg-dir)
                         (add-to-list 'load-path \".\")
                         (load-file \"mcp.el\")
                         (load-file \"mcp-test.el\")
                         (ert-run-tests-batch-and-exit \"test-name\"))"
  ```

- Byte-compile:

  ```
  emacs -batch --eval "(let ((pkg-dir (locate-user-emacs-file \"elpa/simple-httpd-1.5.1\")))
                         (add-to-list 'load-path pkg-dir)
                         (add-to-list 'load-path \".\")
                         (byte-compile-file \"mcp.el\"))"
  ```

- Run elisp-lint:

  ```
  emacs -batch --eval "(let ((pkg-dirs (list (locate-user-emacs-file \"elpa/elisp-lint-20220419.252\")
                                          (locate-user-emacs-file \"elpa/package-lint-0.26\")
                                          (locate-user-emacs-file \"elpa/simple-httpd-1.5.1\")
                                          (locate-user-emacs-file \"elpa/dash-20250312.1307\")
                                          (expand-file-name \".\"))))
                         (dolist (dir pkg-dirs)
                           (add-to-list 'load-path dir))
                         (require 'elisp-lint)
                         (elisp-lint-file \"mcp.el\"))"
  ```

- Check GitHub workflows: `actionlint .github/workflows/*.yml`
- Check Markdown files: `mdl *.md`
- Format YAML files: `prettier --write .github/workflows/*.yml`
- Check YAML formatting: `prettier --check .github/workflows/*.yml`
- Check terminology: `textlint --rule terminology *.md`

## Standing Orders

After each code change:

- Run elisp-lint to catch potential issues
- Byte-compile the code to check for warnings
- Run all tests to ensure nothing was broken

## Style Guide

- **File Structure**: Follow Emacs package conventions with proper headers,
  Commentary, and Code sections
- **Naming**: Use `mcp-` prefix for public functions, `mcp--` for internal/private
  functions
- **Documentation**: Provide docstrings for all functions with Arguments and Examples
  sections
- **Quoting**: Use `\\='` for quoted symbols instead of single quotes
- **Function Parameters**: Prefix unused parameters with underscore (e.g., `_path`)
- **Whitespace**: Avoid trailing whitespace; two spaces after period in docstrings
- **Error Handling**: Use `condition-case` for handling errors in user-facing functions
- **Line Length**: Keep lines under 80 columns when possible

## Project Roadmap

For implementation details and roadmap, refer to the `TODO.org` file which contains:

- Implementation decisions (JSON-RPC, transport layer, etc.)
- Python SDK parity goals
- Transport implementation plans
- Testing requirements
- Future features
