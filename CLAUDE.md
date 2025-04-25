# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Commands

- Run all tests:

  ```shell
  emacs -Q -batch -l mcp.el -l mcp-test.el --eval "(ert-run-tests-batch-and-exit)"
  ```

- Run single test:

  ```shell
  emacs -Q -batch -l mcp.el -l mcp-test.el \
        --eval "(ert-run-tests-batch-and-exit \"test-name\")"
  ```

- Byte-compile:

  ```shell
  emacs -Q -batch --eval "(byte-compile-file \"mcp.el\")"
  ```

- Run elisp-lint:

  ```shell
  emacs -batch --eval "(let ((pkg-dirs (list (locate-user-emacs-file \"elpa/elisp-lint-20220419.252\")
                                          (locate-user-emacs-file \"elpa/package-lint-0.26\")
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
- Check Org files:

  ```shell
  emacs -Q --batch --eval "(require 'org)" --eval "(require 'org-lint)" \
        --eval "(with-temp-buffer (insert-file-contents \"FILE.org\") \
               (org-mode) (let ((results (org-lint))) \
               (if results (message \"Found issues: %S\" results) \
               (message \"No issues found\"))))"
  ```

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
