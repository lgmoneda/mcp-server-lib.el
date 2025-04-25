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
- Check for code duplication: `jscpd -r consoleFull -t 0 .`
- Check Org files:

  ```shell
  emacs -Q --batch --eval "(require 'org)" --eval "(require 'org-lint)" \
        --eval "(with-temp-buffer (insert-file-contents \"FILE.org\") \
               (org-mode) (let ((results (org-lint))) \
               (if results (message \"Found issues: %S\" results) \
               (message \"No issues found\"))))"
  ```

## Development Workflow

**EXTREMELY IMPORTANT: Strictly follow this workflow for all changes.**
**If anything prevents following these steps, STOP and ask for guidance.**

**Task Focus: Never switch tasks mid-development.**
**If you discover additional work needed, ask for permission to file a todo note.**

**Commands: Do not modify the commands in this file.**
**If commands don't work or need modification, ask for explicit permission.**

**IMPORTANT: Never use direct byte-compilation commands.**
**Always use elisp-lint which already includes byte-compilation checks.**

### Step 1: Understand the feature

Before writing any code:

- Understand the feature requirements and specifications
- Review related code and documentation (e.g., `TODO.org`)
- Ask questions to clarify any ambiguities

### Step 2: Test-Driven Development

1. **Write tests**: Create tests that verify the expected behavior

- Test only public interfaces, not implementation details
- Do not depend on internals of the tested component
- If you see no alternative to testing internals, ask for guidance

1. **Run linters**: Run elisp-lint on test code

- Note: Some errors (e.g., undefined symbols) are expected at this stage
- Fix style and formatting issues only

1. **Verify failure**: Run tests to confirm they fail as expected
1. **Implement**: Write the minimal code needed to make tests pass
1. **Verify success**: Run tests to confirm implementation works
1. **Run all quality checks**:

- Run elisp-lint (which already includes byte-compilation checks)
- Run all tests
- Check documentation linting if applicable

1. **Refactor**: Clean up code while maintaining passing tests

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
- **Comments**: Do not add obvious comments that merely restate the code

## Project Roadmap

For implementation details and roadmap, refer to the `TODO.org` file which contains:

- Implementation decisions (JSON-RPC, transport layer, etc.)
- Python SDK parity goals
- Transport implementation plans
- Testing requirements
- Future features
