# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Commands

- Run all quality checks:

  ```shell
  ./check.sh
  ```

- Run single test:

  ```shell
  emacs -Q -batch -l mcp.el -l mcp-test.el \
        --eval "(ert-run-tests-batch-and-exit \"test-name\")"
  ```

- Format YAML files: `prettier --write .github/workflows/*.yml`

## Development Workflow

**EXTREMELY IMPORTANT: Strictly follow this workflow for all changes.**
**If anything prevents following these steps, STOP and ask for guidance.**

**Task Focus: Never switch tasks mid-development.**
- When troubleshooting, explore multiple approaches methodically
- Document alternative approaches considered before choosing a direction
- If multiple solutions exist, discuss trade-offs with the user before proceeding
- Focus on a single solution after deciding on an approach
**If you discover additional work needed, ask for permission to file a todo note.**

**Commands: Do not modify the commands in this file.**
**If commands don't work or need modification, ask for explicit permission.**

**IMPORTANT: Never use direct byte-compilation commands.**
**Always use check.sh which runs elisp-lint and includes byte-compilation checks.**

### Step 1: Understand the feature

Before writing any code:

- Understand the feature requirements and specifications
- Review related code and documentation (e.g., `TODO.org`)
- Ask questions to clarify any ambiguities
- When troubleshooting existing failures, first fully understand the failing test
- Use debugging tools to isolate the exact cause before implementing a solution
- Consider multiple approaches, but document your reasoning before implementing

### Step 2: Test-Driven Development

1. **Write tests**: Create tests that verify the expected behavior

- Test only public interfaces, not implementation details
- Do not depend on internals of the tested component
- If you see no alternative to testing internals, ask for guidance

1. **Run linters**: Run check.sh

- Note: Some errors (e.g., undefined symbols) are expected at this stage
- Fix style and formatting issues only

1. **Verify failure**: Run tests to confirm they fail as expected
1. **Implement**: Write the minimal code needed to make tests pass
1. **Verify success**: Run tests to confirm implementation works
1. **Run all quality checks**:

- Run `./check.sh` to verify all quality checks pass
- It includes elisp-lint, tests, and documentation linting

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
  - Avoid: `# Get the response` above `response = get_response()`
  - Prefer: Comments that explain "why" not "what" when the code isn't self-explanatory
  - Use comments to explain complex algorithms, non-obvious side effects, or historical context
  - Descriptive variable and function names often eliminate the need for comments

## Project Roadmap

For implementation details and roadmap, refer to the `TODO.org` file which contains:

- Implementation decisions (JSON-RPC, transport layer, etc.)
- Python SDK parity goals
- Transport implementation plans
- Testing requirements
- Future features
