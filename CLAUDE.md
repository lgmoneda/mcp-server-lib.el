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
  emacs -Q --batch -l mcp.el -l mcp-test.el \
        --eval "(ert-run-tests-batch-and-exit \"test-name\")"
  ```

- Format YAML files: `prettier --write .github/workflows/*.yml`

## Development Workflow

**EXTREMELY IMPORTANT: Strictly follow this workflow for all changes.**
**If anything prevents following these steps, STOP and ask for guidance.**

**Workflow Steps Summary:**

1. Understand requirements from `TODO.org` and existing code
1. Write tests first, run check.sh to establish baseline
1. Implement solution, running check.sh after each significant change
1. When finished, update documentation and mark items as complete in `TODO.org`
1. Verify all tests pass with check.sh before committing

**Key Rules:**

- **ALWAYS run ./check.sh before and after making changes**
- **NEVER skip running ./check.sh, even for "small" changes**
- **Fix all linting issues before proceeding to the next step**
- **Follow TDD: Write tests first, then implement the feature**

**Task Focus: Never switch tasks mid-development.**

- When troubleshooting, explore multiple approaches methodically
- Document alternative approaches considered before choosing a direction
- If multiple solutions exist, discuss trade-offs with the user before proceeding
- Focus on a single solution after deciding on an approach
- When implementing new validations, consider all edge cases
- Updates to user-facing documentation (README.org) should happen alongside
  code changes

**Error Handling and Validation:**

- Make error messages clear, specific, and actionable
- Consider all validation cases: missing inputs, invalid inputs, edge cases
- Validate early and provide meaningful errors instead of silently failing

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
- Consider both positive and negative test cases (error conditions)
- When adding new features, update existing tests if necessary

1. **Run linters with check.sh**

- **CRUCIAL: Always run ./check.sh BEFORE making any code changes**
- This establishes a baseline of expected failures before implementation
- Note: Some errors (e.g., undefined symbols) are expected at this stage
- Fix style and formatting issues only

1. **Verify failure**: Run tests to confirm they fail as expected

- Note the exact failure message for comparison later

1. **Implement**: Write the minimal code needed to make tests pass

- Implement one feature at a time
- Always think about edge cases and validation
- When adding new validations, make error messages clear and actionable

1. **Verify success with check.sh**:

- **ALWAYS use `./check.sh` to verify the implementation**
- Never skip this step after making changes
- Always run all tests, not just the new ones

1. **Run all quality checks**:

- Run `./check.sh` again to verify all quality checks pass
- It includes elisp-lint, tests, and documentation linting
- Fix any style, formatting, or doc issues before proceeding

1. **Refactor**: Clean up code while maintaining passing tests

- Run `./check.sh` after each refactoring step
- Consider readability and maintainability

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
- Use comments to explain complex algorithms, non-obvious side effects,
  or historical context
- Descriptive variable and function names often eliminate the need for comments

## Documentation Guidelines

### `README.org` Documentation

- Add user-facing documentation to `README.org` when implementing new features
- Include:
  - Brief description of the feature
  - Example code showing how to use it
  - Any important notes or caveats

### Tests as Documentation

- Write test cases that demonstrate both normal usage and edge cases
- Test cases serve as documentation of expected behavior
- Include comments in tests explaining non-obvious test scenarios

## Project Roadmap

For implementation details and roadmap, refer to the `TODO.org` file which contains:

- Implementation decisions (JSON-RPC, transport layer, etc.)
- Python SDK parity goals
- Transport implementation plans
- Testing requirements
- Future features
