# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

mcp-server-lib.el is an Emacs Lisp library for building Model Context Protocol (MCP) servers. It provides infrastructure for Emacs packages to expose functionality as tools and resources to Large Language Models via JSON-RPC communication.

**Architecture:**
- Core library: `mcp-server-lib.el` - Main API, JSON-RPC handling, protocol implementation
- Commands: `mcp-server-lib-commands.el` - Interactive commands for users
- Metrics: `mcp-server-lib-metrics.el` - Usage tracking and debugging support  
- Testing: `mcp-server-lib-ert.el` - ERT utilities for MCP server testing
- Transport: `emacs-mcp-stdio.sh` - Stdio wrapper script for emacsclient communication

## Development Commands

**Testing:**
```bash
eask test                     # Run ERT tests
eask test ert mcp-server-lib-test.el  # Run specific test file
```

**Linting:**
```bash
eask lint                     # Run Elisp linter
eask exec org-lint            # Check org files (README.org, TODO.org)
```

**Building:**
```bash
eask package                  # Build package
eask compile                  # Byte-compile
```

## Project Structure

The library follows a modular architecture:

1. **Protocol Layer** (`mcp-server-lib.el`): Handles MCP protocol specification, JSON-RPC 2.0 communication, and server lifecycle
2. **Registration System**: Tool and resource registration with support for static resources and URI templates using `{variable}` syntax
3. **Transport Layer**: Stdio communication via `emacs-mcp-stdio.sh` wrapper script
4. **Metrics System**: Request counting, error tracking, and debugging support
5. **Testing Framework**: ERT utilities for testing MCP servers built on this library

**Key Design Patterns:**
- Resource templates use RFC 6570 subset for URI pattern matching (`{variable}`, `{+variable}`)  
- Error handling with specific JSON-RPC error codes for meaningful client feedback
- Unified API where static vs templated resources are determined by presence of `{variable}` syntax
- Tool handlers return strings; resource handlers return strings with optional MIME type

## Custom Version Objective

This version has been modified to modernize the tool registration API to match claude-code-ide.el's approach, providing explicit argument specification instead of relying on docstring parsing.

**Objective:** Replace the original `mcp-server-lib-register-tool` function that used:
- Docstring parsing to extract parameter information
- Support for only 0-1 arguments maximum
- `:id` parameter for tool identification

With a new API that uses:
- Explicit `:args` specification with full type information
- Support for multiple arguments via proper `apply` invocation
- `:name` parameter for tool identification
- Same API format as claude-code-ide.el for consistency

## Changes Made

### Core API Changes

1. **Function Signature Update** (`mcp-server-lib.el` lines ~1001-1100):
   - Replaced `mcp-server-lib-register-tool(handler &rest properties)` 
   - With `mcp-server-lib-register-tool(&rest slots)` using keyword arguments
   - Changed from positional `handler` argument to `:function` keyword
   - Changed from `:id` to `:name` for tool identification

2. **Schema Generation** (`mcp-server-lib.el` lines ~1150-1200):
   - Added `mcp-server-lib--generate-schema-from-args(args)` function
   - Generates JSON schema from explicit argument specifications
   - Supports all argument types: string, number, integer, boolean, array, object, null
   - Handles optional arguments and enum constraints

3. **Multi-argument Support** (`mcp-server-lib.el` lines ~1200-1250):
   - Added `mcp-server-lib--call-handler-with-args(handler tool tool-args)` function
   - Fixed critical bug where only first argument was passed to handlers
   - Properly extracts arguments from JSON and maps them to function parameters
   - Uses `apply` instead of `funcall` for multi-argument function calls

4. **Validation Logic** (`mcp-server-lib.el` lines ~1050-1100):
   - Enhanced argument validation for new keyword-based API
   - Validates function, name, description, and optional parameters
   - Added comprehensive argument specification validation

### Updated Documentation

1. **README.org**: Updated to include new API examples and benefits section
2. **CLAUDE.md**: Added this documentation of objectives and changes

### User Configuration Update

Updated user's custom MCP settings file (`/Users/luis.moneda/repos/dot-emacs/settings/custom-emacs-mcp-server-settings.el`):
- Converted all tool registrations from old `:id` format to new `:function/:name/:args` format
- Updated function signatures to match original implementations in mcp-tools folder
- Added proper load-path handling to ensure development version takes precedence over ELPA

### Benefits Achieved

1. **Explicit Type System**: Arguments now have explicit types instead of being inferred from docstrings
2. **Multi-argument Support**: Fixed fundamental limitation where only 0-1 argument functions worked
3. **Better Validation**: JSON schema validation for all argument types
4. **API Consistency**: Same API format as claude-code-ide.el
5. **Maintainability**: Cleaner, more maintainable code without docstring parsing

## Development Notes

- This is an Elisp project following user's Elisp guidelines at `~/.claude/CLAUDE-elisp.md`
- User-facing documentation is in README.org and NEWS
- ELPA isolation: Before Elisp work, move user ELPA copy away and restore after completion
- Protocol version: Currently supports MCP protocol version "2025-03-26"
- **Load Path Precedence**: Development version must be loaded before ELPA version to ensure new API is used
