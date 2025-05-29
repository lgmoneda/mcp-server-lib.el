# MCP Tool Usability Metrics - Design Document

## Overview

A simple, reusable metrics system to track MCP operation usage and error rates across all entity types (tools, resources, prompts).

## Architecture

### Data Structure
```elisp
;; Core metrics type
(cl-defstruct mcp-metrics
  calls      ; integer: total invocations
  errors     ; integer: failed invocations  
  last-called) ; timestamp: most recent invocation

;; Global storage
(defvar mcp-server-lib--metrics (make-hash-table :test 'equal))
;; Key format: "method" or "method:entity-name"
;; Examples: "tools/list", "tools/call:read-file"
```

### Key Design Decisions

1. **Unified metrics struct** - Same structure for all operations
2. **String-based keys** - Leverage JSON-RPC method names directly
3. **Hierarchical tracking** - Both aggregate and specific metrics
4. **Zero configuration** - Works automatically when operations are invoked

## Integration Points

1. **Method dispatch** (`mcp-server-lib--dispatch-jsonrpc-method`)
   - Increment counter for every method call
   - Set last-called timestamp

2. **Tool invocation** (`tools/call` handler)
   - Track specific tool usage: `"tools/call:tool-name"`
   - Increment error counters on failure
   - Propagate errors to method-level metrics

3. **Server lifecycle** (`mcp-server-lib-stop`)
   - Optional display of metrics summary on shutdown

## User Interface

### Interactive Commands

| Command | Description | Output |
|---------|-------------|--------|
| `mcp-server-lib-show-metrics` | Display detailed metrics in dedicated buffer | Full buffer with categorized metrics |
| `mcp-server-lib-reset-metrics` | Clear all counters | Message: "MCP metrics reset" |
| `mcp-server-lib-metrics-summary` | Get one-line summary string | String: "MCP metrics: 188 calls, 6 errors (3.2% error rate)" |

### Display Formats

#### Full Buffer Display (`mcp-server-lib-show-metrics`)
Three sections:
1. **Method Calls** - Aggregate operations (initialize, tools/list, etc.)
2. **Notifications** - Client notifications (no error tracking)
3. **Tool Usage** - Specific tool invocations with error rates

Example output:
```
MCP Usage Metrics
=================

Session started: 2025-01-28 14:30:00

Method Calls:
---------------------------------------- ------ ------- ----------
initialize                                   1       0       0.0%
prompts/list                                 8       0       0.0%
tools/call                                  80       3       3.8%
tools/list                                  15       0       0.0%

Notifications:
---------------------------------------- ------
notifications/cancelled                      3
notifications/initialized                    1

Tool Usage:
---------------------------------------- ------ ------- ----------
  list-directory                            23       1       4.3%
  read-file                                 45       2       4.4%
  write-file                                12       0       0.0%

Summary:
--------
Total operations: 188
Total errors: 6
Overall error rate: 3.2%
```

#### Summary String (`mcp-server-lib-metrics-summary`)
- **Format**: `"MCP metrics: N calls, M errors (X.X% error rate)"`
- **Usage**: 
  - Minibuffer display for quick checks
  - Server stop notification
  - Programmatic access by other packages
- **Example**: `"MCP metrics: 188 calls, 6 errors (3.2% error rate)"`

### Customization Variables

- `mcp-server-lib-collect-metrics` - Enable/disable collection (default: t)
- `mcp-server-lib-show-metrics-on-stop` - Auto-display summary on stop (default: nil)

## Error Handling

- Tool errors increment both tool-specific and method-level error counts
- Internal errors are tracked separately from user errors
- Error rate calculated as: (errors / total calls) * 100

## Future Extensibility

The design supports future entity types without modification:

| Entity Type | Method-level Key | Specific Key |
|------------|------------------|--------------|
| Tools | `tools/call` | `tools/call:tool-name` |
| Resources | `resources/read` | `resources/read:uri-pattern` |
| Prompts | `prompts/get` | `prompts/get:prompt-id` |
| Samplers | `sampling/sample` | `sampling/sample:sampler-id` |

## Benefits

1. **Simple** - Minimal code, easy to understand
2. **Performant** - Single hash lookup per operation
3. **Extensible** - New operations automatically tracked
4. **Insightful** - Shows usage patterns and error hotspots
5. **Non-intrusive** - Can be disabled completely

## Implementation Notes

- Metrics collection happens automatically when `mcp-server-lib-collect-metrics` is non-nil
- The `mcp-metrics` struct uses `cl-defstruct` for type safety and accessor functions
- Keys in the metrics hash table follow the JSON-RPC method naming convention
- Notifications are tracked but don't contribute to error counts
- The reset function clears all metrics but doesn't affect server operation