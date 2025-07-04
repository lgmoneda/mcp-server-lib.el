# Missing Test Coverage for Resource Templates

After analyzing the existing tests, implementation, and researching RFC 6570, MCP protocol specifications, and existing SDK implementations, here are the test coverage gaps I've identified:

## 1. Case Sensitivity Tests (RFC Compliance)
- **test-mcp-server-lib-resource-template-scheme-case-insensitive**: Test that URI schemes should be case-insensitive per RFC 3986 (e.g., `HTTP://example.com` should match `http://example.com`) - **Current implementation is INCORRECT**
- **test-mcp-server-lib-resource-template-variable-names-case-sensitive**: Test that variable names in templates are case-sensitive per RFC 6570 (e.g., `{UserName}` != `{username}`) - Current implementation is correct
- **test-mcp-server-lib-resource-template-path-literals-case-sensitive**: Test that literal path segments are case-sensitive (e.g., `/PATH/` != `/path/`) - Current implementation is correct

## 2. Unicode and Encoding Tests (RFC 6570 Compliance)
- **test-mcp-server-lib-resource-template-unicode-in-variables**: Test Unicode characters in variable values (e.g., `org://café.org` with proper percent-encoding)
- **test-mcp-server-lib-resource-template-percent-encoded-matching**: Test that percent-encoded URIs match correctly (e.g., `org://caf%C3%A9.org` matches `org://{filename}` template)
- **test-mcp-server-lib-resource-template-percent-encoded-extraction**: Test that extracted parameters remain percent-encoded (e.g., `filename` = `"caf%C3%A9.org"`)
- **test-mcp-server-lib-resource-template-no-auto-decoding**: Test that the library does NOT automatically decode parameters (handlers must decode for context-appropriate use)
- **test-mcp-server-lib-resource-template-unicode-normalization**: Test NFC normalization behavior for Unicode strings
- **test-mcp-server-lib-resource-template-multibyte-boundary**: Test that multi-byte UTF-8 sequences aren't split

## 3. Special Character Encoding Tests
- **test-mcp-server-lib-resource-template-reserved-chars-encoding**: Test reserved characters in default expansion (e.g., `/`, `?`, `#` should be encoded)
- **test-mcp-server-lib-resource-template-reserved-expansion-passthrough**: Test that `{+var}` allows reserved chars without encoding
- **test-mcp-server-lib-resource-template-spaces-encoding**: Test space handling (should become `%20`)

## 4. Template Matching Edge Cases  
- **test-mcp-server-lib-resource-template-first-match-precedence**: Test which template wins when multiple could match (currently first registered wins)
- **test-mcp-server-lib-resource-template-partial-literal-mismatch**: Test partial literal segments that don't match completely
- **test-mcp-server-lib-resource-template-consecutive-variables**: Test templates with adjacent variables like `{var1}{var2}` (should fail per RFC 6570)
- **test-mcp-server-lib-resource-template-ambiguous-delimiters**: Test when variable value contains the next literal delimiter
- **test-mcp-server-lib-resource-template-non-greedy-matching**: Test that variables use non-greedy (first match) behavior when followed by literals (e.g., `{name}.txt` matching `file.config.txt` extracts `name="file.config"`)

## 5. Registration and Management Tests
- **test-mcp-server-lib-register-resource-template-duplicate-with-different-handler**: Test registering same template with different handler (should increment ref count)
- **test-mcp-server-lib-register-resource-template-invalid-handler**: Test registration with nil or non-function handler
- **test-mcp-server-lib-unregister-resource-template-nonexistent**: Test unregistering non-existent template returns nil

## 6. Error Handling Tests
- **test-mcp-server-lib-resources-read-malformed-params**: Test resources/read with invalid params structure (string instead of object)
- **test-mcp-server-lib-resources-read-missing-uri**: Test resources/read without uri parameter
- **test-mcp-server-lib-resources-read-non-string-uri**: Test resources/read with non-string uri (number, array, etc.)
- **test-mcp-server-lib-resource-template-handler-wrong-signature**: Test template handler that doesn't accept params argument

## Critical Implementation Issues Found:
1. **URI schemes are currently case-sensitive but should be case-insensitive per RFC 3986**
2. **No percent-decoding of extracted parameters** - handlers receive encoded values and must decode them (this is correct design)
3. **No Unicode normalization (NFC) implementation** - RFC 6570 recommends this
4. Variable names are correctly case-sensitive per RFC 6570
5. Path literals are correctly case-sensitive
6. Variable matching uses non-greedy (first match) behavior - this is correct per Ruby Addressable precedent

## Priority Tests:
1. Percent-encoding/decoding tests (critical for Unicode support)
2. Case sensitivity tests (especially scheme case-insensitivity)
3. Error handling for malformed requests
4. Unicode character handling in variable values
5. Reserved character encoding behavior

## Implementation Notes:
- The library correctly does NOT auto-decode parameters, allowing handlers to decide based on context
- Handlers accessing file systems must decode parameters themselves using `url-unhex-string`
- This design prevents double-encoding issues and security vulnerabilities
- Non-greedy matching behavior aligns with the only major implementation (Ruby Addressable) that supports extraction

## Tests Removed as Already Covered or Redundant:
- Resource list template fields (covered by `test-mcp-server-lib-resources-list-mixed-resources`)
- Direct resource precedence (covered by `test-mcp-server-lib-resources-read-direct-precedence`)
- Variable character validation (covered by existing invalid syntax tests)
- Empty segments (partially covered by `test-mcp-server-lib-resource-template-empty-parameter-value`)
- Greedy matching renamed to non-greedy matching to reflect actual behavior