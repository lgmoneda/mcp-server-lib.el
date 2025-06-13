# Resource Support Implementation Plan for mcp-server-lib

This document outlines both the requirements and implementation plan for adding
resource support to mcp-server-lib.el, following the Model Context Protocol
specification.

## 1. Resource Registration API

### Direct Resources

```elisp
(cl-defun mcp-server-lib-register-resource (uri handler
                                           &key name description
                                           mime-type)
  "Register a direct resource with the MCP server.

  URI: Exact URI for the resource (e.g., \"org://projects.org\")
  HANDLER: Function that takes no arguments and returns the content
  :name - Required human-readable name
  :description - Optional description
  :mime-type - Optional MIME type (default: \"text/plain\")")

(defun mcp-server-lib-unregister-resource (uri)
  "Unregister a resource by its URI.")
```

### Resource Templates

```elisp
(cl-defun mcp-server-lib-register-resource-template (uri-template handler
                                                    &key name description
                                                    mime-type)
  "Register a resource template with the MCP server.

  URI-TEMPLATE: RFC 6570 URI template (e.g., \"org://{filename}/headline/{+path}\")
  HANDLER: Function that takes URI parameters and returns the content
  :name - Required human-readable name for this type of resource
  :description - Optional description
  :mime-type - Optional MIME type for all matching resources")

(defun mcp-server-lib-unregister-resource-template (uri-template)
  "Unregister a resource template by its pattern.")
```

## 2. Required Protocol Methods

The library needs to handle two JSON-RPC methods:

- **`resources/list`** - Return list of available resources and resource templates
  - Direct resources with `uri`, `name`, `description`, `mimeType`
  - Resource templates with `uriTemplate`, `name`, `description`, `mimeType`
- **`resources/read`** - Read resource content by URI
  - Must handle both direct URIs and URIs matching templates
  - Template handlers receive extracted parameters

## 3. Capability Reporting

Update initialization response to include:

```elisp
(capabilities . ((resources . ,(make-hash-table))))
```

## 4. Handler Contract

### Direct Resource Handlers

- Take no arguments
- Return string content for text resources
- Signal errors appropriately

### Template Resource Handlers

- Take URI parameters as an alist (e.g., `(("filename" . "projects.org") ("path" . "Tasks/Urgent"))`)
- Return string content for text resources
- Signal errors appropriately (including invalid parameters)

## Example Usage in org-mcp

```elisp
;; Register direct resources for each allowed file (for discoverability)
(dolist (file org-mcp-allowed-files)
  (let ((expanded (expand-file-name file))
        (basename (file-name-nondirectory file)))
    ;; File resource
    (mcp-server-lib-register-resource
     (format "org://%s" basename)
     (lambda () (org-mcp--read-file-content expanded))
     :name basename
     :description (format "Org file: %s" expanded)
     :mime-type "text/plain")))

;; Register resource templates for dynamic access patterns
(mcp-server-lib-register-resource-template
 "org://{filename}/outline"
 (lambda (params)
   (org-mcp--generate-outline
    (org-mcp--resolve-filename (alist-get "filename" params nil nil #'string=))))
 :name "Org file outline"
 :description "Hierarchical outline of an Org file"
 :mime-type "application/json")

(mcp-server-lib-register-resource-template
 "org://{filename}/headline/{+path}"
 (lambda (params)
   (org-mcp--get-headline-content
    (alist-get "filename" params nil nil #'string=)
    (alist-get "path" params nil nil #'string=)))
 :name "Org headline by path"
 :description "Access specific headline by its path"
 :mime-type "text/plain")

(mcp-server-lib-register-resource-template
 "org://id/{id}"
 (lambda (params)
   (org-mcp--get-headline-by-id
    (alist-get "id" params nil nil #'string=)))
 :name "Org headline by ID"
 :description "Access headline by its unique ID property"
 :mime-type "text/plain")
```

## Key Design Decisions

1. **Both exact URIs and templates** - Direct resources for files, templates for patterns
2. **Name is required** - Every resource/template needs a human-readable name
3. **Two handler types** - Direct (no params) and template (with params)
4. **Hybrid approach** - Direct resources for file discovery, templates for sub-resources
5. **RFC 6570 compliance** - Templates follow standard URI template syntax
6. **Security** - Template handlers must validate parameters against allowed files

## Implementation Plan

### Feature 1: Basic Resource Registration

Start with the simplest case - registering and listing direct resources.

**Test sequence:**

1. Test that resources/list returns empty array when no resources registered
2. Test registering a direct resource with minimal required fields
3. Test that resources/list includes the registered resource
4. Test error when registering without required :name field
5. Test unregistering a resource

**Implementation emerges from tests** - data structures and functions created only when tests require them.

### Feature 2: Reading Direct Resources

**Test sequence:**

1. Test resources/read returns error for non-existent resource
2. Test successful read of registered resource content
3. Test handler errors are properly converted to protocol errors
4. Test multiple resources can be registered and read independently

### Feature 3: URI Template Support

**Test sequence:**

1. Test parsing simple template `{variable}`
2. Test parsing reserved expansion `{+variable}`
3. Test template matching against URIs
4. Test parameter extraction from matched URIs
5. Test invalid template syntax handling

### Feature 4: Template Resources

**Test sequence:**

1. Test registering template resource
2. Test resources/list includes templates with uriTemplate field
3. Test resources/read matching against templates
4. Test parameter passing to template handlers
5. Test template precedence (direct resources checked first)

### Feature 5: Advanced Template Features

**Test sequence:**

1. Test multiple variables in template
2. Test path segments with reserved expansion
3. Test overlapping template patterns
4. Test edge cases in URI matching

### Error Handling Strategy

- Each handler error becomes a protocol error with appropriate code
- Invalid parameters trigger specific error messages
- Missing resources return -32602 (Invalid params)
- Handler exceptions return -32603 (Internal error)

### Integration Points

- Update dispatch function only when tests require it
- Add capabilities only when protocol tests need them
- Reference counting added only if tests show need for it

### Security Considerations

- Test malicious URI patterns before implementing validation
- Test parameter injection scenarios
- Test resource isolation between handlers
