; Note: Tree-sitter injection for fenced code blocks is complex
; For now, we'll disable injections to avoid highlighting entire comments/strings
; TODO: Implement proper fenced code block injection when Tree-sitter supports it better

; Commented out until proper fenced code block support is available:
;
; ((comment) @injection.content
;  (#match? @injection.content "```outrun")
;  (#set! injection.language "outrun"))
;
; ((block_comment) @injection.content
;  (#match? @injection.content "```outrun")
;  (#set! injection.language "outrun"))
;
; ((string) @injection.content
;  (#match? @injection.content "```outrun")
;  (#set! injection.language "outrun"))
;
; ((multiline_string) @injection.content
;  (#match? @injection.content "```outrun")
;  (#set! injection.language "outrun"))