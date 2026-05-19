; (define ":define" @keyword)
(abstraction "\\" @operator "." @keyword)
(abstraction "λ" @operator "." @keyword)
; (macro "$" @operator (identifier) @function.macro)
(application "(" @punctuation.bracket ")" @punctuation.bracket)
(identifier) @variable
