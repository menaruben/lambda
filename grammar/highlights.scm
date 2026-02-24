(define ":define" @keyword)
(abstraction "\\" @operator "." @operator)
(abstraction "λ" @operator "." @operator)
(macro "$" @operator (identifier) @function.macro)
(application "(" @punctuation.bracket ")" @punctuation.bracket)
(identifier) @variable
