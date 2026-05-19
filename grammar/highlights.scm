(let_definition ":let" @keyword)

(macro_identifier [
  "@" @punctuation.special
  (identifier) @function.macro
])

(abstraction [ "\\" "λ" ] @operator "." @punctuation.delimiter)
(application [ "(" ")" ] @punctuation.bracket)

(identifier) @variable
