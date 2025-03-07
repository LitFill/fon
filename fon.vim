if exists("b:current_syntax")
    finish
endif

syn keyword mykeyword jika maka tidak biar di
syn match   mylambda  "λ"
syn match   myarrow   "->"

syn match myoperator "[+\-*/=<>\\]"

syn keyword myboolean benar salah
syn match   mynumber  "\<\d\+\>"
syn region  mystring  start="«" end="»" contains=myescape,mystring
syn match   myescape  "\\[\\«»nt]"

syn match myidentifier "[a-zA-Z_][a-zA-Z0-9_]"

hi def link mykeyword    Keyword
hi def link mylambda     Statement
hi def link myarrow      Statement
hi def link myoperator   Operator
hi def link myboolean    Boolean
hi def link mynumber     Number
hi def link mystring     String
hi def link myescape     SpecialChar
hi def link myidentifier Identifier

let b:current_syntax = "fon"

