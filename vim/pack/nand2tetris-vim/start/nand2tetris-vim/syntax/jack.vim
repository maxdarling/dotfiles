if exists("b:current_syntax")
    finish
endif

" Keywords
syntax keyword jackKeyword class method function constructor
syntax keyword jackKeyword var field static
syntax keyword jackKeyword let if while do return
syntax keyword jackKeyword this

" Constants
syntax keyword jackConstant true false null
syntax region jackString start=/"/ skip=/\\./ end=/"/

" Types
syntax keyword jackType int boolean char void

" Comments
syntax match jackComment "\v\/\/.*$" " // inline
syntax match jackComment "\/\*\_.\{-}\*\/" " block
syntax match jackComment "\/\*\*\_.\{-}\*\/" " docs. todo: make diff color. and fold.

" Operators
syntax match jackOperator "="
syntax match jackOperator "+"
syntax match jackOperator "-"
" syntax match jackOperator "*" " conflicts with comments!! todo: fix.
" syntax match jackOperator "\/"
syntax match jackOperator "&"
syntax match jackOperator "|"
syntax match jackOperator "\~"
syntax match jackOperator "<"
syntax match jackOperator ">"

" Functions
"
" 'Memory'
syntax keyword jackFunction deAlloc 
" 'Keyboard'
syntax keyword jackFunction keyPressed
" 'Screen'
syntax keyword jackFunction setColor drawRectangle

highlight link jackKeyword Keyword
highlight link jackConstant Constant
highlight link jackString String
highlight link jackType Type
highlight link jackComment Comment
highlight link jackOperator Operator
highlight link jackFunction Function

" highlight Comment ctermfg=Blue " this sets it for .vim... lmfao
" highlight Operator ctermfg=Red 

let b:current_syntax = "jack"
