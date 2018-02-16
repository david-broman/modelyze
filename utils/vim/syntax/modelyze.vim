" Vim syntax file
" Language: Modelyze
" Maintainer: Oscar Eriksson
" Latest Revision: 27 November 2017

if exists("b:current_syntax")
  finish
endif

syn keyword modelyzeKeywords else error false fst fun if in sym initmode include let def match snd then true type val with begin end specialize hybridchart mode init probe action when transition lift reinit der switch

syn keyword modelyzeTypes Real Int Bool ?

syntax region modelyzeString start=/\v"/ skip=/\v\\./ end=/\v"/

syn match   modelyzeNumber    "\<\d\+\>" display
syn match   modelyzeNumber    "\.\d\+\(e[+-]\=\d\+\)\=\>" display
syn match   modelyzeNumber     "\<\d\+e[+-]\=\d\+\>" display
syn match   modelyzeNumber     "\<\d\+\.\d*\(e[+-]\=\d\+\)\=" display

" syn match modelyzeBlockComment '\/\*\_.*\*\/'
syn match modelyzeLineComment '\/\/.*'

let b:current_syntax = "modelyze"
hi def link modelyzeBlockComment    Comment
hi def link modelyzeLineComment     Comment
hi def link modelyzeKeywords        Keyword
hi def link modelyzeTypes           Type
hi def link modelyzeNumber          Constant
hi def link modelyzeString          String
