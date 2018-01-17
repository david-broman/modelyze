" Vim syntax file
" Language: Modelyze
" Maintainer: Oscar Eriksson
" Latest Revision: 27 November 2017

if exists("b:current_syntax")
  finish
endif

syn keyword modelyzeKeywords else error false fst fun if in sym initmode include let def match snd then true type val with begin end specialize hybridchart mode init probe action when transition lift reinit der switch pre

syn keyword modelyzeTypes Real Int Bool ?

syn region modelyzeString oneline start='"' end='"' contained

syn match modelyzeNumber '\d\+' contained display
syn match modelyzeNumber '[-+]\d\+' contained display
syn match modelyzeNumber '\d\+\.\d*' contained display
syn match modelyzeNumber '[-+]\d\+\.\d*' contained display

syn match modelyzeLineComment '\/\/.*'
syn match modelyzeBlockComment '\/\*\_.*\*\/'

let b:current_syntax = "modelyze"
hi def link modelyzeLineComment     Comment
hi def link modelyzeBlockComment    Comment
hi def link modelyzeKeywords        Keyword
hi def link modelyzeTypes           Type
hi def link modelyzeString          Constant
hi def link modelyzeNumber          Constant
