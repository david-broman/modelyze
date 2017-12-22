" Vim syntax file
" Language: Modelyze
" Maintainer: Oscar Eriksson
" Latest Revision: 27 November 2017

if exists("b:current_syntax")
  finish
endif

syn keyword modelyzeKeywords else error false fst fun if in sym initmode include let def match snd then true type val with begin end specialize hybridchart mode init probe action when transition lift

syn match modelyzeLineComment '\/\/.*'
syn match modelyzeBlockComment '\/\*\_.*\*\/'

let b:current_syntax = "modelyze"
hi def link modelyzeLineComment     Comment
hi def link modelyzeBlockComment    Comment
hi def link modelyzeKeywords        Keyword
