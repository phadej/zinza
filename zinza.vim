" Vim syntax file
" Language:   Zinza
" Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
" File Types: .zinza
"
" https://hackage.haskell.org/package/zinza
"
" Remember to add
"
" autocmd BufRead,BufNewFile *.zinza set filetype=zinza
"
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn match zinzaComment "{#.\{-}#}"

syn keyword zinzaKeyword contained for in endfor
syn keyword zinzaKeyword contained if elif else endif
syn keyword zinzaKeyword contained defblock endblock useblock

syn match zinzaPragma "{%.\{-}%}" contains=zinzaKeyword

syn match zinzaVariable "{{.\{-}}}"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_cabal_syn_inits")
  if version < 508
    let did_cabal_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink zinzaComment       Comment
  HiLink zinzaPragma        Function
  HiLink zinzaKeyword       Keyword
  HiLink zinzaVariable      Identifier
  delcommand HiLink
endif

let b:current_syntax = "zinza"
