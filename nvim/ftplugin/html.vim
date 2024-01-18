" vim by defaults sources html ftplugin for markdown files. this aborts.
" see $VIMRUNTIME/ftplugin/markdown.vim for full details
if &ft=="markdown"
  finish
endif

map <buffer> <leader>x :exe ':silent !open -a /Applications/Google\ Chrome.app %'<CR>:redr!<CR>
