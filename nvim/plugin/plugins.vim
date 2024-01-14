call plug#begin()
" core
Plug 'https://github.com/tpope/vim-commentary'
Plug 'https://github.com/tpope/vim-surround'

" extra
Plug 'glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }
call plug#end()
