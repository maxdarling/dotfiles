call plug#begin()
Plug 'https://github.com/tpope/vim-commentary'
Plug 'https://github.com/tpope/vim-surround'
Plug 'https://github.com/tpope/vim-repeat'
Plug 'https://github.com/nelstrom/vim-visual-star-search'
Plug 'morhetz/gruvbox'

" markdown preview in browser (:MarkdownPreview)
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}

" fzf
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" == plugin todo == 
" - https://github.com/tpope/vim-unimpaired (todo: extract useful ones myself)
" - abolish.vim ( I should implement the ones I want from scratch!)
"             ( e.g. convert words to snake/camel/upper case. )
call plug#end()
