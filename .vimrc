" goal: I want to know what I have in my vimrc. 
" Thus, I'll add things as-needed. Over time, I'll get a sense for what's most useful
" and what isn't. 

" vimplug for plugins
call plug#begin()
" commenting stuff out
Plug 'tpope/vim-commentary'
" colorscheme
Plug 'morhetz/gruvbox'
" kotlin syntax highlighting
Plug 'https://github.com/udalov/kotlin-vim'
" markdown preview in browser (:MarkdownPreview)
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
" * search on visual selection
Plug 'https://github.com/nelstrom/vim-visual-star-search'
" fzf
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" == todo == 
" Valloric/YouCompleteMe
" kien/ctrlp.vim.git (or similar file search)
" moll/vim-mode.git (or similar text search)
" scrooloose/nerdtree
" use ag (tell ctrlp to use ag to do file searching)
" https://github.com/tpope/vim-unimpaired
" abolish.vim
call plug#end()


"==================== remaps: the magic of vim!==================== 
let mapleader=" "
set showcmd

" dotfile editing (matches bash_profile)
nnoremap <silent> <leader>dvp :e $MYVIMRC<CR>
nnoremap <silent> <leader>dbp :e $MYBASHPROFILE<CR>

" fzf
nnoremap <silent> <leader>fo :Files<CR>
nnoremap <silent> <leader>fb :Buffers<CR>
nnoremap <silent> <leader>fg :GFiles<CR>
nnoremap <silent> <leader>fh :History<CR>

" open ftplugin file for current filetype (todo)
" nnoremap <silent> <leader>q :exe ':echo ~/.vim/ftplugin/&ft.vim'

" ~~~ AUTOCMDs ~~~
" necessary for autocmds to just be included 1x
augroup vimrc
  " Remove all vimrc autocommands
  autocmd!
  "misc
  au vimenter * ++nested colorscheme gruvbox
augroup END

"==================== GOOD ==================== 
set history=500

" colorscheme
set bg=dark "override lightmode Terminal

" line numbers
set number

" show existing tab with 4 spaces width
set tabstop=4
" when indenting with '>', use 4 spaces width
set shiftwidth=4
" On pressing tab, insert 4 spaces
set expandtab

" When searching try to be smart about cases
set ignorecase
set smartcase

" search highlight
set hlsearch
set incsearch
" add :nohlsearch to the default <C-l> redraw functionality (practical vim book)
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>
" clear search when sourcing (otherwise higlights last search)
let @/ = ""

" relative number
" set relativenumber
set number

" Enable filetype plugins
set nocompatible
filetype plugin on
filetype indent on

"==================== TESTING  ==================== 
" speed up pasting?
set noswapfile

" autoindent (good for markdown bulleted lists)
set autoindent

" hidden buffers
set hidden

" put all swap files in a single folder 
" source: https://vi.stackexchange.com/questions/177/what-is-the-purpose-of-swap-files
set directory^=$HOME/.vim/tmp//

"" Turn on the Wild menu
set wildmenu

"==================== TO TEST  ==================== 
" remove word wrapping

" remove auto comment on return?  

" Set 7 lines to the cursor - when moving vertically using j/k
"set so=7

"" Always show current position
"set ruler

"" Height of the command bar
"set cmdheight=1

"" A buffer becomes hidden when it is abandoned
"set hid

"" Configure backspace so it acts as it should act
"set backspace=eol,start,indent
"set whichwrap+=<,>,h,l

"" Don't redraw while executing macros (good performance config)
"set lazyredraw

"" For regular expressions turn magic on -- on by default though...
"set magic

"" Show matching brackets when text indicator is over them
"set showmatch

"" How many tenths of a second to blink when matching brackets
"set mat=2

"" No annoying sound on errors
"set noerrorbells
"set novisualbell
"set t_vb=
"set tm=500

"" Properly disable sound on errors on MacVim
"if has("gui_macvim")
"    autocmd GUIEnter * set vb t_vb=
"endif

"" Add a bit extra margin to the left
"set foldcolumn=1

"" Set utf8 as standard encoding and en_US as the standard language
"set encoding=utf8

"" Use Unix as the standard file type
"set ffs=unix,dos,mac

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" => Colors and Fonts
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Enable syntax highlighting
"syntax enable

"" Enable 256 colors palette in Gnome Terminal
"if $COLORTERM == 'gnome-terminal'
"    set t_Co=256
"endif

"try
"    colorscheme desert
"catch
"endtry

"set background=dark



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" => Files, backups and undo
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Turn backup off, since most stuff is in SVN, git etc. anyway...
"set nobackup
"set nowb

