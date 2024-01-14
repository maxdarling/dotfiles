" Core settings to be shared by Vim and NeoVim
" todo:
"   - source the plugin dir so neovim can benefit?
"   - then, add mappings for :Bdi, lite terminal, etc
"==================== Sane Defaults  ====================
set nocompatible
filetype plugin on
filetype indent on
syntax on
set backspace=indent,eol,start

"==================== Mappings  ====================
let mapleader = " "

" testing (comments should be easier than `gcc`...)
nmap <leader>c gcc
vmap <leader>c gc

nmap <leader>ej :e ~/.vim/base_config.vim<CR>
nmap <leader>eb :e ~/.bash_profile<CR>

" use s to cycle windows!
noremap s <C-w><C-w>
" use S to cycle buffers! (testing)
noremap S :bn<CR>

" todo: have <C-c>'ed files not be switchable back to
" or, have another key cycle files (with a single keypress)
"
" todo: <in a file> -> netrw to pick a new file -> 'r' -> goes to a "[New
" File]". I don't want that.
nnoremap r <C-^>
" available maps: {R, -, ', `, \, <C-f>, <C-b>}

" delete buffer without losing split
" (competes with this 500-stars 80 lines: https://github.com/moll/vim-bbye)
nnoremap <C-c> :bp\|bd! #<CR>

nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>

" todo:
" cnoremap <C-k>   
cnoremap <C-a> <C-b>

nnoremap <silent> <leader>sp :execute "botright split " .. bufname("#")<CR>

"make Y consistent with C and D
nnoremap Y y$

"==================== Settings ====================
set number
set hidden

set tabstop=4 " tab = 4 spaces wide
set shiftwidth=4 " indent = 4 spaces
set expandtab " tabs insert spaces
set autoindent

" When searching try to be smart about cases
set ignorecase
set smartcase

" search highlight
set hlsearch
set incsearch

set statusline=%f\ %y%=\(%c\)\ \ %l/%L
set laststatus=2
set showcmd
set showmode
set cmdheight=2

set wildmenu
set wildmode=list:longest,full
set wildignore=*.o,*.obj,*~

set scrolloff=3
set sidescrolloff=7
set sidescroll=1

" misc
set nostartofline " make C-u/d not move cursor to start of line
set mouse=a "enable mouse
set history=500
set bg=dark "override lightmode Terminal

"==================== Testing  ===================
set showmatch
set matchtime=2

"display tabs and trailing spaces
set list
set listchars=tab:▷⋅,trail:⋅,nbsp:⋅

set wrap        "dont wrap lines
set linebreak   "wrap lines at convenient points
set breakindent "wrap lines such that vertical indent is not broken

set confirm "show dialog when exiting with unsaved changes.

" disable swapfiles (as I can't get the below to work, it makes netrw dir
" access take 2 enter presses for some reason)
" set noswapfile
" " put all swap files in a single folder 
set directory=~/.vim/swap

" set formatoptions-=o "dont continue comments when pushing /O
