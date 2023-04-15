" Core settings to be shared by Vim and NeoVim
"==================== Sane Defaults  ====================
set nocompatible
filetype plugin on
filetype indent on
syntax on
set backspace=indent,eol,start

"==================== Mappings  ====================
let mapleader = " "

nnoremap s <C-w><C-w>
nnoremap S s
nnoremap r <C-^>
" available maps: {R, -, ', `, \}

nnoremap <C-c> :bp\|bd! #<CR>
nnoremap <silent> <C-h> :<C-u>nohlsearch<CR><C-l>

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

set confirm "show dialog when exiting with unsaved changes.

" put all swap files in a single folder 
" source: https://vi.stackexchange.com/questions/177/what-is-the-purpose-of-swap-files
set directory^=~/.vim/swapfiles//

" set formatoptions-=o "dont continue comments when pushing /O
