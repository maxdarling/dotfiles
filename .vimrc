colorscheme desert
"" goal: I want to know what I have in my vimrc. 
"" Thus, I'll add things as-needed. Over time, I'll get a sense for what's most useful
"" and what isn't. 

"" vimplug for plugins
"call plug#begin()
"" commenting stuff out
"Plug 'tpope/vim-commentary'
"" colorscheme
"Plug 'morhetz/gruvbox'
"" kotlin syntax highlighting
"Plug 'https://github.com/udalov/kotlin-vim'

"" == todo == 
"" Valloric/YouCompleteMe
"" kien/ctrlp.vim.git (or similar file search)
"" moll/vim-mode.git (or similar text search)
"" scrooloose/nerdtree
"" use ag (tell ctrlp to use ag to do file searching)
"" https://github.com/tpope/vim-unimpaired
"call plug#end()


""==================== remaps (todo) (the magic of vim!==================== 
"" use leader as <space>
"" YcmCompleter GoTo



""==================== GOOD ==================== 
""autocmd vimenter * ++nested colorscheme gruvbox
"colorscheme gruvbox
"set number


""==================== TESTING  ==================== 
"set history=500

"" Enable filetype plugins
"filetype plugin on
"filetype indent on
"" show existing tab with 4 spaces width
"set tabstop=4
"" when indenting with '>', use 4 spaces width
"set shiftwidth=4
"" On pressing tab, insert 4 spaces
"set expandtab




""==================== TO TEST  ==================== 
"" remove word wrapping

"" remove auto comment on return?  

"" Set 7 lines to the cursor - when moving vertically using j/k
""set so=7


""" Turn on the Wild menu
""set wildmenu


""" Always show current position
""set ruler

""" Height of the command bar
""set cmdheight=1

""" A buffer becomes hidden when it is abandoned
""set hid

""" Configure backspace so it acts as it should act
""set backspace=eol,start,indent
""set whichwrap+=<,>,h,l

""" Ignore case when searching
""set ignorecase

""" When searching try to be smart about cases
""set smartcase

""" Highlight search results
""set hlsearch

""" Makes search act like search in modern browsers
""set incsearch

""" Don't redraw while executing macros (good performance config)
""set lazyredraw

""" For regular expressions turn magic on
""set magic

""" Show matching brackets when text indicator is over them
""set showmatch

""" How many tenths of a second to blink when matching brackets
""set mat=2

""" No annoying sound on errors
""set noerrorbells
""set novisualbell
""set t_vb=
""set tm=500

""" Properly disable sound on errors on MacVim
""if has("gui_macvim")
""    autocmd GUIEnter * set vb t_vb=
""endif

""" Add a bit extra margin to the left
""set foldcolumn=1


""" Set utf8 as standard encoding and en_US as the standard language
""set encoding=utf8

""" Use Unix as the standard file type
""set ffs=unix,dos,mac

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" => Colors and Fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" Enable syntax highlighting
""syntax enable

""" Enable 256 colors palette in Gnome Terminal
""if $COLORTERM == 'gnome-terminal'
""    set t_Co=256
""endif

""try
""    colorscheme desert
""catch
""endtry

""set background=dark



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" => Files, backups and undo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" Turn backup off, since most stuff is in SVN, git etc. anyway...
""set nobackup
""set nowb
""set noswapfile

