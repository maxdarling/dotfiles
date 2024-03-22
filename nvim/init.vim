""""""""""""""""""""""""""""""""""
" PLUGINS {{{
""""""""""""""""""""""""""""""""""
call plug#begin()
" core
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'

"easymotion
Plug 'easymotion/vim-easymotion'

" treesitter
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'nvim-treesitter/nvim-treesitter-textobjects'
" Plug 'nvim-treesitter/nvim-treesitter-context'

" telescope
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make' }
Plug 'nvim-telescope/telescope.nvim', { 'tag': '0.1.5' }

"floating term
Plug 'voldikss/vim-floaterm'

" conjure (for lisp eval)
Plug 'Olical/conjure'

" colorscheme
Plug 'ellisonleao/gruvbox.nvim'
Plug 'folke/tokyonight.nvim'
Plug 'catppuccin/nvim', { 'as': 'catppuccin' }
Plug 'sainnhe/sonokai'
Plug 'navarasu/onedark.nvim'

" statusline
Plug 'nvim-lualine/lualine.nvim'
Plug 'nvim-tree/nvim-web-devicons'

" firenvim
Plug 'glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }
call plug#end()

source ~/.vim/base_config.vim
lua require('treesitter-config')
lua require('firenvim-config')
lua require('lualine-config')
" }}}

""""""""""""""""""""""""""""""""""
" TODO {{{
""""""""""""""""""""""""""""""""""
" - learn to use treesitter folding (e.g. fold all functions)
" - lsp setup? for python?
" - improve quit buffer than leader-q? hurts my hand
" - fugitive
" - context treesitter (for floating containing function at top of buffer)
" - harpoon?

" - config 'r' to not switch back to a C-c'ed buffer (can use normal C-^ or 
"   :e # if really needed)

" Open Keys:
" - h, l (I use arrow keys! ballsy, I know!)
" }}}

""""""""""""""""""""""""""""""""""
" TESTING {{{
""""""""""""""""""""""""""""""""
" delete without affecting the default register
noremap <leader>d "_d

" quickly browse quickfixlist (when grepping, for example)
nnoremap <leader>n :cn<CR>
nnoremap <leader>p :cp<CR>

" homebrew sicp repl
tnoremap <Esc> <C-\><C-n>
tnoremap <C-w> <C-\><C-n><C-w>

function! SendBufferToAltWin()
    norm ggyG<C-w>wp<CR><C-w>w
endfunction




" free up q. it's not very common. it should require a leader.
" noremap <leader>q q

" format entire file not that good. often makes massive edits. better '=ap'
" nmap <leader>f <Cmd>normal! mzgg=gG`z<CR>
" }}}

"""""""""""""""""""""""""""""""""
" GENERAL {{{
"""""""""""""""""""""""""""""""""
set background=dark
let g:onedark_config = {
    \ 'style': 'darker',
\}
colorscheme onedark
" colorscheme sonokai

" utils (~/.config/nvim/plugin/utils.vim)
command! Bdi :call DeleteInactiveBufs()

" NOTE: vscode-neovim maps several commands like :e, :w so that VSCode can understand
" them. So our mappings that use them must be ~~recursive~~.
nmap <silent><leader>ev :e ~/.config/nvim/init.vim<CR>
nmap <silent><leader>ep :e ~/.config/nvim/plugin/<CR>
nmap <silent><leader>ef :execute 'edit ~/.config/nvim/ftplugin/' .. &filetype .. '.vim'<CR>
nmap <silent><leader>ez :e ~/.zshrc<CR>

" }}}

"""""""""""""""""""""""""""""""""
" TERMINAL {{{
"""""""""""""""""""""""""""""""""
" one key to pop to/from a terminal
noremap <silent> <C-j> :<C-u>FloatermToggle<CR>
inoremap <silent> <C-j> <C-o>:FloatermToggle<CR>
tnoremap <silent> <C-j> <C-\><C-n>:FloatermToggle<CR>
" tnoremap <Esc> <C-\><C-n>
" set winblend=5 " testing
hi Floaterm guibg=#282828
hi FloatermBorder guibg=#282828 guifg=#25de14
" }}}

"""""""""""""""""""""""""""""""""
" NICHE {{{
"""""""""""""""""""""""""""""""""
augroup filetype_vim
  autocmd!
  " enable folding for Vimscript files (usage: 'za' to toggle)
  autocmd FileType vim setlocal foldmethod=marker

  " (exclude 'curdir' from viewoptions to not clobber current dir)
  let &viewoptions="folds,cursor"

  " remember folds for vimrc 
  autocmd BufWinLeave init.vim mkview
  autocmd BufWinEnter init.vim silent loadview 
augroup end

" html files no indenting (my xah style)
au FileType html set indentexpr=""

" }}}

""""""""""""""""""""""""""""""""
" Easymotion {{{
""""""""""""""""""""""""""""""""
let g:EasyMotion_do_mapping = 0 "disable default mappings
nmap s <Plug>(easymotion-overwin-f)
nmap S <Plug>(easymotion-bd-jk)
let g:EasyMotion_smartcase = 1
let g:EasyMotion_keys = 'bdfgjklmopruvwxyncsiteha'

" }}}

""""""""""""""""""""""""""""""""
" Telescope {{{
""""""""""""""""""""""""""""""""
nnoremap <silent> lo :Telescope find_files<CR>
nnoremap <silent> la :Telescope buffers<CR>
nnoremap <silent> lg :Telescope live_grep<CR>
nnoremap <silent> lh :Telescope help_tags<CR>
" fringe
nnoremap <silent> lc :Telescope grep_string<CR>
nnoremap <silent> lx :lua require'telescope.builtin'.builtin()<CR>
" useful?
" - git files (to exclude build files, for example. or can set .rgignore...)
" - commands

" also: might want a way to change dir on-demand. see: 
" - https://github.com/nvim-telescope/telescope.nvim/issues/2201 or https://www.reddit.com/r/neovim/comments/y7xhll/introducing_dirtelescopenvim_find_or_grep_in_a/
" - manual way: :Telescope live_grep search_dirs=
" }}}

""""""""""""""""""""""""""""""""
" VSCODE {{{
""""""""""""""""""""""""""""""""
if exists('g:vscode')
    " note: commands take all sorts of args. see below. but i couldn't figure
    " out vscode.open in the end. huh.
    " call VSCodeNotify('workbench.action.findinfiles', { 'query': expand('<cword>')})
    " call VSCodeNotify('workbench.extensions.search', 'vim')

    " commentary.
    xmap gc  <plug>VSCodeCommentary
    nmap gc  <plug>VSCodeCommentary
    omap gc  <plug>VSCodeCommentary
    nmap gcc <plug>VSCodeCommentaryLine

    " harpoon
    nnoremap mj <Cmd>call VSCodeNotify('vscode-harpoon.addEditor1')<CR>
    nnoremap mk <Cmd>call VSCodeNotify('vscode-harpoon.addEditor2')<CR>
    nnoremap ml <Cmd>call VSCodeNotify('vscode-harpoon.addEditor3')<CR>
    nnoremap m; <Cmd>call VSCodeNotify('vscode-harpoon.addEditor4')<CR>

    nnoremap <c-j> <Cmd>call VSCodeNotify('vscode-harpoon.gotoEditor1')<CR>
    nnoremap <c-k> <Cmd>call VSCodeNotify('vscode-harpoon.gotoEditor2')<CR>
    nnoremap <c-l> <Cmd>call VSCodeNotify('vscode-harpoon.gotoEditor3')<CR>
    " must map <c-;> in vscode, as vim can't recognize <c-;>

    nnoremap <leader>a <Cmd>call VSCodeNotify('vscode-harpoon.addEditor')<CR>
    nnoremap <c-p> <Cmd>call VSCodeNotify('vscode-harpoon.editEditors')<CR>
endif
" }}}
