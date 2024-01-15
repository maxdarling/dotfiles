""""""""""""""""""""""""""""""""""
" PLUGINS {{{
""""""""""""""""""""""""""""""""""
call plug#begin()
" core
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'

" treesitter
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'nvim-treesitter/nvim-treesitter-textobjects'
" Plug 'nvim-treesitter/nvim-treesitter-context'

"floating term
Plug 'voldikss/vim-floaterm'

" colorscheme
Plug 'ellisonleao/gruvbox.nvim'

" statusline
Plug 'nvim-lualine/lualine.nvim'
Plug 'nvim-tree/nvim-web-devicons'

" extra
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
" - replace linux penguin in statusline with a lambda or xah sigma (my symbol)
" - learn to use treesitter folding (e.g. fold all functions)
" - lsp setup? for python?
" }}}

""""""""""""""""""""""""""""""""""
" TESTING {{{
""""""""""""""""""""""""""""""""
" delete without affecting the default register
noremap <leader>d "_d

" one key to pop to/from a terminal
noremap <silent> <C-0> :FloatermToggle<CR>
tnoremap <silent> <C-0> <C-\><C-n>:FloatermToggle<CR>
" tnoremap <Esc> <C-\><C-n>
" set winblend=5 " testing
hi Floaterm guibg=#282828
hi FloatermBorder guibg=#282828 guifg=#25de14

" free up q. it's not very common. it should require a leader.
" noremap <leader>q q

" format entire file not that good. often makes massive edits. better '=ap'
" nmap <leader>f <Cmd>normal! mzgg=gG`z<CR>
" }}}

"""""""""""""""""""""""""""""""""
" GENERAL {{{
"""""""""""""""""""""""""""""""""
set background=dark
colorscheme gruvbox

" utils (~/.config/nvim/plugin/utils.vim)
command! Bdi :call DeleteInactiveBufs()

" NOTE: vscode-neovim maps several commands like :e, :w so that VSCode can understand
" them. So our mappings that use them must be ~~recursive~~.
nmap <leader>ev :e ~/.config/nvim/init.vim<CR>
nmap <leader>ep :e ~/.config/nvim/plugin/<CR>
nmap <leader>ez :e ~/.zshrc<CR>
"map s <C-w><C-w>
map r <C-^>
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
