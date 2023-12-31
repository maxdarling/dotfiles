lua require('plugins')
lua require('firenvim-config')

" base commands, shared by vim
source ~/.vim/base_config.vim

" utils (~/.config/nvim/plugin/utils.vim)
command! Bdi :call DeleteInactiveBufs()

" vscode-neovim maps several keys like :e, :w so that VSCode can understand
" them. So our mappings that use them must be ~~recursive~~.
nmap <leader>ev :e ~/.config/nvim/init.vim<CR>
nmap <leader>ep :e ~/.config/nvim/lua/plugins.lua<CR>
nmap <leader>eb :e ~/.bash_profile<CR>
map s <C-w><C-w>
map r <C-^>

" free up q on maCRo. it's not very common. it should require a leader.
noremap <leader>q q

" NEW MAPS
" 'm' too valuable for mark. make it comment. 
" actually, bad. needs to be left hand, since right hand is nav-ing.
" try <C-f> or <leader>f
nmap <C-f> gcc
vmap <C-f> gc
nmap <leader>f gcc
vmap <leader>f gc
" format entire file not that good. often makes massive edits. better '=ap'
" nmap <leader>f <Cmd>normal! mzgg=g`z<CR>

if !exists('g:vscode')
    echom "started not in vscode!"
    colorscheme slate
    set cursorline
endif

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
