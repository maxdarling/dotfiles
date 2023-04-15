require('plugins')

-- base commands, shared by vim
vim.cmd('source ~/.vim/base_config.vim')

vim.cmd('nnoremap <leader>ev :e ~/.config/nvim/init.lua<CR>')
vim.cmd('nnoremap <leader>ep :e ~/.config/nvim/lua/plugins.lua<CR>')

vim.cmd('colorscheme slate')

-- VSCode stuff
if vim.g.vscode then
    -- harpoon
    vim.cmd[[
        nnoremap mj <Cmd>call VSCodeNotify('vscode-harpoon.addEditor1')<CR>
        nnoremap mk <Cmd>call VSCodeNotify('vscode-harpoon.addEditor2')<CR>
        nnoremap ml <Cmd>call VSCodeNotify('vscode-harpoon.addEditor3')<CR>
        nnoremap m; <Cmd>call VSCodeNotify('vscode-harpoon.addEditor4')<CR>

        nnoremap <C-j> <Cmd>call VSCodeNotify('vscode-harpoon.gotoEditor1')<CR>
        nnoremap <C-k> <Cmd>call VSCodeNotify('vscode-harpoon.gotoEditor2')<CR>
        nnoremap <C-l> <Cmd>call VSCodeNotify('vscode-harpoon.gotoEditor3')<CR>
        " note: below must be mapped in VSCode, as vim can't recognize <C-;>
        nnoremap <C-;> <Cmd>call VSCodeNotify('vscode-harpoon.gotoEditor4')<CR>

        nnoremap <C-p> <Cmd>call VSCodeNotify('vscode-harpoon.editEditors')<CR>
    ]]
end

