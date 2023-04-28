-- attempting to fix indentation issue
-- https://github.com/nvim-treesitter/nvim-treesitter/issues/1573, https://github.com/NvChad/NvChad/issues/1591
M.treesitter = {
   ensure_installed = {
     "vim",
     "lua",
     "python",
   },
   indent = {
     enable = true,
     disable = {"python"}
   },
 }
