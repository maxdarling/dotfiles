-- firenvim config (source: https://github.com/glacambre/firenvim)
vim.g.firenvim_config = {
    -- globalSettings = { alt = "all" },
    localSettings = {
        [".*"] = {
            -- cmdline  = "neovim",
            -- content  = "text",
            -- priority = 0,
            -- selector = "textarea",
            takeover = "never"
        }
    }
}
