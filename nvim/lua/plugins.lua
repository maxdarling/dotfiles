return require('packer').startup(function(use)
    use 'wbthomason/packer.nvim'
    use 'tpope/vim-commentary'
    use 'tpope/vim-surround'

    -- misc
    use {
        'glacambre/firenvim',
        run = function() vim.fn['firenvim#install'](0) end 
    }
    use 'ThePrimeagen/vim-be-good'
end)
