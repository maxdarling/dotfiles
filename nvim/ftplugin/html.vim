" vim by defaults sources html ftplugin for markdown files. this aborts.
" see $VIMRUNTIME/ftplugin/markdown.vim for full details
if &ft=="markdown"
  finish
endif

function! OpenHtmlInBrowser()
    let fname = expand('%')

    if match(expand('%:p'), "maxdarling.github.io") != -1
        let fname = 'http://localhost:8000/' .. expand('%:p:s?^.*maxdarling.github.io/??')
    endif

    exe ':silent !open -a /Applications/Google\ Chrome.app ' .. fname
    redr!
endfunction

map <silent><buffer> <leader>x :call OpenHtmlInBrowser()<CR>

