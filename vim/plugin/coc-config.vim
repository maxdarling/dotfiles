let g:coc_start_at_startup=v:false
let g:coc_ext_allowlist = ['py']

set updatetime=300
set signcolumn=number
set nobackup
set nowritebackup

autocmd CursorHold * silent call CocActionAsync('highlight')

function! s:disable_coc_for_type()
    if index(g:coc_ext_allowlist, expand('%:e')) == -1
        let b:coc_enabled = 0
    endif
endfunction

augroup Coc
    autocmd!
    " enable CoC by file
    autocmd BufNew,BufRead * call s:disable_coc_for_type()
    " Setup formatexpr specified filetype(s)
    autocmd FileType python setlocal formatexpr=CocAction('formatSelected')
    " Update signature help on jump placeholder
    autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end
