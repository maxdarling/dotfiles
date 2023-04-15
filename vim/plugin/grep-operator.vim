nnoremap <expr> <Plug>GrepOperatorNormalMode <SID>GrepOperatorNormalMode()
vnoremap <Plug>GrepOperatorVisualMode :<c-u>call <SID>GrepOperatorVisualMode()<CR>
" note: I originally had this as:
"   nnoremap <leader>g :set operatorfunc=<SID>GrepOperator<CR>g@
"
" however, I wanted to define the mapping from my Vimrc, but define a global
" function. To achieve this, I'm using <Plug>, which is the Right Way (based
" on looking at surround.vim source, for example).

function! s:GrepOperatorNormalMode()
    set operatorfunc=<SID>GrepOperator
    return 'g@'
endfunction

function! s:GrepOperatorVisualMode()
    call <SID>GrepOperator(visualmode())
endfunction


function! s:GrepOperator(type)
    let saved_unamed_reg = @@

    if a:type ==# 'v'
        normal! `<v`>y
    elseif a:type ==# 'char'
        normal! `[v`]y
    else
        return
    endif

    silent execute "grep! -R " .. shellescape(@@) .. " ."
    copen

    let @@ = saved_unamed_reg
endfunction
