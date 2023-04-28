" coc toggle
let g:md_coc_toggle_on=0
function! ToggleCoc()
    let g:md_coc_toggle_on = !g:md_coc_toggle_on
    if (g:md_coc_toggle_on)
        CocStart
        CocEnable
    else
        CocDisable
    endif
endfunction

" quickfix list toggle
let g:quickfix_is_open = 0
function! QuickfixToggle()
    if g:quickfix_is_open
        cclose
        let g:quickfix_is_open = 0
        execute g:quickfix_return_to_window .. "wincmd w"
    else
        let g:quickfix_return_to_window = winnr()
        copen
        let g:quickfix_is_open = 1
    endif
endfunction

" toggle trailing whitespace highlight (invalidated by listchars setting...) 
let g:md_toggle_tr_whitespace_highlight = 0
function! ToggleTrailingWhitespaceHighlight()
    let g:md_toggle_tr_whitespace_highlight =
      \ !g:md_toggle_tr_whitespace_highlight
    if g:md_toggle_tr_whitespace_highlight
        execute "match Error " .. '/\v\s+$/'
    else
        execute "match Error //"  
    endif
endfunction

" fold column toggle
function! FoldColumnToggle()
    if &foldcolumn
        setlocal foldcolumn=0
    else
        setlocal foldcolumn=4
    endif
endfunction

" cleanup all inactive bufs
" source:
" https://stackoverflow.com/questions/1534835/how-do-i-close-all-buffers-that-arent-shown-in-a-window-in-vim
function! DeleteInactiveBufs()
    "From tabpagebuflist() help, get a list of all buffers in all tabs
    let tablist = []
    for i in range(tabpagenr('$'))
        call extend(tablist, tabpagebuflist(i + 1))
    endfor

    "Below originally inspired by Hara Krishna Dara and Keith Roberts
    "http://tech.groups.yahoo.com/group/vim/message/56425
    let nWipeouts = 0
    for i in range(1, bufnr('$'))
        if bufexists(i) && !getbufvar(i,"&mod") && index(tablist, i) == -1
        "bufno exists AND isn't modified AND isn't in the list of buffers open in windows and tabs
            silent exec 'bwipeout' i
            let nWipeouts = nWipeouts + 1
        endif
    endfor
    echomsg nWipeouts . ' buffer(s) wiped out'
endfunction

" useless...?
function! NewiTermTabInCwd()
    exe "!osascript" ..
                \" -e \'tell application \"iTerm\" to activate\'" ..
                \" -e \'tell application \"System Events\" to tell " ..
                \"process \"iTerm\" to keystroke \"t\" using command down\'" ..
                \" -e \'tell application \"System Events\" to tell process " ..
                \"\"iTerm\" to keystroke \"cd " .. expand("%:p:h") .. "\"\'"
endfunction

