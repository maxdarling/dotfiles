" section motions use top-level function curly braces
" (note: this is copied from the suggestion in ':h section')
" noremap <buffer> <silent> [[ ?{<CR>w99[{
" noremap <buffer> <silent> ][ /}<CR>b99]}
" noremap <buffer> <silent> ]] j0[[%/{<CR>
" noremap <buffer> <silent> [] k$][%?}<CR>

" exercise (to delete)
function! s:NextSection(type, backwards, visual)
    if a:visual
        normal! gv
    endif

    if a:type == 1
        let pattern = '\v(\n\n^\S|%^)'
        let flags = 'e'
    elseif a:type == 2
        let pattern = '\v^\S.*\=.*:$'
        let flags = ''
    endif

    if a:backwards
        let dir = '?'
    else
        let dir = '/'
    endif

    execute 'silent normal!' .. dir .. pattern .. dir .. flags .. "\r"
endfunction

noremap <script> <buffer> <silent> ]]
            \ :call <SID>NextSection(1, 0)<cr>
noremap <script> <buffer> <silent> [[
            \ :call <SID>NextSection(1, 1)<cr>
noremap <script> <buffer> <silent> ][
            \ :call <SID>NextSection(2, 0)<cr>
noremap <script> <buffer> <silent> []
            \ :call <SID>NextSection(2, 1)<cr>


if !exists("g:potion_command")
    let g:potion_command = "echo"
endif

function! PotionCompileAndRunFile()
    silent !clear
    execute "!" . g:potion_command . " " . bufname("%")
endfunction

function! DoStuff()
    let output = split(system("date"), '\n')

    " re-use the 'temp' window if it's open. otherwise, create a new window.
    let temp_winnr = bufwinnr('temp')
    if temp_winnr != -1
        execute temp_winnr .. 'wincmd w'
    else
        sp temp
    endif

    " prepare file (delete, set scratch filetype)
    normal! ggdG
    setlocal buftype=nofile

    call append(0, output)
endfunction

