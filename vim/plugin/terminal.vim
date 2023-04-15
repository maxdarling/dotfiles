" philosophy: quickly pop out to a tab to run some commands. I might switch
" back and forth using 'F3'. when I want to exit the terminal, <C-w>c closes
" everything.
" pros over iterm tabs:
" - 1 vim instance, so registers, etc are preserved
" - '<F3>' swapping is lower overhead than directional Cmd chords.
" - terminal shares vim cwd
" cons:
" - must commit my 'F3' key
" - current vim workflow is somewhat specialized/brittle 
function! CreateLiteTerm(usetab)
    if a:usetab
        tab terminal
    else
        terminal
    endif
    set bufhidden=wipe
    set nobuflisted
endfunction
" OLD:
" augroup term_
"     au!
"     " - aggressively close terminals with <C-w>
"     " - hide terminals in buffer list (e.g. <C-c> won't switch back)
"     au TerminalWinOpen * if &buftype == 'terminal' 
"         \ | set bufhidden=wipe
"         \ | set nobuflisted
"         \ | endif
" augroup END
