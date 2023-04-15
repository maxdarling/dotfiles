" Note: this plugin requires global variable persistence via :h viminfo-!
" To do so, simply set the following in your vimrc (below 'set nocompatible').
"   set viminfo+=!

" trident philosophy: switching between the most common 4 files deserves 1
" keystroke on the homerow. No more fuzzy finder switching, no more bp/bn!

" creds: this plugin idea was inspired by harpoon by the primagen. However,
" it's only on neovim! :). And, I wanted to hack my own for fun.

" suggested keybinds (qwerty):
" nnoremap mj :call TridentSetMapping('j')<CR>
" nnoremap mk :call TridentSetMapping('k')<CR>
" nnoremap ml :call TridentSetMapping('l')<CR>
" nnoremap m; :call TridentSetMapping(';')<CR>
" nnoremap <C-j> :call TridentJumpToFile('j')<CR>
" nnoremap <C-k> :call TridentJumpToFile('k')<CR>
" nnoremap <C-l> :call TridentJumpToFile('l')<CR>
" nnoremap <C-;> :call TridentJumpToFile(';')<CR>
" above: vim doesn't recognize <C-;>. up to you how to map this. I've chosen
" a useless Mac key sequence (Option+s) that vim recognizes, and then I have iterm
" configured to transform Ctrl+; to Option+s.
" also: to replace <C-l> redraw screen (and clear search highlight, which
" everyone should use...), <C-h> works. It's a very common action for me, so
" I've kept it on the homerow.
" nnoremap <silent> <C-h> :<C-u>nohlsearch<CR><C-l>
let g:TRIDENT_MAP = get(g:, 'TRIDENT_MAP', {})

function! TridentSetMapping(char)
    let g:TRIDENT_MAP[a:char] = expand('%:p')
endfunction

" jump to the file mapped to 'char'.
" if 'avoid_duplicate_views' is set and the destination file is open in
" another window (in any tab), jumping will change to that window instead of
" creating a duplicate view. 
function! TridentJumpToFile(char, avoid_duplicate_views = 1)
    if !has_key(g:TRIDENT_MAP, a:char)
        echom "Error: no trident mapping set for key " .. a:char
        return
    endif

    let f = g:TRIDENT_MAP[a:char]
    let winIDList = win_findbuf(bufnr(f))
    if a:avoid_duplicate_views && !empty(winIDList)
        " goto first window in list (I guess...)
        call win_gotoid(winIDList[0])
        echom "trident SWAPPED to window"
    else
        execute 'edit' f
        echom "trident JUMPED to file directly"
    endif
endfunction

" OLD:
" function! TridentJump(mark_char)
"     " strategy: global marks are persisted by default! (see :h viminfo).
"     " so, simply use global marks for {jkl;} with 3 tweaks:
"     " 1. caps by default (e.g. mj -> mJ)
"     " 2. jump via ctrl-chord (e.g. <C-j> instead of `j) 
"     " 3. marks take you back to your last curpos in the file.
"     " thus, the semantics are not 'jump to mark', but 'jump to file'

"     " jump to the mark, OR if the associated file is already open in a window,
"     " simply switch to that window.
"     " note: redir not needed, I found 'getmarklist()' in :h function-list
"     redir => cout
"     silent marks
"     redir END
"     let list = split(cout, "\n")
"     " todo: 
"     " - read vim pager help (curiosity)
"     " - read redir help
"     " - meat: parse this w/ regex

"     execute "normal! " ..  "\`" .. a:mark_char

"     " make an autocmd for the current file to update the mark on exit
"     let curfile = expand('%:p')
"     execute "augroup trident" .. a:mark_char
"     au!
"     execute 'autocmd BufWinLeave ' .. curfile .. ' normal! m' .. a:mark_char
"     execute 'autocmd BufWinLeave ' .. curfile .. ' echom "updated mark for file ' .. a:mark_char .. '"'
"     augroup END
" endfunction
