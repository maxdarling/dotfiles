" evaluate entire buffer as vimscript
" note: there's probably a much better way to do this...
" todo: does ':source %' do the same thing...?
" map <buffer> <F3> :execute "normal! GVggy\r:@\"\r"<CR>
map <buffer> <F3> :w<CR>:source<CR>

