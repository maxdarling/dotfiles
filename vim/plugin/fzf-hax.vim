" how to go up directory in fzf popup. issue: (https://github.com/junegunn/fzf.vim/issues/338#issuecomment-623087034)

" nit: dont' actually want to cd, though.
function! FzfFilesUppable(dir)
  if empty(a:dir)
    let dir = getcwd()
  else
    let dir = a:dir
  endif
  let parentdir = fnamemodify(dir, ':h')
  let spec = fzf#wrap(fzf#vim#with_preview({'options': ['--expect', 'ctrl-u'] }))

  " hack to retain original sink used by fzf#vim#files
  let origspec = copy(spec)

  unlet spec.sinklist
  unlet spec['sink*']
  function spec.sinklist(lines) closure
    if len(a:lines) < 2
      return
    endif
    if a:lines[0] == 'ctrl-u'
      call TFile(parentdir)
    else
      call origspec.sinklist(a:lines)
    end
  endfunction
  call fzf#vim#files(dir, spec)
endfunction
command! -nargs=* FzfFilesUppable call FzfFilesUppable(<q-args>)

