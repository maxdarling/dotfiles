function! Sorted(l)
    let new_list = deepcopy(a:l)
    call sort(new_list)
    return new_list
endfunction

function! Reversed(l)
    let new_list = deepcopy(a:l)
    call reverse(new_list)
    return new_list
endfunction

function! Append(l, val)
    let new_list = deepcopy(a:l)
    call add(new_list, a:val)
    return new_list
endfunction

" works on lists or dictionaries ("obj")
" for list, key is the idx
function! Assoc(obj, key, val)
    let new_obj = deepcopy(a:obj)
    let new_obj[a:key] = a:val
    return new_obj
endfunction
" let list = [3, 2, 4, 1]
" let dict = {1:'a', 2:'b'}
" echom Assoc(list, 2, 5)
" echom Assoc(dict, 2, 5)
" echom Assoc(list, 6, 5)
" echom Assoc(dict, 6, 5)

" works on dict/list
function! Pop(obj, i)
    let new_obj = deepcopy(a:obj)
    call remove(new_obj, a:i)
    return new_obj
endfunction
" let list = [3, 2, 4, 1]
" let dict = {1:'a', 2:'b'}
" echom Pop(list, 2)
" echom Pop(dict, 2)

" funcref example:
"
" let Myfunc = function("Append")
" echo Myfunc([1, 2], 3)
" let funcs = [function("Append"), function("Pop")]
" echo funcs[1](['a', 'b', 'c'], 1)

" ========== higher-order ==========

" works on list/dict
function! Mapped(fn, obj)
    if type(a:obj) ==# v:t_list
        let new_list = deepcopy(a:obj)
        call map(new_list, string(a:fn) . '(v:val)')
        return new_list
    elseif type(a:obj) ==# v:t_dict
        let new_dict = deepcopy(a:obj)
        call map(new_dict, string(a:fn) . '(v:key, v:val)')
        return new_dict
    endif
endfunction
" let mylist = [[1, 2], [3, 4]]
" echo Mapped(function("Reversed"), mylist)
" let dict = {'a':1, 'b':2}
" echo Mapped({key, value -> value .. '-' .. key}, dict)
" echo Mapped({key, value -> key }, dict)

" works on list/dict
function! Filtered(fn, obj)
    if type(a:obj) ==# v:t_list
        let new_list = deepcopy(a:obj)
        call filter(new_list, string(a:fn) . '(v:val)')
        return new_list
    elseif type(a:obj) ==# v:t_dict
        let new_dict = deepcopy(a:obj)
        call filter(new_dict, string(a:fn) . '(v:key, v:val)')
        return new_dict
    endif
endfunction
" let mylist = [[1, 2], [], ['foo'], []]
" echo Filtered(function('len'), mylist)
" let dict = {'a':1, '2b':2, 'c':0}
" echo Filtered({key, value -> key}, dict)
" echo Filtered({key, value -> value}, dict)

" works on list/dict
function! Removed(fn, obj)
    if type(a:obj) ==# v:t_list
        let new_list = deepcopy(a:obj)
        call filter(new_list, '!' . string(a:fn) . '(v:val)')
        return new_list
    elseif type(a:obj) ==# v:t_dict
        let new_dict = deepcopy(a:obj)
        call filter(new_dict, '!' . string(a:fn) . '(v:key, v:val)')
        return new_dict
    endif
endfunction
" let mylist = [[1, 2], [], ['foo'], []]
" echo Removed(function('len'), mylist)
" let dict = {'a':1, '2b':2, 'c':0}
" echo Removed({key, value -> key}, dict)
" echo Removed({key, value -> value}, dict)

" like `reduce()`, except it's functional-style and returns a deepcopy.
" also, this accepts dicts by using their values by default
function! Reduced(acc, obj, ...)
    let values = (type(a:obj) ==# v:t_dict? values(a:obj) : a:obj)
    return a:0 == 0 ? 
           \ reduce(values, a:acc) : 
           \ reduce (values, a:acc, a:1)
endfunction
" let mylist = [1, 2, 3, 4]
" echo Reduced({sum, val -> sum + val}, mylist, 5)
" let dict = {'a':1, '2b':2, 'c':0}
" echo Reduced({sum, val -> sum + val}, dict)
