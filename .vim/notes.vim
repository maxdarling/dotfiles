" HOMEROW CTRL KEYS AUDIT: 
" - a - incr number - B tier
" - s - None
" - d - down. S-tier
" - f - aggressive C-d - F tier
" - g - display filename(?) - C tier
" - h - h - F tier
" - j - j - F tier
" - k - None
" - l - redraw + clear search - A tier
" - ; - None
"   ;
"
" decision: 
" - h -> l 
" - jkl; -> harpoon keys. 
" - f -> u (for c-d/c-u adjacency)


" plugin ideas:
" - harpoon, but for Vim
" - fuzzy find over all user-defined functions (apparently possible with 
"   the Unite plugin (:Unite command)
" - code templates. useful for many languages, esp if you need a refresher,
"   e.g. bash or file I/O. Imagine having a file I/O example for every
"   language! Tight. And competetive programmers copy-paste templates around
"   like this iirc. Cool to integrate it into the editor. For UI, might want a
"   2-step fuzzy search: language -> code type. e.g. 'python -> file I/O'.
" - chat GPT integration. it's all text, baby! you can query from vim and get
"   your answer back in a buffer or something. this could generalize the
"   entire code template plugin above, btw... :) (example link:
"   https://www.youtube.com/watch?v=4oUrm4CnIjo) (other:
"   https://www.youtube.com/watch?v=SL-nNOjqoxg) (another: 
"   https://github.com/dpayne/CodeGPT.nvim)
" - helps you see opportunities for remaps. records keystrokes in different modes
"   and gives at-a-glance stats. most often and least often keys are the 
"   interesting ones. I.e. people might notice they never use the 's' key in 
"   normal mode, or on the flipside, press <C-w><C-w> a ton (as I did). I'd
"   like this for myself, so why not make it good enough for others!
