;; -*- lexical-binding: t; -*-
(evil-set-leader '(normal visual motion) (kbd "SPC"))
(evil-set-leader 'emacs (kbd "C-SPC")) ;; not great on my keyboard...
(setopt evil-want-Y-yank-to-eol t)

;; C-o prefix
(define-prefix-command 'my/c-o-map)

;; philosophy (top-level)
;; - speed + ergonomics
;; - consider omni-modal vs. vim-only
;;   - e.g. embrace C- and s- and consider SPC leader for vim-only
;; - consider conflicts in other workflows, e.g:
;;   - terminal, GDocs, etc: emacs commands C-k/a/e are essential
;;
;; reflections (start of phase 1) - C-k and C-i are likely better than
;; "r" and "h". the latter were a bold play to make the most-common
;; actions 1-keystroke, which i stand by in theory. the issue is that
;; it doesn't work in some modes: 1. insert-always modes like terminal
;; (can't think of other examples) and 2. <E>-always modes like magit
;; (and more examples likely to follow over time). sure, you can C-x o
;; your way out in those cases, but that lack of uniformity actually
;; bugs me more I think. I hold a subconscious of those modes because
;; of it - they feel foreign and unwieldy to me.  And then there's
;; considering the next point: C-i/k is basically as comfy as h/r.  an
;; L/R chord is pretty decent. specifically C- because my thumb hovers
;; that by default when i'm in navigation-mode via C-d/u/e/y, and of
;; course spacebar right below it when i'm in typing mode. it's very
;; natural. cneitahicneainsetaicseaticens


;; phase 1 (start 2/11/26, comfy ??/??/??)
;; - remove C-o and C-i. too vim-specific. I like s-<left/right>.
;; - bind C-o (strongest available key) as a prefix for core stuff (fzf file/dir, grep, etc)
;; - revamp "h" to be pan-modal (C-i)
;; - revamp "r" to be pan-modal (C-k OR C-t)
;; - change inc (C-[S-]a) to (C-[S-]p)
;; - force phase 1 changes on cursor, too :)

;; todo (habits):
;; - use C-l more for centering screen on cursor when scrolling up/down

;; todo:
;; - vterm conf (enable C-c, decide C-k, ...)
;; - M-<L/R> default in normal mode (and working in term) , winner to C-<L/R>
;; - unbind h, r (and restore help mode defaults?)
;; - sift though C-x to understand groupings
;; - read up on C-c conventions in different modes (e.g. C-c C-c is "apply changes") 
;; - more vs-code and browser unification
;;   - s-n for new empty file and s-S-n for new frame ("window")
;;   - ...
;; - cleanup xah-bound keys
;;   - i can keep many to look at for inspo, but i should put somewhere out of the way.
;; - "all modes" contains many leader sequences that are not relevant for
;; emacs state. i should move them to the next section "normal mode". I likely
;; confused the sections as one before(?).
;; - switch to C-s for search (this frees n for magit and other "n/p" modes)

;; abandoned:
;; - scrolling revamp
;;   - C-e and C-y for single line scroll, and C-u and C-d for multi line
;;   - why not just combine the above and use shift for the less common one?
;;   - it's more divergence from vim, but who cares. i'm on engram for the long haul.
;;   - cleanup:
;;     - C-a and C-S-a are inc. but too good a key for that. C-y is fine.
;;     - C-u/C-d free (keep C-u default)
;;   - reason abandoned: minor bonus to free up some keys in exchange for slightly but
;;   meaningfully more awkward binds for core actions. plus minor con in defying vim
;;   defaults. plenty of benefit in just scrapping C-a and putting universal arg on C-'

;; available keys:
;; - C (L): C-a, C-b, C-,, ...
;; - C (R): C-t, C-n, C-q, C-m, C-f
;; - S: s-LOTS

;; key replacement ideas:
;; - r and C-r.
;;   - why are u and C-r undo/redo? weird. i could map r to redo and then
;;   free up C-r for something juicy.
;; - C-h
;;   - it's core in emacs, but only as a newb now. in theory it should be less important.
;;   i should power-finger real estate for something more important and universal! 

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key* '(normal visual motion emacs insert) 'global
  ;; philosophy:
  ;; - be mindful of hand-balance for chords. C is on R side, s is on left. most of the time
  ;; it feels better to press modifier on one hand and actual keys on the other.

  (kbd "s-p") 'switch-to-buffer ;; todo: use consult for vs-code style behavior
  (kbd "s-P") 'execute-extended-command

  (kbd "<control-i>") 'evil-window-next
  (kbd "C-t") 'evil-buffer ;; testing
  (kbd "s-e") 'evil-buffer ;; testing
  ;; (kbd "C-k") 'evil-buffer 

  (kbd "C-o") my/c-o-map

  (kbd "s-s") 'save-buffer
  (kbd "s-d") 'kill-buffer ;; this kinda sucks. default should be not have to press RET
  ;; below doesn't work - maps to "s-D"
  ;; (kbd "s-S-d") (lambda () (interactive)
  ;; 		  (save-buffer) (kill-buffer) (message "Saved and killed buffer."))


  ;; testing: harpoon
  ;; notes:
  ;; - i'm totally used to the visual indicators in vscode. hmm. but i could memorize e.g. for
  ;; emacs dotfiles the seq: init, keymaps, mode keymaps, or something.
  ;; - i should probably push myself to memorize for a couple weeks and see how it goes.
  ;; - todo: C-c C-c as a common "apply changes" pattern on a virtual file a la. wgrep, wdired, magit.
  ;; but i'd need to make a major mode? Why not?!
  ;; - make it appear as a popup buffer in the center of the screen? or the minibuffer at least?
  ;; upper left is uncomfortable for me. and modal better signifies that it's for quick changes.
  ;;   - todo: quick win: make it appear in a tiny window at bottom, mimicking the minibuffer
  (kbd "C-c h a") 'harpoon-add-file
  (kbd "C-c h h") 'harpoon-toggle-file
  (kbd "C-c h f") 'harpoon-toggle-quick-menu
  (kbd "C-1") 'harpoon-go-to-1
  (kbd "C-2") 'harpoon-go-to-2
  (kbd "C-2") 'harpoon-go-to-2
  (kbd "C-3") 'harpoon-go-to-3
  (kbd "C-4") 'harpoon-go-to-4
  (kbd "C-5") 'harpoon-go-to-5
  (kbd "C-6") 'harpoon-go-to-6
  )

(define-key my/c-o-map (kbd "C-c") #'my/find-dir-rec)
(define-key my/c-o-map (kbd "C-u") #'my/find-file-rec)
(define-key my/c-o-map (kbd "C-g") #'grep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-emacs state
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key '(normal visual motion insert) 'global
  ;; quandry: *most* <E> modes are readonly. but the terminal is an annoying exception (side note,
  ;; could i use vterm in insert mode?).
  ;;
  ;; this leads to conflicts. e.g. I like M-<left/right> as an "editing-style" command in terminal
  ;; (<E> state) as well as all other editing situations. but there may be a mode such as magit
  ;; (<E> state) where such an editing-style command is bound to something useful (e.g. M-<up/down>
  ;; for git rebase).
  ;;
  ;; what is the solution?
  ;; 1. divide commands into "editing" and "navigation".
  ;; 2. only bind nav commands to <E> state (this is solving for the common case)
  ;; 3. if there's ever a situation where you're in an <E> state and you want editing commands,
  ;; you fucked up (or, it's a valid exception like vterm, and the solution is to setup binds for that
  ;; mode ad-hoc).

  (kbd "M-<down>") 'my/move-text-down
  (kbd "M-<up>") 'my/move-text-up
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-insert modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key '(normal visual motion emacs) 'global
  (kbd "C-u")'evil-scroll-up
  (kbd "C-\'") 'universal-argument ;; testing. 

  ;; philosophy:
  ;; - SPC leader in <E> is good/harmless. can rebind or use another way if it overwrites something
  ;; useful (e.g. "dc" == SPC in magit). Note: <E>-specific leader combos might be desirable
  ;; one day, but I'm ingoring that for now, and assuming sharing across all states is fine.
  ;; - non-leader binds: good for consistency but bad if you have to rebind. I'm hesitant to rebind
  ;; "h". But SPC is worth it. <C-u> and <C-d> are likely worth it. Etc. I only really use <E> in
  ;; Info and magit currently, so it's not a huge deal, but that might change.
  ;; - note: editing commands aren't needed in <E>. I'll put those in the <NVM> map where
  ;; convenient (e.g. if there's just 1 editing command that fits nicely into a group, cleaner to
  ;; keep it)

  ;; * Core Nav *
  ;; (kbd "s-,") (lambda () (switch-to-buffer my/init-file))
  (kbd "s-,") (lambda () (interactive) (switch-to-buffer "init.el")) ;; todo: fix
  (kbd "s-<left>") 'evil-jump-backward  ;; testing
  (kbd "s-<right>")  'evil-jump-forward ;; testing

  (kbd "<leader>q") 'evil-record-macro ;; ballsy?! but quit window >often than creating macro.

  ;; * Windows/Frames *
  (kbd "M-s-<left>") 'winner-undo
  (kbd "M-s-<right>") 'winner-redo
  ;; ...
  (kbd "<leader>ws") 'evil-window-split
  (kbd "<leader>wv") 'evil-window-vsplit
  (kbd "<leader>wc") 'evil-window-delete

  ;; * Buffers/Files *
  (kbd "<leader>e") 'switch-to-buffer
  (kbd "<leader>E") 'switch-to-buffer-other-window ;; ideally, i could press C-o or S-RET in the minibuffer. embark?
  (kbd "<leader>a") 'find-file
  (kbd "<leader>o") 'bookmark-jump
  (kbd "<leader>d") 'dired-jump
  (kbd "<leader>D") 'dired-jump-other-window
  (kbd "<leader>x") 'my/run-current-file
  ;;
  ;; ...
  (kbd "<leader>ir") 'bookmark-set
  (kbd "<leader>im") 'bookmark-bmenu-list
  ;; ...
  ;; ...
  (kbd "<leader>in") 'xah-new-empty-buffer  
  ;; ;; ...
  ;; 'xah-open-last-closed
  ;; 'xah-list-recently-closed
  (kbd "<leader>iw") 'recentf
  ;; ;; ...
  (kbd "<leader>i1") 'xah-open-in-terminal
  ;; xah-show-in-desktop
  ;; ;; ...
  (kbd "<leader>ic") 'xah-copy-file-path

  ;; * Common *
  (kbd "<leader>ti") 'query-replace-regexp ;; todo: incremental replace preview like :%s
  (kbd "<leader>tt") 'repeat-complex-command
  (kbd "<leader>tl") 'list-matching-lines
  (kbd "<leader>tv") 'delete-matching-lines
  ;; ...
  (kbd "<leader>tu") 'abbrev-mode
  ;; todo: abbrev selector

  ;; * Edits/Insertions + Formatting *
  (kbd "<leader>b\"") 'xah-escape-quotes
  (kbd "<leader>b\\") 'xah-space-to-newline
  (kbd "<leader>bl") 'xah-quote-lines
  (kbd "<leader>bw") 'xah-remove-whitespace
  ;; ...
  (kbd "<leader>bd") 'xah-insert-date
  ;; 'format dwim
  ;; ...
  (kbd "<leader>bf") 'xah-show-formfeed-as-line ;; note: should be on almost always

  ;; * Evaluations *
  (kbd "<leader>kh") 'my/run-current-file
  (kbd "<leader>kf") 'eval-buffer
  (kbd "<leader>kd") 'eval-defun
  (kbd "<leader>kl") 'eval-last-sexp
  (kbd "<leader>ks") 'eval-expression
  (kbd "<leader>kr") 'eval-region
  ;; (kbd "<leader>kz") 'xah-run-current-file
  )

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Normal mode
;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key '(normal visual motion) 'global
  ;; * Core *
  "h" 'comment-line
  "H" 'my/comment-and-duplicate
  ;; "h" 'evil-window-next
  ;; "r" 'evil-buffer
  "q" 'quit-window
  "-" 'delete-other-windows
  "s" 'avy-goto-char-timer
  "S" 'avy-goto-line ;'avy-goto-char
  "l" 'execute-extended-command ;; testing.
  "L" 'evil-ex
  ":" 'repeat-complex-command ;; testing. idea: mirrors the '.' command

  (kbd "C-p") 'my/inc-number-after-point
  (kbd "C-S-p") (lambda() (interactive) (my/inc-number-after-point (- 1)))
  (kbd "C-@") (lambda() (interactive) (set-buffer-modified-p nil) (kill-buffer)) ;; forcibly delete buffer

  ;; * Commenting + Indentation *
  (kbd "<leader>0") (lambda () (interactive) (indent-region (point-min) (point-max)))
  (kbd "<leader>c") 'my/comment-line-dwim
  (kbd "<leader>C") 'comment-line
  "gq" 'evil-indent
  ;; xah inspiration:
  ;; ("TAB TAB" . indent-for-tab-command)
  ;; ("TAB i" . complete-symbol)
  ;; ("TAB g" . indent-rigidly)
  ;; ("TAB r" . indent-region)
  ;; ("TAB s" . indent-sexp)

  ;; ;; * Edits/Insertions + Formatting *
  (kbd "<leader>b\"") 'xah-escape-quotes
  (kbd "<leader>b\\") 'xah-space-to-newline
  (kbd "<leader>bl") 'xah-quote-lines
  (kbd "<leader>bw") 'xah-remove-whitespace
  ;; ...
  (kbd "<leader>bd") 'xah-insert-date
  ;; 'format dwim
  ;; ...
  (kbd "<leader>bf") 'xah-show-formfeed-as-line ;; note: should be on almost always
  )

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert Mode Maps
;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key 'insert 'global
  ;; make RET continue comments by default. use S-RET to bypass.
  ;; https://emacs.stackexchange.com/questions/59575/continue-comment-while-editing-lisp-and-when-hitting-enter
  (kbd "RET") 'comment-indent-new-line
  (kbd "<S-return>") 'newline
  (kbd "M-<delete>") 'kill-word
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key 'emacs 'global
  "\C-w" 'evil-window-map
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key '(normal visual motion) 'global
  (kbd "s-j") 'vterm
  
  ;; issues:
  ;; - how to toggle back from eshell? (currently using winner). way better if it's same key to toggle.
  ;; (kbd "s-j") 'eshell

  ;; issues:
  ;; - line highlight persists, making it hard to read
  ;; - does not always play nice with winner or changing windows
  ;; - overwrites default C-j "newline without completion"
  ;; (kbd "C-j") 'term-toggle-term
  )
;; note, this is kinda jank. don't love it.
(evil-define-key 'emacs 'global
  (kbd "s-j") 'evil-window-delete
  (kbd "C-j") 'evil-window-delete
  )


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes
;;;;;;;;;;;;;;;;;;;;;;;;;
;; xah window commands:
;; 'w' for xah-next-window-or-frame
;; '-' or '3' to close other windows (he uses this to close help) 
;; he uses leader-3 to close current window. it's lower-use than close-other-windows. fascinating!

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fix to distinguish <C-i> from Tab:
;; - keystroke C-i generates ^I (which is displayed at TAB in emacs)
;; - traditionally, Tab on terminals sends ^I too. Emacs binds it this way, too.
;; - many binds exist for Tab across emacs, so we should leave that alone.
;; - instead, we rebind C-i to send not ^I, but the custom code "<control-i>". then, we can
;; just define keybinds in terms of input (kbd "<control-i>")
;; - source: https://stackoverflow.com/questions/916797/emacs-global-set-key-to-c-tab
(define-key input-decode-map [(control ?i)] [control-i])
(define-key input-decode-map [(control ?I)] [(shift control-i)])

