(evil-set-leader nil (kbd "SPC"))
(setopt evil-want-Y-yank-to-eol t)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Normal mode maps
;;;;;;;;;;;;;;;;;;;;;;;;;
;; todo: this should be more forceful. e.g. right now dired's use of evil-make-overriding-map is
;; overpowering this mapping. evil-make-intercept-map seems good...?
;; (define-minor-mode foo-mode
;;   "Foo mode."
;;   :keymap (make-sparse-keymap))

;; minor mode way works but is clunky. I'd rather have the evil minor mode active and that's it.
;; I think I can just define a custom keymap, e.g. "foo-map" as in the docs.
;; (evil-define-keymap mymap
;;   "my keymap, baby"
;;   "h" 'evil-window-next
;;   "l" 'fireplace)

;; (evil-make-intercept-map mymap )


;; other idea: use advice on 'evil-make-overriding-map'. when called with dired-mode-map, just
;; stop. pretty lit...?
;; (defun md-trace-evil-override-map (orig-fun &rest args)
;;   (message "evil-make-overriding-map called with args %S" args)
;;   (let ((res (apply orig-fun args)))
;;     (message "evil-make-overriding-map returned %S" res)
;;     res))

;; (advice-add 'evil-make-overriding-map :around #'md-trace-evil-override-map)

;; ok, WTF. 'h' and 'l' are taking precedence in dired now. but they weren't before. makes ZERO sense.
;; but 'r' doesn't take precedence, for example. and now I've commented all my changes and yet this
;; behavior remains. huh??? I'm tempted to reinstall my emacs now. I need to know how I did that.
;; and then how to undo it as well, and how to do it for 'r' and others. wtf is going on.


;; (evil-define-key '(normal visual motion) 'global
(evil-define-key '(normal visual motion) 'global
;; (evil-define-key '(normal visual) 'foo-mode 
;; (defvar md-keymap (define-keymap
  ;; * Core *
  "s" 'avy-goto-char-timer
  "S" 'avy-goto-line ;'avy-goto-char
  "r" 'evil-buffer
  "l" 'execute-extended-command ;; testing.
  "L" 'evil-ex
  (kbd "C-u") 'evil-scroll-up
  (kbd "<leader>v") 'universal-argument
  ":" 'repeat-complex-command ;; testing. idea: mirrors the '.' command
  (kbd "C-a") 'md-inc-number-after-point
  (kbd "C-S-a") (lambda() (interactive) (md-inc-number-after-point (- 1)))
  (kbd "C-3") (lambda() (interactive) (kill-buffer nil)) ;; testing. 3 is a little random, but a decent key.
  (kbd "C-@") (lambda() (interactive) (set-buffer-modified-p nil) (kill-buffer)) ;; forcibly delete buffer

  ;; * Windows/Frames *
  "h" 'evil-window-next
  "-" 'delete-other-windows ;; this is most important. help/info windows are simply closed with 'q'
  (kbd "<leader>-") 'delete-window
  ;; ...
  (kbd "<leader>ws") 'evil-window-split
  (kbd "<leader>wv") 'evil-window-vsplit
  (kbd "<leader>wc") 'evil-window-delete
  ;; (kbd "<leader>wu") 'winner-undo ;; todo: look into winner mode
  ;; (kbd "<leader>wU") 'winner-redo

  ;; * Buffers/Files *
  (kbd "<leader>e") 'switch-to-buffer
  (kbd "<leader>a") 'find-file ;; todo: need directory-recursive version (via helm, likely?)
  (kbd "<leader>o") 'bookmark-jump
  (kbd "<leader>d") 'dired-jump
  (kbd "<leader>D") 'dired-jump-other-window
  (kbd "<leader>j") 'eshell ;; I'm unpracticed with eshell
  ;;
  (kbd "<leader>ie") 'switch-to-buffer
  (kbd "<leader>ia") 'find-file
  ;; ...
  (kbd "<leader>io") 'bookmark-jump
  (kbd "<leader>ir") 'bookmark-set
  (kbd "<leader>im") 'bookmark-bmenu-list
  ;; ...
  (kbd "<leader>is") 'save-buffer
  (kbd "<leader>iS") (lambda () (interactive) (save-some-buffers t) (message "Saved all buffers."))
  (kbd "<leader>id") 'kill-buffer ;; this kinda sucks. default should be not have to press RET
  (kbd "<leader>iD") (lambda () (interactive)
		       (save-buffer) (kill-buffer) (message "Saved and killed buffer."))
  ;; ...
  (kbd "<leader>in") 'xah-new-empty-buffer  
  ;; ;; ...
  ;; 'xah-open-last-closed
  ;; 'xah-list-recently-closed
  (kbd "<leader>iw") 'recentf
  ;; ;; ...
  (kbd "<leader>ix") 'md-run-current-file
  (kbd "<leader>ik") 'md-run-current-file
  (kbd "<leader>i1") 'xah-open-in-terminal
  ;; xah-show-in-desktop
  ;; ;; ...
  (kbd "<leader>ic") 'xah-copy-file-path

  ;; * Indentation (todo) *
  ;; ("TAB TAB" . indent-for-tab-command)
  ;; ("TAB i" . complete-symbol)
  ;; ("TAB g" . indent-rigidly)
  ;; ("TAB r" . indent-region)
  ;; ("TAB s" . indent-sexp)
  (kbd "<leader>0") (lambda () (interactive) (indent-region (point-min) (point-max)))

  ;; * Misc *
  (kbd "<leader>c") 'comment-line
  "gq" 'evil-indent

  ;; * Common *
  (kbd "<leader>tg") 'rgrep
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
  (kbd "<leader>kh") 'md-run-current-file
  (kbd "<leader>kf") 'eval-buffer
  (kbd "<leader>kd") 'eval-defun
  (kbd "<leader>kl") 'eval-last-sexp
  (kbd "<leader>ks") 'eval-expression
  (kbd "<leader>kr") 'eval-region
  ;; (kbd "<leader>kz") 'xah-run-current-file

  )

;; ;; * Global Windows / Frames *
(evil-define-key nil 'global
  ;; todo: potentially remove all these.
  (kbd "s-e") 'evil-window-next ;; e.g. useful in scheme repl. but is there a better chord?
  ;;   (kbd "s-w") 'delete-window
  ;;   (kbd "s-o") 'delete-other-windows
  ;;   (kbd "s-W") 'delete-frame ;; alternative to above
  )

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert Mode Maps
;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key 'insert 'global
  ;; make RET continue comments by default. use S-RET to bypass.
  ;; https://emacs.stackexchange.com/questions/59575/continue-comment-while-editing-lisp-and-when-hitting-enter
  (kbd "RET") 'comment-indent-new-line
  (kbd "<S-return>") 'newline
  )

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Project Toggle
;;;;;;;;;;;;;;;;;;;;;;;;;
;; problem: too annoying to add a "p" prefix for project commands. e.g. C-x p f or C-x p b
;; ideas:
;; - make a toggle system. i get to keep the same SPC-i-e/a commands, but it will be project or non-project based on
;; a variable. and I have one keycombo to set the variable. totally nice. but added complexity.
;; - default to project for now...ballsy, but not bad. since my workflow is to generally stay in-project.

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Todo
;;;;;;;;;;;;;;;;;;;;;;;;;

;; use hippie expand
;; use YA snippet

;; lookup winner mode

;; settle on regexp replace. I want live preview like :%s. do I need a package for that? eww.
;; also, I should learn how to use elisp funcs in the replacement! https://www.reddit.com/r/emacs/comments/mil5to/use_visualreplaceregexp_lisp_functions_to_do/

;; yank-from-kill-ring

;; use emacs transpose functionality. seems powerful. e.g. word, sexp, line with M-t, C-M-t, C-x C-t
;; seems evil has unbound these, though

;; use emacs filling and formatting

;; todo: copy xah abbrev completion
;; http://xahlee.info/emacs/emacs/emacs_interactive_abbrev.html

;; bonus: TAB -> xah-elisp-complete-or-indent (https://www.youtube.com/live/bHGVp9c7fPg?si=2sPVPRMfTvpqHByF&t=1030)
;; I don't see this in XFK, it must be part of his elisp mode. but I do see xah-reformat-lines.
;; todo: xah-comment-dwim (set to ;). kinda lit.
;; todo: I can use number keys. I rarely use em in vim. I'm fine to prefix it to use em as numbers if
;; needed. 

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes
;;;;;;;;;;;;;;;;;;;;;;;;;
;; xah window commands:
;; 'w' for xah-next-window-or-frame
;; '-' or '3' to close other windows (he uses this to close help) 
;; he uses leader-3 to close current window. it's lower-use than close-other-windows. fascinating!
