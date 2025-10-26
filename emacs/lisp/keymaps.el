(evil-set-leader '(normal visual motion) (kbd "SPC"))
(evil-set-leader 'emacs (kbd "C-SPC"))
(setopt evil-want-Y-yank-to-eol t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (evil-define-key '(normal visual motion emacs) 'global
(evil-define-key nil 'global
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
  (kbd "<leader>q") 'evil-record-macro ;; ballsy?! but quit window >often than creating macro.
  ;; todo: map C-i and C-o in all modes? (alternative is s-<left> and s-<right>, as I do in IJ)
  (kbd "C-u") 'evil-scroll-up
  (kbd "C-d") 'evil-scroll-down ;; (already bound in <N>)
  (kbd "<leader>v") 'universal-argument
  ;; "h" 'evil-window-next ;; holding off binding for all modes. using for magit currently.
  ;; todo: decide on below
  ;; (kbd "s-e") 'evil-window-next ;; e.g. useful in scheme repl. but is there a better chord?
  ;; (kbd "s-w") 'delete-window
  ;; (kbd "s-o") 'delete-other-windows
  ;; (kbd "s-W") 'delete-frame ;; alternative to above

  ;; * Windows/Frames *
  (kbd "M-<left>") 'winner-undo
  (kbd "M-<right>") 'winner-redo
  (kbd "<leader>-") 'delete-window
  ;; ...
  (kbd "<leader>ws") 'evil-window-split
  (kbd "<leader>wv") 'evil-window-vsplit
  (kbd "<leader>wc") 'evil-window-delete

  ;; * Buffers/Files *
  (kbd "<leader>e") 'switch-to-buffer
  (kbd "<leader>E") 'switch-to-buffer-other-window ;; ideally, i could press C-o or S-RET in the minibuffer. helm?
  (kbd "<leader>a") 'find-file ;; todo: need directory-recursive version (via helm, likely?)
  (kbd "<leader>o") 'bookmark-jump
  (kbd "<leader>d") 'dired-jump
  (kbd "<leader>D") 'dired-jump-other-window
  (kbd "<leader>j") 'eshell ;; I'm unpracticed with eshell
  (kbd "<leader>x") 'my/run-current-file
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
  (kbd "<leader>ix") 'my/run-current-file
  (kbd "<leader>ik") 'my/run-current-file
  (kbd "<leader>i1") 'xah-open-in-terminal
  ;; xah-show-in-desktop
  ;; ;; ...
  (kbd "<leader>ic") 'xah-copy-file-path

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
  (kbd "<control-i>") 'evil-jump-forward ;; fix <C-i>. see Tab vs. <C-i> discussion below.
  "q" 'quit-window
  "s" 'avy-goto-char-timer
  "S" 'avy-goto-line ;'avy-goto-char
  "r" 'evil-buffer
  "l" 'execute-extended-command ;; testing.
  "L" 'evil-ex
  ;; (kbd "C-u") 'evil-scroll-up
  ;; (kbd "<leader>v") 'universal-argument
  ":" 'repeat-complex-command ;; testing. idea: mirrors the '.' command
  (kbd "C-a") 'my/inc-number-after-point
  (kbd "C-S-a") (lambda() (interactive) (my/inc-number-after-point (- 1)))
  (kbd "C-@") (lambda() (interactive) (set-buffer-modified-p nil) (kill-buffer)) ;; forcibly delete buffer

  ;; testing - numbers and C-numbers are same by default. that's free real-estate since I don't use numbers much!
  "7" 'magit
  "3" (lambda() (interactive) (kill-buffer nil))
  "9" 'my/align

  ;; ;; * Windows/Frames *
  "h" 'evil-window-next
  "-" 'delete-other-windows ;; this is most important. help/info windows are simply closed with 'q'

  ;; * Commenting + Indentation *
  (kbd "<leader>0") (lambda () (interactive) (indent-region (point-min) (point-max)))
  (kbd "<leader>c") 'comment-line
  (kbd "<leader>c") 'my/comment-dwim
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
  ;; (kbd "RET") 'comment-indent-new-line
  (kbd "<S-return>") 'newline
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key 'emacs 'global
  "\C-w" 'evil-window-map
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Term popups
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key '(normal motion) 'global "\C-j" 'term-toggle-term)
(evil-define-key 'emacs 'global "\C-j" 'evil-window-delete)

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
;; last, I need to rebind <C-i> to the evil defaults

