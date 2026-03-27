;; -*- lexical-binding: t; -*-
(evil-set-leader '(normal visual motion) (kbd "SPC"))
(evil-set-leader '(emacs) (kbd "S-SPC"))

;; C-o prefix
(define-prefix-command 'my/c-o-map)
(define-key my/c-o-map (kbd "C-c") #'my/find-dir-rec)
(define-key my/c-o-map (kbd "C-u") #'my/find-file-rec)
(define-key my/c-o-map (kbd "C-g") #'grep)

;; leader-a prefix
(define-prefix-command 'my/leader-a-map)
(define-key my/leader-a-map (kbd "j") #'my/journal)
(define-key my/leader-a-map (kbd "o") #'my/open-org-dir)
(define-key my/leader-a-map (kbd "m") #'my/open-mandarin-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All states
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key* '(normal visual motion emacs insert) 'global
  (kbd "s-p") 'project-find-file
  (kbd "s-P") 'execute-extended-command

  (kbd "<control-i>") 'evil-window-next
  (kbd "C-t") 'evil-buffer ;; testing
  (kbd "s-e") 'evil-buffer ;; testing

  (kbd "C-o") my/c-o-map

  (kbd "s-s") 'save-buffer
  (kbd "s-d") 'kill-buffer ;; this kinda sucks. default should be not have to press RET

  ;; testing: harpoon (see notes.org for discussion)
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
  (kbd "C-\'") 'universal-argument

  ;; "\C-w" 'evil-window-map

  ;; * Core Nav *
  (kbd "s-,") (lambda () (interactive) (find-file my/emacs-init-loc))
  (kbd "s-N") (lambda ()
                (interactive)
                (select-frame-set-input-focus (make-frame))
                (find-file my/code-dir))
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
  (kbd "<leader>E") 'consult-buffer
  ;; (kbd "<leader>E") 'switch-to-buffer-other-window ;; ideally, i could press C-o or S-RET in the minibuffer. embark?
  (kbd "<leader>a") my/leader-a-map
  (kbd "<leader>o") 'bookmark-jump
  (kbd "<leader>d") 'dired-jump
  (kbd "<leader>D") 'dired-jump-other-window
  (kbd "<leader>x") 'my/run-file

  (kbd "<leader>ia") 'find-file

  ;; org (testing)
  (kbd "<leader>h") 'org-capture
  (kbd "<leader>j") 'my/journal
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

  ;; * Common *
  (kbd "<leader>ti") 'query-replace-regexp ;; todo: incremental replace preview like :%s
  (kbd "<leader>tt") 'repeat-complex-command
  (kbd "<leader>tl") 'list-matching-lines
  (kbd "<leader>tv") 'delete-matching-lines
  ;; ...
  (kbd "<leader>tu") 'abbrev-mode
  ;; todo: abbrev selector

  ;; * Evaluations *
  (kbd "<leader>kh") 'my/run-file
  (kbd "<leader>kf") 'eval-buffer
  (kbd "<leader>kd") 'eval-defun
  (kbd "<leader>kl") 'eval-last-sexp
  (kbd "<leader>ks") 'eval-expression
  (kbd "<leader>kr") 'eval-region
  )

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Normal mode
;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key '(normal visual motion) 'global
  ;; * Core *
  "h" 'my/comment-line-dwim
  "H" 'my/comment-and-duplicate
  "q" 'quit-window
  "-" 'delete-other-windows
  "s" 'avy-goto-char-timer
  "S" 'avy-goto-line
  "l" 'execute-extended-command
  "L" 'evil-ex
  ":" 'repeat-complex-command ;; testing. idea: vim '.' analog for complex cmds

  (kbd "C-p") 'my/inc-number-after-point
  (kbd "C-S-p") 'my/dec-number-after-point
  (kbd "C-@") (lambda() (interactive) (set-buffer-modified-p nil) (kill-buffer)) ;; forcibly delete buffer

  ;; * Commenting + Indentation *
  (kbd "<leader>0") (lambda () (interactive) (indent-region (point-min) (point-max)))
  (kbd "<leader>c") 'my/comment-line-dwim
  "gq" 'evil-indent
  )

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert Mode Maps
;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key 'insert 'global
  ;; GNU Readline -style ovverides
  (kbd "M-<delete>") 'kill-word
  (kbd "C-e") 'move-end-of-line
  (kbd "C-a") 'move-beginning-of-line
  (kbd "C-k") 'kill-line
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C-i hack
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
