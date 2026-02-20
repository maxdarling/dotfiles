;;;;;;;;;;;;;;;;;;;;;;;;;;;  -*- lexical-binding: t; -*-
;; Todo
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; done:
;; - continue comment by default (elisp), S-RET to not.
;; - try vterm a bit, get M-<right/left> working
;; - setup fzf file and dir.
;; - debug/fix custom-file
;; - setup vscode M-<up>/<down>
;; - fix lexical binding warnings (and understand them...)
;; - make s-, switch to init file
;; - cleanup my-theme.el
;;   - factor out frame sizing.
;;   - refactor into options for background color toggles
;; - keymap revamp phase 1 (and cleanups)
;; - early-init.el (for window stuff)
;; - try harpoon plugin in emacs
;; - bind find-file, find-dir, grep (NOT LEADER)
;; - rg for grep and rgrep
;; - auto-fill-mode for comments only (and learned M-q manual approach too)
;; - project: "H" for custom duplicate and comment func.

;; projects:
;; - keyboard diagram visualizer
;; - harpoon: show list in minibuffer (for better ergo looking down than up)

;; todo:
;; - get fill (M-q) to work for not just comments (didn't work for defun docstring)
;; - research autosave (and why .#init.el is being generated)
;; - setup format-on-save (a good pattern IMO)
;; - consult omni search bound to s-p (vscode style)
;; - corfu show "src" next to popup (e.g. like in the screenshots here:
;; https://github.com/minad/corfu)
;; - revamp keymaps (ongoing)
;; 
;; - study modus themes config
;; - cleanup init file
;;   - research user-lisp-directory
;;   - general cleanups
;;   - outline mode?
;; - literate emacs config? (via org)
;;   - this would give me functionality for bullets in comments: autocontinue and tab to nest
;;     - this is more like gdocs and markdown
;; - embark: shift-RET should do otherwindow by default (or "O"?) (e.g. for bookmark, switch to buffer, etc.)
;; - prot consult/vertico/marginalia/embark/orderless video
;;   - vertico - how is it different than fido? i wanna understand, e.g. see "learning" note.
;;   - marg seems good. too busy for files tho.
;;   - consult seems beast? impressed at his "buffer vs. bookmark is just an impl. detail"
;;   - orderless: ...
;;   - embark: ...

;; terminal workflow notes:
;; - except C-c? Big choice, but likely worth. C-z is my escape hatch.
;; - how to get multi-tab?
;; - explore vterm-toggle, vterm hotkey, vtm

;; LEARNING:
;; - what is xref?
;; - flymake
;; - compile commands
;; - elisp fundamentals
;; - recentf mode
;; - ido vs icomplete vs fido (look at source?) (xah?)
;; - imenu
;; - study avy (https://karthinks.com/software/avy-can-do-anything/)

;; IMPORTANT: 
;; - visual marks (e.g. IntelliJ style. highlight the line. or, use left "gutter", e.g. like bookmarks)
;; - decide on magit key (leader something)
;; - disable corfu inside comments (no problem in elisp, but it is in go...?)
;; - setup treesitter (elisp/scheme highlighting??) (evil-treesitter-text-obj!) (read karthinks post)

;; MEDIUM IMPORT:
;; - use C-x p f (and rest of "project" commands)
;; - try consult (e.g. consult-dir in the Embark karthinks article) 
;; - setup section folding (like my vimrc) for this file. does it exist in emacs? make my own?? outline mode?

;; BACKBURNER:
;; - I can bind number keys. I rarely use em in vim. I'm fine to prefix it to use em as numbers if needed. 
;; - popper. very powerful. terms and more. per-project. dedicated help toggle could be nice.
;; - harpoon
;; - paredit (and aggressive-indent-mode)
;; - setup advanced abbrev completion system, like Xah (http://xahlee.info/emacs/emacs/emacs_interactive_abbrev.html)
;; - try ace-window (and combine with embark)
;; - yank-from-kill-ring
;; - use emacs transpose functionality. seems powerful. e.g. word, sexp, line with M-t, C-M-t, C-x C-t
;; seems evil has unbound these, though
;; - TAB -> xah-elisp-complete-or-indent (https://www.youtube.com/live/bHGVp9c7fPg?si=2sPVPRMfTvpqHByF&t=1030)
;; I don't see this in XFK, it must be part of his elisp mode. but I do see xah-reformat-lines.
;; - xah-comment-dwim (set to ;). kinda lit.
;; - use sexp-based movement. e.g. C-M-f forward-sexp (but, this doesn't go to next lines, wtf?
;; note that C-M-b/n are related. b works nicely. but n won't let me skip to new lines, wtf.
;; - copy his syntax highlighting for elisp. see `custom-elisp-mode.el`

(add-to-list 'load-path "~/.emacs.d/lisp")

;; put package-manager-generated stuff in another file
(setq custom-file "~/.emacs.d/customization.el")
(load-file custom-file)

(setq my/init-file "~/.emacs.d/init.el")
(setq initial-buffer-choice my/init-file)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;
;; essentials
(set-frame-font "Menlo-15" t t) ;; todo: try prot
(global-hl-line-mode 1)
(blink-cursor-mode -1)
(setq-default display-line-numbers t)

;; frame: see 'early-init.el'

;; modeline
(load-file "~/.emacs.d/lisp/themes/modeline.el")

;; prot theme
(use-package modus-themes
  :config
  ;; todo
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil) ;; testing
  (modus-themes-load-theme 'modus-operandi)
  )

;; (load-file "~/.emacs.d/lisp/hacks/term-color-hacks.el")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package package
  :config
  (require 'use-package-ensure)
  (setq use-package-always-ensure t)

  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
  (package-refresh-contents t))

;; todo:
;; - hard to see shit. should use a diff theme
;; - read this: https://karthinks.com/software/fringe-matters-finding-the-right-difference/?utm_source=chatgpt.com
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; (setq diff-hl-fringe-bmp-function #'diff-hl-fringe-bmp-from-type)
  (setq diff-hl-update-async t)
  (setq diff-hl-draw-borders nil)
  ;; to test:
  ;; - async update on?
  ;; - bmp max width
  ;; - draw borders
  ;; - face ("change", "delete")
  )


(use-package evil
  :init
  (setq evil-search-module 'isearch) ;; bug 8/2/24: 'evil-search breaks search in help/dired/info modes
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)
  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)

  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)
  (use-package evil-surround)
  (global-evil-surround-mode 1)
  (use-package evil-visualstar)
  (global-evil-visualstar-mode t)
  (load-file "~/.emacs.d/lisp/move-text.el"))

(use-package avy
  :config
  (setq avy-keys '(?a ?h ?e ?t ?i ?s ?c ?n ?y ?x ?w ?v ?u ?r ?p ?o ?m ?l ?k ?j ?g ?f ?d ?b))
  (setq avy-all-windows nil)
  (setq avy-case-fold-search nil)
  (setq avy-timeout-seconds 0.3))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package magit)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

(use-package fireplace)

(use-package wgrep)

(use-package corfu
  :init
  (global-corfu-mode)
  (use-package cape)
  :config
  (setq corfu-auto t
	    corfu-auto-delay .1) ;; .2 default
  (add-to-list 'completion-styles 'flex) 

  ;; order matters (first in list = highest priority)
  (advice-add #'cape-dabbrev :around #'cape-wrap-inside-code)
  ;; (defalias 'defensive-cape-line (cape-capf-inside-code #'cape-line))
  ;; (add-to-list 'completion-at-point-functions #'defensive-cape-line) ;; sometimes overpowers elisp?
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-history) ;; haven't tried.
  (add-to-list 'completion-at-point-functions #'cape-file) ;; this is sick...
  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ;; note: dabbrev capf doesn't work.

  ;; todo? scheme keyword complete with prefix length 2 (currently 3)
  ;; to get more completion in scheme mode.
  )

(use-package embark
  :bind (:map minibuffer-mode-map
	          (("C-e" . embark-act)
	           ("C-a" . embark-dwim)))
  :config
  ;; my hack to use embark-dwim to do "other-window" stuff in 1 keypress.
  ;; see 'lisp/hacks/embark-hacks.el' for more ideas
  (setq embark-default-action-overrides
	    '(((buffer . switch-to-buffer) . switch-to-buffer-other-window) ;; works. must be 'buffer'
	      ((command . execute-extended-command) . describe-symbol) ;; this works!! wow!
	      ;; opens dired, and not even on right file.
	      ;; note: same behavior as builtin find-file 'o' option.
	      ((file . find-file) . find-file-other-window) 
	      )
	    )
  )

(use-package term-toggle
  ;; is this better? way older... https://github.com/kyagi/shell-pop-el
  :ensure nil
  :load-path "~/.emacs.d/lisp/emacs-term-toggle")

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; treesitter
(require 'treesit)
;; (language definitions built via https://github.com/casouri/tree-sitter-module.git)
;; resources:
;; - https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter
;; - https://www.masteringemacs.org/article/how-to-get-started-tree-sitter

(use-package go-mode
  ;; :mode ("\.go$") ;; needed?
  :init
  ;; work around GUI emacs path being different than term/system
  (add-to-list 'exec-path "/Users/mhd/go/bin/") ;; gopls
  )

(use-package typescript-ts-mode
  :mode ("\.ts$") ;; needed?
  )

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l") ;; needs to happen initially?
  :config
  ;; (setq lsp-keymap-prefix "C-c l")
  (setq lsp-ui-sideline-show-code-actions t)
  ;; (setq lsp-ui-sideline-show-diagnostics t) ;; useless?
  ;; (setq lsp-ui-sideline-show-hover t) ;; just types, very noisy

  :hook (
	     (go-mode . lsp) ;; maybe lsp-deferred?
	     (typescript-ts-mode . lsp))
  :commands (lsp lsp-deferred))
(use-package lsp-ui :commands lsp-ui-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing
;;;;;;;;;;;;;;;;;;;;;;;;;
;; global enable abbrevs
(add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))
(add-hook 'prog-mode-hook (lambda () (abbrev-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Binds
;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/lisp/keymaps.el")
(load-file "~/.emacs.d/lisp/mode-keymaps.el")
(load-file "~/.emacs.d/lisp/mystuff.el")

;; use XFK functions, discard all mappings
(setq xah-fly-use-control-key nil)
(setq xah-fly-use-meta-key nil)
(setq xah-fly-use-isearch-arrows nil)
;; (load-file "~/.emacs.d/lisp/pedagogy/xah-fly-keys.el")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Settings
;;;;;;;;;;;;;;;;;;;;;;;;;
;; so we can edit files from terminal with the "emacsclient" command.
(server-start)

;; differentiate emacs kill ring and system clipboard
(setq x-select-enable-clipboard nil)
(global-set-key (kbd "s-v") 'clipboard-yank)
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-x") 'clipboard-kill-region)

;; misc low-level
(setq vc-follow-symlinks t)
(setq use-short-answers t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)
(setq dired-listing-switches "-alh --group-directories-first")
(setq insert-directory-program "gls") ;; needed to get above option
(setq help-window-select t) ;; start cursor in help windows. easy exit with 'q'
;; (global-prettify-symbols-mode 1) ;; e.g. lambda
(setq garbage-collection-messages t) ;; starting point before gcmh
(setq shell-command-prompt-show-cwd t)
(setq gc-cons-threshold (* 100 1024 1024) ;; recommended for LSP
      read-process-output-max (* 1024 1024))

;; misc
(winner-mode 1)
(which-key-mode 1)

(setq indent-tabs-mode nil)
(setq tab-width 4)

;; autofill comments (or, just M-q it manually)
(column-number-mode 1)
(setq-default fill-column 90)
;; (auto-fill-mode 1)
;; (setq comment-auto-fill-only-comments t)

(setq auto-save-default nil) ;; stop creating autosave files e.g. #file#
(setq make-backup-files nil) ; stop creating ~ files
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(global-set-key [remap list-buffers] 'ibuffer) ;; replace list-buffers with ibuffer

;; rg for grep
(setq grep-command "rg -nS --no-heading "
      grep-use-null-device nil)

;; fido mode
(fido-vertical-mode 1)
(setq completions-detailed t) ;; add docstrings to each item in completion prompt
(with-eval-after-load 'icomplete
  ;; Make C-j exit literally instead of forcing completion
  (define-key icomplete-minibuffer-map (kbd "C-j") #'exit-minibuffer))
