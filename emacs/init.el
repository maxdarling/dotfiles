;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Todo
;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; - / search is broken in dired, info, help modes (the initial search + highlight is made, but you can't n/N. fuck.)
;; - cleanup modeline (don't need minor modes)
;; - figure out how to auto-disable hl-line-mode in term (hook not working!!)
;; - figure out how to continue comments (but, corfu relies on normal RET?)
;; - disable corfu inside comments (no problem in elisp, but it is in go...?)
;; - figure out how to fill region while respecting comments.
;; - setup treesitter (elisp/scheme highlighting??) (evil-treesitter-text-obj!) (read karthinks post)
;; - develop Kotlin in Emacs (corfu, treesitter, LSP, magit) video: https://www.youtube.com/watch?v=J4s3T0dd5CY

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
;; - [project] cycle colorschemes + screen offsets for new windows.

(add-to-list 'load-path "~/.emacs.d/lisp")
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

;; evil
(use-package evil
  :init
  (setq evil-search-module 'evil-search)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)
  (use-package evil-surround)
  (global-evil-surround-mode 1)
  (use-package evil-visualstar)
  (global-evil-visualstar-mode t))

;; avy
(use-package avy
  :config
  (setq avy-keys '(?a ?h ?e ?t ?i ?s ?c ?n ?y ?x ?w ?v ?u ?r ?p ?o ?m ?l ?k ?j ?g ?f ?d ?b))
  (setq avy-all-windows nil)
  (setq avy-case-fold-search nil))

;; yasnippet
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; magit
(use-package magit)

;; fireplace
(use-package fireplace)

;; wgrep
(use-package wgrep)


;; corfu
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

;; embark
(use-package embark
  :ensure t
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

;; term-toggle
(use-package term-toggle
  ;; is this better? way older... https://github.com/kyagi/shell-pop-el
  :ensure nil
  :load-path "~/.emacs.d/lisp/emacs-term-toggle/term-toggle.el")

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
  ;; workaround GUI emacs path being different than term/system
  (add-to-list 'exec-path "/Users/mhd/go/bin/") ;; gopls
  )

(use-package typescript-ts-mode
  :mode ("\.ts$") ;; needed?
  )

(use-package kotlin-mode ;; (ts mode is shit)
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
	 (kotlin-mode . lsp)
	 (typescript-ts-mode . lsp))
  :commands (lsp lsp-deferred))
(use-package lsp-ui :commands lsp-ui-mode)

;; (lsp-register-custom-settings
;;  '(("gopls.completeUnimported" t t)
;;    ("gopls.staticcheck" t t)))

(use-package terraform-mode)

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
(load-file "~/.emacs.d/lisp/xah-fly-keys.el")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Xah theme
(if (not (display-graphic-p))
    (load-theme 'leuven-dark)
  (progn
    (setq initial-frame-alist
	  '(
	    (tool-bar-lines . 0)
	    (vertical-scroll-bars . nil)
	    (width . 200)
	    (height . 55)
	    (background-color . "honeydew")
	    (left . 30)
	    (top . 30)))
    (setq default-frame-alist
	  '(
	    (tool-bar-lines . 0)
	    (vertical-scroll-bars . nil)
	    (width . 200)
	    (height . 55)
	    (background-color . "antique white") ;; light yellow | seashell | antique white
	    (left . 10)
	    (top . 10)))))

(set-frame-font "Menlo-15" t t)
(global-hl-line-mode 1)
(blink-cursor-mode -1)

;; term background colors gave me lots of trouble. see lisp/hacks/term-color-hacks.el
(load-file "~/.emacs.d/lisp/hacks/term-color-hacks.el")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Settings
;;;;;;;;;;;;;;;;;;;;;;;;;
;; separate emacs and system clipboard
(setq x-select-enable-clipboard nil)
;; (evil-define-key nil 'global
;;   (kbd "s-v") 'clipboard-yank
;;   (kbd "s-c") 'clipboard-kill-ring-save
;;   (kbd "s-x") 'clipboard-kill-region)
(global-set-key (kbd "s-v") 'clipboard-yank) ;; above works, do these?
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-x") 'clipboard-kill-region)

(setq vc-follow-symlinks t)
(setq use-short-answers t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)
(setq dired-listing-switches "-alh")
(setq help-window-select t) ;; start cursor in help windows. easy exit with 'q'
;; (global-prettify-symbols-mode 1) ;; e.g. lambda
(setq garbage-collection-messages t) ;; starting point before gcmh
(setq shell-command-prompt-show-cwd t)
(setq gc-cons-threshold (* 100 1024 1024) ;; recommended for LSP
      read-process-output-max (* 1024 1024))


(winner-mode 1)
(add-hook 'scheme-mode-hook (lambda () (setq indent-tabs-mode -1))) ;; not working!!

(setq make-backup-files nil) ; stop creating ~ files
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(global-set-key [remap list-buffers] 'ibuffer) ;; replace list-buffers with ibuffer

;; fido mode
(fido-vertical-mode 1)
(setq completions-detailed t) ;; add docstrings to each item in completion prompt

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(go-mode lsp-ui lsp-mode rainbow-mode color-theme-buffer-local term-toggle evil-visualstar wgrep cape terraform-mode embark kotlin-mode kotlin-ts-mode treesit corfu magit fireplace yasnippet evil-surround undo-tree evil-easymotion evil)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(term-color-black ((t (:foreground "#3F3F3F" :background "#2B2B2B"))))
 '(term-color-blue ((t (:foreground "#7CB8BB" :background "#4C7073"))))
 '(term-color-cyan ((t (:foreground "#93E0E3" :background "#8CD0D3"))))
 '(term-color-green ((t (:foreground "#7F9F7F" :background "#9FC59F"))))
 '(term-color-magenta ((t (:foreground "#DC8CC3" :background "#CC9393"))))
 '(term-color-red ((t (:foreground "#AC7373" :background "#8C5353"))))
 '(term-color-white ((t (:foreground "#DCDCCC" :background "#656555"))))
 '(term-color-yellow ((t (:foreground "#DFAF8F" :background "#9FC59F"))))
 '(term-default-bg-color ((t (:inherit term-color-black))))
 '(term-default-fg-color ((t (:inherit term-color-white)))))
