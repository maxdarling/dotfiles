;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Todo
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - why are my normal mode commands not there in <N> mode in dired? E.g. 'h' is go left
;;   - I found it! bkmk dired-overrides.
;; - why even in <N> mode is my SPC doing dired-next-line?? I guess the major mode is stealing...?
;; - look at dired commands. (enable 'h', 'r', and leader, enable search with '/' and n/p)

;; - setup treesitter for elisp. highlighting??

;; - enable "h" switching from help mode (it's shadowed by '?' too, so it's fine)


(add-to-list 'load-path "~/.emacs.d/lisp")
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package package
  :config
  (require 'use-package-ensure)
  (setq use-package-always-ensure t)

  (add-to-list 'package-archives
               '("nongnuu" . "https://elpa.nongnu.org/nongnu/"))
  (package-initialize)
  (package-refresh-contents t))

;; evil
(use-package evil
  :config
  (evil-mode 1)
  (use-package evil-surround)
  (global-evil-surround-mode 1)

  ;; misc settings.
  ;; todo: enable C-w in emacs mode for window commands (instead of default kill-region) 
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'Info-mode 'emacs)
  (evil-set-initial-state 'inferior-scheme-mode 'emacs)
  )

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

;; fireplace
(use-package fireplace)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing
;;;;;;;;;;;;;;;;;;;;;;;;;
;; start cursor in help windows. without, kinda awk when a help window covers a buffer in the other window.
;; i have to move to it and press q.
(setq help-window-select t)

;; global enable abbrevs
(add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))
(add-hook 'prog-mode-hook (lambda () (abbrev-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Binds
;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/lisp/keymaps.el")
(load-file "~/.emacs.d/lisp/mystuff.el")

;; use XFK functions, discard all mappings
(setq xah-fly-use-control-key nil)
(setq xah-fly-use-meta-key nil)
(setq xah-fly-use-isearch-arrows nil)
(load-file "~/.emacs.d/lisp/xah-fly-keys.el")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes (+ Evil)
;;;;;;;;;;;;;;;;;;;;;;;;;
;; I should start thinking about disabling evil in certain major modes. E.g. info clearly seems better
;; without it. Dired, however, should work fine with evil.
;; I can look at the evil-collection for inspo here.

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Xah theme
;; todo: copy his syntax highlighting for elisp. see `custom-elisp-mode.el`
(if (not (display-graphic-p))
    (load-theme 'leuven-dark)
  (setq initial-frame-alist
	'(
	  (tool-bar-lines . 0)
	  (width . 210)
	  (height . 60)
	  (background-color . "honeydew")
	  (left . 50)
	  (top . 50))))

(set-frame-font "Menlo-14" t t)
(global-hl-line-mode 1)
(blink-cursor-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;
(setq make-backup-files nil) ; stop creating ~ files
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(setq vc-follow-symlinks t)
(setq use-short-answers t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
;; (global-prettify-symbols-mode 1) ;; e.g. lambda

;; fix evil undo and redo
(evil-set-undo-system 'undo-redo)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mastering emacs
;;;;;;;;;;;;;;;;;;;;;;;;;
;; replace list-buffers with ibuffer
(global-set-key [remap list-buffers] 'ibuffer)

;; fido mode
(fido-vertical-mode 1)
(setq completions-detailed t) ;; add docstrings to each item in completion prompt

;; todo: use sexp-based movement. e.g. C-M-f forward-sexp (but, this doesn't go to next lines, wtf?
;; note that C-M-b/n are related. b works nicely. but n won't let me skip to new lines, wtf.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(fireplace yasnippet evil-surround undo-tree evil-easymotion evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
