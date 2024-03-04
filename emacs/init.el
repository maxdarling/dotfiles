(add-to-list 'load-path "~/.emacs.d/lisp")
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)

;; todo: can I skip the below??? it was recommended in the evil readme.
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'evil)
  (package-install 'evil)
  (package-install 'evil-surround))
(require 'evil)
(evil-mode 1)
(global-evil-surround-mode 1)

;; avy
(unless (package-installed-p 'avy)
  (package-install 'avy))
(setq avy-keys '(?a ?h ?e ?t ?i ?s ?c ?n ?y ?x ?w ?v ?u ?r ?p ?o ?m ?l ?k ?j ?g ?f ?d ?b))
(setq avy-all-windows nil)
(setq avy-case-fold-search nil)

;; is pretty bad (or, good if you want typing game practice, lol). so, instead I should
;; setup a screen-local search that doesn't pollute jump list.

;; yasnippet
(unless (package-installed-p 'yasnippet) (package-install 'yasnippet))
(require 'yasnippet)
(yas-global-mode 1)
(require 'yasnippet)
(yas-global-mode 1)

;; fireplace
(unless (package-installed-p 'fireplace) (package-install 'fireplace))
(require 'fireplace)

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
	  (width . 106)
	  (height . 60)
	  (background-color . "honeydew")
	  (left . 50)
	  (top . 50))))

(global-hl-line-mode 1)
(set-frame-font "Menlo-14" t t)

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
