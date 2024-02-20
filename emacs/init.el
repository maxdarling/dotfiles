;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; evil
(require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))
;; (package-initialize)
;; (package-refresh-contents)

(unless (package-installed-p 'evil)
  (package-install 'evil))

(require 'evil)
(evil-mode 1)

;; ;; easymotion
;; (unless (package-installed-p 'evil-easymotion)
;;   (package-install 'evil-easymotion))

;; avy
(unless (package-installed-p 'avy)
  (package-install 'avy))
(setq avy-keys '(?a ?h ?e ?t ?i ?s ?c ?n ?y ?x ?w ?v ?u ?r ?p ?o ?m ?l ?k ?j ?g ?f ?d ?b))
;; Xah valid critique of Avy: http://xahlee.info/emacs/misc/ace_jump_avy_vs_isearch.html 
;; the only downside of search is that it alters jumplist. I agree that the brain cycle
;; is pretty bad (or, good if you want typing game practice, lol). so, instead I should
;; setup a screen-local search that doesn't pollute jump list.

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Binds
;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-set-leader nil (kbd "SPC"))

(evil-define-key 'normal 'global (kbd "<leader>ev") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(evil-define-key 'normal 'global (kbd "<leader>c") 'comment-line)

(define-key evil-normal-state-map "gq" 'evil-indent)

(define-key evil-normal-state-map "s" 'avy-goto-char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq make-backup-files nil) ; stop creating ~ files
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(setq vc-follow-symlinks t)
(setq use-short-answers t) 
(setq visible-bell 1)
(set-frame-font "Menlo-14" t t)
(setq font-lock-maximum-decoration t)

;; Xah theme
;; todo: copy his syntax highlighting for elisp. see `custom-elisp-mode.el`
(if (display-graphic-p)
    (setq initial-frame-alist
          '(
            (tool-bar-lines . 0)
            (width . 106)
            (height . 60)
            (background-color . "honeydew")
            (left . 50)
            (top . 50)))
  (load-theme 'leuven-dark))

(global-hl-line-mode 1)

;; fix evil undo and redo
(global-undo-tree-mode)
(evil-set-undo-system 'undo-tree)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(evil-easymotion undo-tree spacemacs-theme evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
