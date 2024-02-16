;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'evil)
  (package-install 'evil))

(require 'evil)
(evil-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Binds
;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-set-leader nil (kbd "SPC"))

(evil-define-key 'normal 'global (kbd "<leader>ev") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(evil-define-key 'normal 'global (kbd "<leader>c") 'comment-line)

(define-key evil-normal-state-map "gq" 'evil-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq make-backup-files nil) ; stop creating ~ files

(load-theme 'leuven-dark)

;; fix evil undo and redo
(global-undo-tree-mode)
(evil-set-undo-system 'undo-tree)
