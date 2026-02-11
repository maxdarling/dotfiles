;;;;;;;;;;;;;;;;;;;;;;;;;;;  -*- lexical-binding: t; -*-
;; Modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hide minor modes (https://emacs.stackexchange.com/a/3928)
(defvar my//hidden-minor-modes
  '(abbrev-mode
    yas-minor-mode
    auto-revert-mode
    eldoc-mode
    ;; auto-fill-function
    ;; flycheck-mode
    ;; flyspell-mode
    ;; smooth-scroll-mode
    ))
(defun purge-minor-modes ()
  (interactive)
  (dolist (x my//hidden-minor-modes nil)
    (let ((trg (cdr (assoc x minor-mode-alist))))
      (when trg
        (setcar trg "")))))
(add-hook 'after-change-major-mode-hook 'purge-minor-modes)
