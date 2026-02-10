;; source: https://karthinks.com/software/fifteen-ways-to-use-embark/  -*- lexical-binding: t; -*-
;; above is great. makes heavy use of ace-window, too.

(eval-when-compile
  (defmacro my/embark-split-action (fn split-type)
    `(defun ,(intern (concat "my/embark-"
			     (symbol-name fn)
			     "-"
			     (car (last  (split-string
					  (symbol-name split-type) "-"))))) ()
       (interactive)
       (funcall #',split-type)
       (call-interactively #',fn))))

(define-key embark-file-map     (kbd "2") (my/embark-split-action find-file split-window-below))
(define-key embark-buffer-map   (kbd "2") (my/embark-split-action switch-to-buffer split-window-below))
(define-key embark-bookmark-map (kbd "2") (my/embark-split-action bookmark-jump split-window-below))

(define-key embark-file-map     (kbd "3") (my/embark-split-action find-file split-window-right))
(define-key embark-buffer-map   (kbd "3") (my/embark-split-action switch-to-buffer split-window-right))
(define-key embark-bookmark-map (kbd "3") (my/embark-split-action bookmark-jump split-window-right))


;; from https://github.com/oantolin/embark/wiki/Additional-Actions#run-the-default-action-in-another-window
;; (this approach is general, but I like the above better)
(defun embark-default-action-in-other-window ()
  "Run the default embark action in another window."
  (interactive))

(cl-defun run-default-action-in-other-window
    (&rest rest &key run type &allow-other-keys)
  (let ((default-action (embark--default-action type)))
    (message "type is: %s" type)
    (split-window-below) ; or your preferred way to split
    (funcall run :action default-action :type type rest)))

(setf (alist-get 'embark-default-action-in-other-window
                 embark-around-action-hooks)
      '(run-default-action-in-other-window))

(define-key embark-general-map "O" #'embark-default-action-in-other-window)



;; *single-keypress* stuff:
;; - https://www.reddit.com/r/emacs/comments/pac8kp/comment/ha3xx9j/?utm_source=share&utm_medium=web2x&context=3
;; - oantolin response in: https://www.reddit.com/r/emacs/comments/zznamq/how_to_open_file_in_vertical_split_window_from/
