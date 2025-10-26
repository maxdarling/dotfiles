;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run Current File
;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/run-current-file ()
  "Run a file. E.g. for scheme, eval it and send to repl.
Based on `xah-run-current-file'"
  (interactive)
  (let* ((fileExt (file-name-extension buffer-file-name))
	 (run-func (cdr (assoc fileExt my/run-current-file-map))))
    (funcall run-func buffer-file-name) 
    ))

(defvar my/run-current-file-map
  '(("scm" . my/run-scheme-file)
    ("el" . my/run-elisp-file)
    ("html" . my/run-html-file)
    ) 
  "Maps file extension to file-runner function. Used by `my/run-current-file'.")

(defun my/run-scheme-file (&optional filepath)
  "Run the given scheme file."
  (interactive)
  (message "Loaded scheme file.")
  (scheme-load-file filepath))

(defun my/run-elisp-file (&optional filepath)
  "Run the given elisp file."
  (interactive)
  (message "Evaluated elisp buffer.")
  (eval-buffer nil nil filepath))

(defun my/run-html-file (&optional filepath)
  "Run the given html file."
  (interactive)
  (message "Opened html file in browser.")
  (browse-url filepath))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SICP
(defun my/setup-project-sicp ()
  (interactive)
  (require 'bookmark)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "sicp")
  (split-window-right)
  (other-window 1)
  (call-interactively 'run-scheme)
  (other-window 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc 
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; todo: fix for decimals, e.g 3.28 goes crazy...? must be a precision thing?
;; todo: fix removal of period, e.g. in "5."
;; todo: make number scoped only to right of cursor, e.g. 10.14.11 -> 10.14.12 intead of 10.15.11
(defun my/inc-number-after-point (&optional incval)
  "Increment the current/next number on the current line by 1 (or `incval' if specified)."
  (interactive)
  (let ((inc (or incval 1))
	(origPoint (point)))
    (skip-chars-backward "0-9")
    (skip-chars-forward "^0-9" (pos-eol))
    (if (number-at-point)
	(replace-match (number-to-string (+ inc (number-at-point))))
      (goto-char origPoint))))

;; improve 'comment-line', and don't rely on evil-commentary (for fun + reduce bloat)
;; issues:
;; - visual line always comments out +1 lines
;; - always moves your cursor to next line... (line or region) (can be good, but try off)
;; note: next-line is good because you can dot it (?).
(defun my/comment-dwim ()
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (comment-line 1)))

;; source: https://www.reddit.com/r/emacs/comments/13y5k9j/simple_fuzzy_find_file/
(defun my/find-file-rec ()
  "Find a file recursively from the current working directory."
  (interactive)
  (let ((find-files-program
	 (cond ((executable-find "rg") '("rg" "--color=never" "--files"))
               ((executable-find "find") '("find" "." "-type" "f")))))
    (find-file
     (completing-read "Find file: " (apply #'process-lines find-files-program)))))

;; source: https://emacs.stackexchange.com/questions/51592/enable-follow-mode-in-dired
;; goal: emulate grep follow mode
(define-minor-mode dired-follow-mode
  "Diplay file at point in dired after a move."
  :lighter " dired-follow"
  :global t
  (cond (dired-follow-mode
	 (advice-add 'evil-next-line :after (lambda (arg) (dired-display-file)))
	 (advice-add 'evil-previous-line :after (lambda (arg) (dired-display-file))))
	(t
	 (advice-remove 'evil-next-line (lambda (arg) (dired-display-file)))
	 (advice-remove 'evil-previous-line (lambda (arg) (dired-display-file))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Align hacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://stackoverflow.com/questions/22710040/emacs-align-regexp-with-spaces-instead-of-tabs
;; force align-regexp to not use tabs. I was convinced this is good in 6.824 where the authors
;; are using tabs in the file, but did their argument type aligning via minimal spaces
;; (defadvice align-regexp (around align-regexp-with-spaces activate)
;;   (let ((indent-tabs-mode nil))
;;     ad-do-it))


;; align by space -- untested, is this useful? running a more heavyweight prettifier might be
;; what I want...
(defun my/align (BEG END)
  (interactive "r")
  (let ((indent-tabs-mode nil))
    (align-regexp BEG END (concat "\\(\\s-*\\)" " ") 1 1)))
