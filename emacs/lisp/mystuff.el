;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run Current File
;;;;;;;;;;;;;;;;;;;;;;;;;
(defun md-run-current-file ()
  "Run a file. E.g. for scheme, eval it and send to repl.
Based on `xah-run-current-file'"
  (interactive)
  (let* ((fileExt (file-name-extension buffer-file-name))
	 (run-func (cdr (assoc fileExt md-run-current-file-map))))
    (funcall run-func buffer-file-name) 
    ))

(defvar md-run-current-file-map
  '(("scm" . md-run-scheme-file)
    ("el" . md-run-elisp-file)
    ) 
  "Maps file extension to file runner function, used by `md-run-current-file'.")

(defun md-run-scheme-file (&optional filepath) ;; todo: use interactive args
  "Run the given scheme file."
  (interactive)
  (message "Loaded scheme file.")
  (scheme-load-file filepath))

(defun md-run-elisp-file (&optional filepath) ;; tood: use interactive args
  "Run the given elisp file."
  (interactive)
  (message "Evaluated elisp buffer.")
  (eval-buffer nil nil filepath))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SICP
(defun md-setup-project-sicp ()
  (interactive)
  (dired "~/code/sicp/")
  (split-window-right)
  (other-window 1)
  (call-interactively 'run-scheme)
  (other-window 1)
  (set-frame-width (selected-frame) 220) ;; arb. codify later.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc 
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; todo: fix for decimals, e.g 3.28 goes crazy...? must be a precision thing?
;; todo: fix removal of period, e.g. in "5."
;; todo: make number scoped only to right of cursor, e.g. 10.14.11 -> 10.14.12 intead of 10.15.11
(defun md-inc-number-after-point (&optional incval)
  "Increment the current/next number on the current line by 1 (or `incval' if specified)."
  (interactive)
  (let ((inc (or incval 1))
	(origPoint (point)))
    (skip-chars-backward "0-9")
    (skip-chars-forward "^0-9" (pos-eol))
    (if (number-at-point)
	(replace-match (number-to-string (+ inc (number-at-point))))
      (goto-char origPoint))))
