;;;;;;;;;;;;;;;;;;;;;;;;;  -*- lexical-binding: t; -*-
;; Run Current File
;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my//run-file-map
  '((el   . my/run-elisp-file)
    (scm  . my/run-scheme-file)
    (html . my/run-html-file)))

(defun my/run-file ()
  "Run current file based on its extension. Simplified adaptation of `xah-run-current-file'"
  (interactive)
  (let* ((ext (file-name-extension (or buffer-file-name ""))))
    (funcall
     (alist-get (and ext (intern ext))
                my//run-file-map
                #'my/run-elisp-file))))

(defun my/run-elisp-file (&optional filepath)
  "Run the given elisp file."
  (interactive "bEval buffer: ")
  (eval-buffer filepath)
  (message "Evaluated elisp buffer."))

(defun my/run-scheme-file (&optional filepath)
  "Run the given scheme file."
  (interactive "bEval scheme buffer: ")
  (scheme-load-file (or filepath buffer-file-name))
  (message "Loaded scheme file."))

(defun my/run-html-file (&optional filepath)
  "Run the given html file."
  (interactive "bFile to run: ")
  (browse-url (or filepath buffer-file-name))
  (message "Opened html file in browser."))

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
  (other-window 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc 
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/comment-and-duplicate ()
  "Copy the current line/region, comment it, and place it above point."
  (interactive)
  ;; region annoyance:
  ;; - behavior: when run on region and cursor is on bottom line, an extra line is jumped.
  ;; works fine for single-line and when cursor is on top line of region.
  ;; - reason: internal impl of region, somewhere, represents the end of the region
  ;; (recall: emacs region = between "mark" and "point") as beginning of line ("bol") of
  ;; next line. in debug print below, point line number is 1 greater than we expect,
  ;; i.e. on the next line.
  ;; - solution: set a mark to jump back to (e.g. to start of region)
  ;; - desired solution: have your cursor position appear unchanged before/after running command,
  ;; i.e. a commented duplication appeared above you without movement. chatGPT 5.2 couldn't figure
  ;; it out though - no prob, i'm quite happy with the current solution.

  ;; (message "pt line=%d bolp=%S | mark line=%d | rb=%d re=%d"
  ;;          (line-number-at-pos (point)) (bolp)
  ;;          (line-number-at-pos (mark))
  ;;          (line-number-at-pos (region-beginning))
  ;;          (line-number-at-pos (region-end)))
  (let* ((n-lines (if (use-region-p)
		      (count-lines (region-beginning) (region-end))
		    1))
         (anchor (if (use-region-p) (region-beginning) (line-beginning-position))))
    (duplicate-dwim 1) ;; region stays active!
    (my/comment-line-dwim t) ;; don't advance point
    ;; below: must deactivate manually for predictable movement.
    ;; see command loop notes in 'deactivate-mark' for explainer.
    (deactivate-mark) 
    (goto-char anchor)
    (forward-line n-lines)))

(defun my/comment-line-dwim (&optional dont-advance-point)
  "Comment line(s). Needed because `comment-line' on a region erroneously comments an extra line.
  If `dont-advance-point' is t, don't move point to next line after commenting
  (for single-line comments only)" 
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (comment-line 1)
    (if dont-advance-point (previous-line))))

;; todo: fix for decimals, e.g 3.28 goes crazy...? must be a precision thing?
;; todo: fix removal of period, e.g. in "5."
;; todo: make number scoped only to right of cursor, e.g. 10.14.11 -> 10.14.12 intead of 10.15.11
(defun my/inc-number-after-point (&optional incval)
  "Increment the current/next number on the current line by 1 (or `incval' if specified)."
  (let ((inc (or incval 1))
	(origPoint (point)))
    (skip-chars-backward "0-9")
    (skip-chars-forward "^0-9" (pos-eol))
    (if (number-at-point)
	(replace-match (number-to-string (+ inc (number-at-point))))
      (goto-char origPoint))))

;; source: https://www.reddit.com/r/emacs/comments/13y5k9j/simple_fuzzy_find_file/
(defun my/find-file-rec ()
  "Find a file recursively from the current working directory. rg highly recommended."
  (interactive)
  (let ((find-files-program
	 (cond ((executable-find "rg") '("rg" "--color=never" "--files"))
	       ((executable-find "find") '("find" "." "-type" "f")))))
    (find-file
     (completing-read "Find file: " (apply #'process-lines find-files-program)))))

(defun my/find-dir-rec ()
  "Find a directory recursively from the current working directory. rg highly recommended."
  (interactive)
  (let ((find-files-program
	 (cond ((executable-find "fd") '("fd" "--type=d"))
	       ((executable-find "find") '("find" "." "-type" "f")))))
    (find-file
     (completing-read "Find dir: " (apply #'process-lines find-files-program)))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inspo
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - nice workflow for "search-and-replace on steroids" https://lambdaland.org/posts/2023-05-31_warp_factor_refactor/
