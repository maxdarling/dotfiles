;;;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'project)
(require 'subr-x)

;; Hide minor modes and keep just the major mode name.
(setq mode-line-modes
      '("[" (:propertize mode-name face mode-line-buffer-id) "]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Notes (chrichton + karthinks v1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; inspo:
;;;; - https://www.youtube.com/watch?v=bnnacleqg6k
;;;; - https://karthinks.com/software/it-bears-repeating/
;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface my-modeline-vc-face
  '((t :inherit mode-line
       :foreground "brown"))
  "Face for VC branch segment.")

(defface my-modeline-project-face
  '((t :inherit mode-line))
  "Face for project segment.")

(defface my-modeline-evil-face
  '((t :inherit mode-line
       :weight bold))
  "Face for Evil state segment.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/ml--with-face (face s)
  (when (and s (not (string-empty-p s)))
    (propertize s 'face face)))

(defun my/ml--truncate (s width)
  (when s
    (truncate-string-to-width s width nil nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Segments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/ml-evil-state ()
  (when (boundp 'evil-mode-line-tag)
    (my/ml--with-face
     'my-modeline-evil-face
     (format-mode-line (or evil-mode-line-tag " ")))))

(defun my/ml-project ()
  (let ((proj (project-current nil)))
    (when proj
      (let* ((root (expand-file-name (project-root proj)))
             (name (file-name-nondirectory
                    (directory-file-name root))))
        (my/ml--with-face
         'my-modeline-project-face
         (format "[%s]" (my/ml--truncate name 18)))))))

(defun my/ml-buffer-name ()
  (my/ml--truncate
   (if (fboundp 'sml/generate-buffer-name)
       (sml/generate-buffer-name)
     (format-mode-line mode-line-buffer-identification))
   40))

;; note: i have a feeling there's a more builtin way to do this, e.g.
;; via mode-line-modified and/or mule-info.
;; karthinks might do this much more cleanly...
(defun my/ml-file-state ()
  (let* ((eol (coding-system-eol-type buffer-file-coding-system))
         (coding
          (pcase eol
            (0 "U")
            (1 "D")
            (2 "M")
            (_ "-")))
         (read-only (if buffer-read-only "R" "-"))
         (modified (if (buffer-modified-p) "*" "-")))
    (format "%s%s%s" coding read-only modified)))

(defun my/ml-line-col ()
  (format " %s" (format-mode-line "%l:%c")))

(defun my/ml-vc-branch ()
  (when vc-mode
    (let* ((branch (substring-no-properties vc-mode))
           (branch (replace-regexp-in-string "\\`[[:space:]]*[-:@!\\?]+" "" branch)))
      (when (not (string-empty-p branch))
        (my/ml--with-face
         'my-modeline-vc-face
         (concat ":" branch))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Assembly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default
 mode-line-format
 '("%e"
   (:eval (my/ml-evil-state))
   (:eval (my/ml-line-col))
   " "
   (:eval (my/ml-file-state))
   (:eval (my/ml-buffer-name))
   "    "
   " %p  "
   (:eval (my/ml-vc-branch))
   "  "
   mode-line-modes
   mode-line-misc-info
   mode-line-end-spaces))

(force-mode-line-update t)

(provide 'my-modeline)
