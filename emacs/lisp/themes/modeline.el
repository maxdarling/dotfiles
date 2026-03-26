;;;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'project)
(require 'subr-x)

;; Hide minor modes and keep just the major mode name.
(setq mode-line-modes
      '("(" (:propertize mode-name face mode-line-buffer-id) ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Notes (chrichton + karthinks v1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; inspo:
;;;; - https://www.youtube.com/watch?v=bnnacleqg6k
;;;; - https://karthinks.com/software/it-bears-repeating/
;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/ml--with-face (face s)
  (when (and s (not (string-empty-p s)))
    (propertize s 'face face)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Segments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/ml-evil-state ()
  (when (boundp 'evil-mode-line-tag)
    (my/ml--with-face
     '(:weight bold)
     (format-mode-line (or evil-mode-line-tag " ")))))

(defun my/ml-buffer-name ()
  (if (fboundp 'sml/generate-buffer-name)
      (sml/generate-buffer-name)
    (format-mode-line mode-line-buffer-identification)))

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
  ;; crichton bolds line num, but it only looks good because the default font is
  ;; de-emphasized.
  ;; (concat
  ;; (propertize (format " %4s" (format-mode-line "%l"))
  ;; 'face 'bold)
  ;; (format ":%-3s" (format-mode-line "%c"))))
  (format " %4s:%-3s"
	  (format-mode-line "%l")
	  (format-mode-line "%c")))

(defun my/ml-vc ()
  (when vc-mode
    (my/ml--with-face
     '(:foreground "brown")
     (format-mode-line vc-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Assembly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default
 mode-line-format
 '("%e"
   (:eval (my/ml-line-col))
   " "
   (:eval (my/ml-file-state))
   (:eval (my/ml-buffer-name))
   "    "
   " %p  "
   (:eval (my/ml-vc))
   "  "
   mode-line-modes
   (:eval (my/ml-evil-state))
   mode-line-misc-info
   mode-line-end-spaces))

(force-mode-line-update t)

(provide 'my-modeline)
