;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general 
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'subr-x)   ;; string-trim, string-empty-p
(require 'project)  ;; project-current

;; hide minor modes
(setq mode-line-modes
      '("[" (:propertize mode-name face mode-line-buffer-id) "]"))

;; strip lexical binding hint (not working)
(setq-default mode-name
	      '(:eval (replace-regexp-in-string "/l\\'" "" (format-mode-line mode-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; notes (chrichton + karthinks v1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inspo:
;; - https://www.youtube.com/watch?v=bnnacleqg6k
;; - https://karthinks.com/software/it-bears-repeating/

;; todo
;; - COLORS! (see prot examples?)
;; - add path prefix to the filename in projects
;; - trim the lexical mode on elisp?

;; backlog:
;; - crichton: pads line and col separately? https://youtu.be/bnnacleqg6k?si=la7G6oNCJLLA99Pz&t=469

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface my-modeline-vc-face
  '((t :inherit mode-line
       :foreground "brown"
       ))
  "Face for VC branch segment.")

(defface my-modeline-project-face
  '((t :inherit mode-line
       ))
  "Face for project segment.")

(defface my-modeline-buffer-face
  '((t :inherit mode-line
       :weight bold))
  "Face for buffer name.")

(defface my-modeline-evil-face
  '((t :inherit mode-line
       :weight bold))
  "Face for Evil state segment.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/ml--with-face (face s)
  (when (and s (not (string-empty-p s)))
    (propertize s 'face face)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Segments
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/ml-modified ()
  (format-mode-line mode-line-modified))

(defun my/ml-evil-state ()
  (when (boundp 'evil-mode-line-tag)
    (my/ml--with-face
     'my-modeline-evil-face
     (format-mode-line (or evil-mode-line-tag " ")))))

(defun my/ml-project ()
  (let ((proj (project-current nil))) ; nil = don’t prompt
    (when proj
      (let* ((root (expand-file-name (project-root proj)))
             (name (file-name-nondirectory
                    (directory-file-name root))))
        (my/ml--with-face
         'my-modeline-project-face
         (format "[%s]" name))))))

(defun my/ml-buffer-name ()
  (my/ml--with-face
   'my-modeline-buffer-face
   (format-mode-line mode-line-buffer-identification)))

(defun my/ml-line-col ()
  ;; (format " %-7s" (format-mode-line "%l:%c")))
  (format " %-3s" (format-mode-line "%l")))

(defun my/ml-percent ()
  (format-mode-line " %p  "))

(defun my/ml-vc-branch ()
  (when vc-mode
    (my/ml--with-face
     'my-modeline-vc-face
     (concat ":" (substring-no-properties vc-mode 5)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assembly
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default
 mode-line-format
 '("%e"
   (:eval (my/ml-modified))
   " "
   (:eval (my/ml-evil-state))
   (:eval (my/ml-project))
   (:eval (my/ml-buffer-name))
   "    "
   (:eval (my/ml-line-col))
   (:eval (my/ml-percent))
   (:eval (my/ml-vc-branch))
   "  "
   mode-line-modes
   mode-line-misc-info
   mode-line-end-spaces))

(force-mode-line-update t)

(provide 'my-modeline)
