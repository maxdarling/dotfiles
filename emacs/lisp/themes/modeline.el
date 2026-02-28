;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general 
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hide minor modes
(setq mode-line-modes
      '("[" (:propertize mode-name face mode-line-buffer-id) "]"))

;; strip lexical binding hint (not working)
(setq-default mode-name
	          '(:eval (replace-regexp-in-string "/l\\'" "" (format-mode-line mode-name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; chrichton + karthinks v1
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inspo:
;; - https://www.youtube.com/watch?v=bnnacleqg6k
;; - https://karthinks.com/software/it-bears-repeating/

;; default
;; (setq mode-line-format
;;       ("%e" mode-line-front-space
;;        (:propertize
;;         ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
;;          mode-line-window-dedicated)
;;         display (min-width (6.0)))
;;        mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
;;        evil-mode-line-tag (project-mode-line project-mode-line-format) (vc-mode vc-mode) "  "
;;        mode-line-modes mode-line-misc-info mode-line-end-spaces)
;;       )

;; todo
;; - COLORS! (see prot examples?)
;; - trim the lexical mode on elisp?

;; backlog:
;; - crichton: pads line and col separately? https://youtu.be/bnnacleqg6k?si=la7G6oNCJLLA99Pz&t=469

(defvar-local my/mode-line--line-col
  '((:eval (format "%-6s" (format-mode-line "%l:%C")))))

(defvar-local my/mode-line--modified
  '(mode-line-modified))
  ;; '((:eval (if (buffer-modified-p) "*" "-"))))

(defvar-local my/mode-line--evil-state
  ;; evil-mode-line-tag " " ; bug: disappearing after entering insert mode
  '((:eval (format-mode-line evil-mode-line-tag))))

(defvar-local my/mode-line--proj-and-file
  '((:eval
     (let ((p (string-trim
               (format-mode-line '(project-mode-line project-mode-line-format)))))
       (if (string-empty-p p) "" (concat "[" p "]"))))
    mode-line-buffer-identification))

(defvar-local my/mode-line--vc-branch
        '((:eval (when vc-mode
                 (concat ":" (substring-no-properties vc-mode 5)))))) ; strip leading " Git:"

(setq-default mode-line-format
      `(
	"%e"
	,@my/mode-line--line-col
	,@my/mode-line--modified 
	,@my/mode-line--evil-state
	,@my/mode-line--proj-and-file
        "    "
	" %p  "
	,@my/mode-line--vc-branch "  "
	mode-line-modes
	mode-line-misc-info
        mode-line-end-spaces))


(kill-local-variable 'mode-line-format)
(force-mode-line-update t)
