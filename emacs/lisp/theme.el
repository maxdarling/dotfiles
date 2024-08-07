;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq my//frame-start-pos-left 10)
(setq my//frame-start-pos-top 35) ;; macOS top "bar" size is 25

(if (not (display-graphic-p))
    (load-theme 'leuven-dark)
  (progn
    (setq initial-frame-alist
	  `(
	    (tool-bar-lines . 0)
	    (vertical-scroll-bars . nil)
	    (width . 200)
	    (height . 55)
	    (background-color . "honeydew")
	    (left . ,my//frame-start-pos-left)
	    (top . ,my//frame-start-pos-top))) 
    (setq default-frame-alist initial-frame-alist)))

;; colors to cycle
(setq my//background-color-ring (ring-convert-sequence-to-ring
				 '(
				   "antique white"
				   "light yellow"
				   "lavender"
				   "LightBlue1"
				   "gainsboro"
				   "honeydew"
				   )))

(add-hook
 'before-make-frame-hook
 (lambda()
   (let* ((current-color (alist-get 'background-color default-frame-alist))
	  (next-color (ring-next my//background-color-ring current-color))
	  (frame-pos-offset (* 5 (length (frame-list)))))
     ;; cycle color
     (setcdr (assq 'background-color default-frame-alist) next-color)
     ;; offset frame position
     (setcdr (assq 'top default-frame-alist) (+ my//frame-start-pos-top frame-pos-offset))
     (setcdr (assq 'left default-frame-alist) (+ my//frame-start-pos-left frame-pos-offset))
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-frame-font "Menlo-15" t t)
(global-hl-line-mode 1)
(blink-cursor-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
