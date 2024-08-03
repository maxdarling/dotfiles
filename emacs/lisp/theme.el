;; Xah theme
(if (not (display-graphic-p))
    (load-theme 'leuven-dark)
  (progn
    (setq initial-frame-alist
	  '(
	    (tool-bar-lines . 0)
	    (vertical-scroll-bars . nil)
	    (width . 200)
	    (height . 55)
	    (background-color . "honeydew")
	    (left . 10)
	    (top . 10)))
    (setq default-frame-alist
	  '(
	    (tool-bar-lines . 0)
	    (vertical-scroll-bars . nil)
	    (width . 200)
	    (height . 55)
	    (background-color . "antique white")
	    (left . 30)
	    (top . 30)))))

;; cycle colors
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
	  (next-color (ring-next my//background-color-ring current-color)))
     (setcdr (assq 'background-color default-frame-alist) next-color))))


;; misc
(set-frame-font "Menlo-15" t t)
(global-hl-line-mode 1)
(blink-cursor-mode -1)
