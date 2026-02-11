;; -*- lexical-binding: t; -*-
(defcustom my/frame-should-cycle-colors nil "")
(defcustom my/frame-should-offset-position nil "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cycle frame colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commentary:
;; - next phase: if cycling themes is too cumbersome, perhaps another visual indicator
;; would do, e.g. some colored thing at top of window? karthinks, prot, etc have had this.
;; - the hook doesn't run for the first frame, as in docs. does it matter?
;; - nor does it run for the first entry, apparently.
;;   - but, not a big deal. this is only when mixing the prot theme with this,
;;   which is not intended.
(setq my//bg-color-list 
      '(
	"honeydew"
	"antique white"
	"light yellow"
	"lavender"
	"LightBlue1"
	"gainsboro"
	"White"
	))

;; purpose: "private" current-color via closure
(defun my//make-next-bg-color-fn ()
  (let ((i -1)
	(colors my//bg-color-list))
    (lambda ()
      (setq i (mod (1+ i) (length colors)))
      (message "color: %s" (nth i colors))
      (nth i colors))))

(setq my//next-color-fn (my//make-next-bg-color-fn))

(defun my//cycle-frame-bg-color ()
  (let ((next-color (funcall my//next-color-fn)))
    (setf (alist-get 'background-color default-frame-alist nil nil #'eq) next-color)))

(defun my//cycle-frame-theme ()
  ;; todo: add modus themes cycle
  (my//cycle-frame-bg-color))

;; note: the before- hook avoids flicker, unlike the after- hook.
(when my/frame-should-cycle-colors
  (add-hook 'before-make-frame-hook #'my//cycle-frame-theme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; offset frame positions
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commentary:
;; this could be useful in theory for getting visual feedback
;; on the # of active frames, but it's useless when your frames
;; are fullscreen. theme cycling provides ample visual feedback.

(defun my//cycle-frame-position ()
  ;; macos top bar is 25px
  (let ((n (length (frame-list))))
    (setf (alist-get 'left default-frame-alist nil nil #'eq) (+ 0 (* 5 n)))
    (setf (alist-get 'top default-frame-alist nil nil #'eq) (+ 35 (* 5 n)))))

(when my/frame-should-offset-position
  (add-hook 'before-make-frame-hook #'my//cycle-frame-position))
