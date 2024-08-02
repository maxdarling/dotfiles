;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Term background exploration
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial issue: term looks kinda meh.
;; - same light background as emacs. maybe nicer to visually distinguish?
;; - "yellow" looks crappy, which strings use

;; goal: set the background color to black and the foreground text to white

;; solution: it's hard?? it's all faces. but not much on the web. i'm confused and
;; just hacking shit together at this point.


;; (custom-set-faces
;;  '(term-color-black ((t (:foreground "#3F3F3F" :background "#2B2B2B"))))
;;  '(term-color-blue ((t (:foreground "#7CB8BB" :background "#4C7073"))))
;;  '(term-color-cyan ((t (:foreground "#93E0E3" :background "#8CD0D3"))))
;;  '(term-color-green ((t (:foreground "#7F9F7F" :background "#9FC59F"))))
;;  '(term-color-magenta ((t (:foreground "#DC8CC3" :background "#CC9393"))))
;;  '(term-color-red ((t (:foreground "#AC7373" :background "#8C5353"))))
;;  '(term-color-white ((t (:foreground "#DCDCCC" :background "#656555"))))
;;  '(term-color-yellow ((t (:foreground "#DFAF8F" :background "#9FC59F"))))
;;  '(term-default-bg-color ((t (:inherit term-color-black))))
;;  '(term-default-fg-color ((t (:inherit term-color-white)))))

;; (custom-set-faces '(term-default-fg-color ((t (:inherit term-color-bright-white)))))

(add-hook 'term-mode-hook
	  #'(lambda()
	      ;; main thing from https://github.com/syl20bnr/spacemacs/issues/11306
	      ;; this changes the fg/bg color of the chars themselves, e.g. white + black
	      (defface term-background
		'((t (:inherit default :foreground "#FFFFFF" :background "#2B2B2B")))
		"Term mode background"
		:group 'basic-faces)

	      (setf (elt ansi-term-color-vector 0) 'term-background) ;; this is needed...

	      ;; other thing: seems to change the buffer background color
	      (set (make-local-variable 'face-remapping-alist)
		   '((default :foreground "#FFFFFF" :background "#2B2B2B")))

	      ;; disable hl-line (doesn't work!)
	      (hl-line-mode -1)
	      ))

(add-hook 'term-mode-hook
	  #'(lambda ()
	      (interactive)
	      (hl-line-mode) ;; also doesn't work.
	      (message "ran hook!")))

;; note: term.el has stuff, e.g. this: 
;; I wonder if the "inherit default" is the same as the black face in emacs. probably!
;; (defface term
;;   `((t :inherit default))
;;   "Default face to use in Term mode."
;;   :group 'term)
