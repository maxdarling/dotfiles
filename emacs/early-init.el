;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(top . 1))
(add-to-list 'default-frame-alist '(left . 1))

;; discussion:
;; lots of weird shit here...
;; - corfu popup breaks when default-frame-alist set to (fullscreen . maximized).
;; this is sad. we have to workaround. you could use a hook, but it's very verbose.
;; i like below hardcoded pixel sizes instead.
;; - initial frame specifically won't inherit default's size for some reason. huh.

;; inital frame
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; default frame
;; (setq frame-resize-pixelwise t) ; not needed it seems.
;; 13" laptop: 1450x900
;; 24" monitor: 1500x800
(add-to-list 'default-frame-alist '(width  . (text-pixels . 1500)))
(add-to-list 'default-frame-alist '(height . (text-pixels . 900)))

;; custom frame stuff
;; (setq my/frame-should-cycle-colors nil
;;       my/frame-should-offset-position nil)
;; (load-file "~/.emacs.d/lisp/themes/my-frame.el")
