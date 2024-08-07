;; my custom binds for special modes like Dired, Info, help, grep, eshell, etc.
;; these start from the core normal state bindings in 'keybinds.el', but mix in emacs keys where
;; it makes sense.

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Discussion
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - useful resource for advanced evil keymap details: https://github.com/noctuid/evil-guide

;; Stuff that evil does by default:
;; - base normal/motion for a mode on default emacs binds via 'evil-make-overriding-map' (e.g. Dired)
;; - Map hjkl keys on top of a mode via 'evil-add-hjkl-bindings' (e.g. info mode. see 'evil-keybindings.el')

;; Stuff that I do in response:
;; 1. keep the evil overrides, with a few of my own tweaks. this is done via a higher-precedence
;; override. see precedences in the file comment for 'evil-core.el'. I do this for Dired, for example,
;; because there are *many* dired keys, and many are good by default (single-key).
;; 2. scrap the evil overrides. this is better when there's only a few useful default keys, in which
;; case it's easier for me to add those specifically. I do this for help/info mode.

;; Other thoughts:
;; - many special modes have overlaps. R-only modes especially. e.g. tabbing through links, or going
;; forward/back in history, or quitting with 'q'.
;; - some modes have special core keys. like 'm' in Info for the menu.
;; - in all of these modes, I want my own 'h', 'l', 'r', SPC, C-u/C-d, at least.
;; - Writeable modes are harder. E.g. shell or scheme repl. those have to be in emacs state, I think.
;; I guess I'm fine with that, solution being having to C-z every time. Might be improveable, though.  

;; Future Todo:
;; - I can write my own state...! Would it be useful?
;; - To avoid redundantly specifying emacs key behavior, e.g. TAB in help and info are ~SAME but have
;; different functions, I can use general: (general-key "C-n" :state 'emacs) to emulate the emacs key.
;; this would save some headache? and then I can put that into my own state, I guess (or motion, still)
;; this tip came from https://github.com/noctuid/evil-guide?tab=readme-ov-file#using-emacs-keybindings-in-normal-state 


;; todo: build abstraction on top of this. 
(defvar normal-mode-map (cdr (alist-get 'evil-normal-state-minor-mode evil-mode-map-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Index
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repl modes (emacs state)
(evil-set-initial-state 'eshell-mode 'emacs)
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'inferior-scheme-mode 'emacs)

;; Other
(evil-set-initial-state 'wdired-mode 'normal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key 'normal dired-mode-map
  ;; vim
  (kbd "SPC") (alist-get (string-to-char " ") normal-mode-map)
  "h" (alist-get ?h normal-mode-map)
  "l" (alist-get ?l normal-mode-map)
  "r" (alist-get ?r normal-mode-map)
  "n" 'evil-search-next
  "N" 'evil-search-previous
  "-" 'delete-other-windows

  ;; override dired
  "gg" 'evil-goto-first-line ;; revert buffer. call via M-x. or, gg not that useful...?
  "G" 'evil-goto-line        ;; dired-do-chgrp. useless.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key 'motion help-mode-map
  ;; vim
  (kbd "SPC") (alist-get (string-to-char " ") normal-mode-map)
  "h" (alist-get ?h normal-mode-map)
  "l" (alist-get ?l normal-mode-map)
  "r" (alist-get ?r normal-mode-map)
  "n" 'evil-search-next
  "N" 'evil-search-previous

  ;; general RO emacs
  (kbd "S-<tab>") 'backward-button
  (kbd "<tab>") 'forward-button
  (kbd "C-<left>") 'help-go-back
  (kbd "C-<right>") 'help-go-forward
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Info
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key 'motion Info-mode-map
  ;; vim
  (kbd "SPC") (alist-get (string-to-char " ") normal-mode-map)
  "h" (alist-get ?h normal-mode-map)
  "l" (alist-get ?l normal-mode-map)
  "r" (alist-get ?r normal-mode-map)
  "n" 'evil-search-next
  "N" 'evil-search-previous

  ;; general RO emacs
  (kbd "<return>") 'Info-follow-nearest-node
  (kbd "S-<tab>") 'Info-prev-reference
  (kbd "<tab>") 'Info-next-reference
  (kbd "C-<left>") 'Info-history-back
  (kbd "C-<right>") 'Info-history-forward

  ;; Info-specific emacs
  "," 'Info-index-next
  "a" 'Info-goto-node
  "A" 'Info-history
  "u" 'Info-toc

  "[" 'Info-backward-node
  "]" 'Info-forward-node
  "^" 'Info-up
  ;; my solution to n/p node navigation. but this is not comfy at all, hmm.
  ;; numbers are open...
  "{" 'Info-prev 
  "}" 'Info-next
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grep
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil sets no overrides for this mode, so we just need to enable a few keys
(evil-define-key 'motion grep-mode-map
  ;; general RO emacs
  (kbd "S-<tab>") 'compilation-previous-error
  (kbd "<tab>") 'compilation-next-error

  ;; grep-specific 
  (kbd "S-<return>") 'compilation-display-error
  "[" 'previous-error-no-select ;; trying these (formerly n/p). they seem *good*.
  "]" 'next-error-no-select
  ;; todo: rebind C-c C-f ?
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; can't set to motion state. rats.
;; related issue: https://github.com/emacs-evil/evil/issues/1115
(evil-set-initial-state 'messages-buffer-mode 'motion) ;; broken: msg buffer inits first.
(add-hook 'messages-buffer-mode-hook #'evil-motion-state) ;; surprisingly, also broken.
(add-hook 'messages-buffer-mode-hook 'evil-insert-state)
(add-hook 'messages-buffer-mode-hook (lambda () (interactive) (message "hello!") (evil-motion-state)))
(evil-define-key 'motion message-mode-map
  ;; vim
  (kbd "SPC") (alist-get (string-to-char " ") normal-mode-map)
  "h" (alist-get ?h normal-mode-map)
  "l" (alist-get ?l normal-mode-map)
  "r" (alist-get ?r normal-mode-map)
  "n" 'evil-search-next
  "N" 'evil-search-previous
  )
