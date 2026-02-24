;; -*- lexical-binding: t; -*-
;; my custom binds for special modes like Dired, Info, help, grep, eshell, etc.  
;; these start from the core normal state bindings in 'keybinds.el', but mix in emacs keys where
;; it makes sense.

;; todo 10/9/2024: below "stuff I do in response" #1 is dumb. I should pay the upfront cost of
;; manually porting over all the dired keys I use. Reason: I want to be able to update my keymap
;; in ./keymaps.el and not have to duplicate the effort here for dired.
;; - 2/12/26: maybe. but it works fine for now. no biggie.

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

(defun my/evil-normal-binding (keys)
  "Return the command bound to KEYS in Evil normal state.
KEYS may be a kbd string (e.g. \"C-o\" or \"SPC\") or a key vector (e.g. (kbd \"C-o\"))."
  (let* ((map (cdr (assq 'evil-normal-state-minor-mode evil-mode-map-alist)))
         (keyvec (cond
                  ((stringp keys) (kbd keys))
                  ((vectorp keys) keys)
                  (t (error "KEYS must be a string or vector: %S" keys)))))
    (key-binding keyvec t map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Index
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repl modes (emacs state)
(evil-set-initial-state 'vterm-mode 'emacs)
(evil-set-initial-state 'eshell-mode 'emacs)
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'inferior-scheme-mode 'emacs)

;; Other
(evil-set-initial-state 'wdired-mode 'normal)
(evil-set-initial-state 'package-menu-mode 'normal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key 'normal dired-mode-map
  ;; vim
  (kbd "C-o") (my/evil-normal-binding "C-o")
  (kbd "C-t") (my/evil-normal-binding "C-t")
  (kbd "SPC") (my/evil-normal-binding "SPC")
  "h" (my/evil-normal-binding "h")
  "l" (my/evil-normal-binding "l")
  "r" (my/evil-normal-binding "r")
  "n" 'evil-search-next
  "N" 'evil-search-previous
  "-" 'delete-other-windows

  ;; override dired
  "gg" 'evil-goto-first-line ;; revert buffer. call via M-x. or, gg not that useful...?
  "G" 'evil-goto-line        ;; dired-do-chgrp. useless.
  (kbd "<S-return>") 'dired-display-file ;; this is what *grep* does, very sensible
  "@" 'evil-execute-macro

  ;; match grep follow and writable modes
  (kbd "C-c C-p") 'wdired-change-to-wdired-mode
  (kbd "C-c C-c") 'wdired-finish-edit ;; (already bound)
  (kbd "C-c C-k") 'wdired-abort-changes ;; (already bound)
  (kbd "C-c C-f") 'dired-follow-mode
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key 'motion help-mode-map
  ;; vim
  (kbd "SPC") (my/evil-normal-binding "SPC")
  "h" (my/evil-normal-binding "h")
  "l" (my/evil-normal-binding "l")
  "r" (my/evil-normal-binding "r")
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
  (kbd "C-o") (my/evil-normal-binding "C-o")
  (kbd "C-t") (my/evil-normal-binding "C-t")
  (kbd "SPC") (my/evil-normal-binding "SPC")
  "h" (my/evil-normal-binding "h")
  "l" (my/evil-normal-binding "l")
  "r" (my/evil-normal-binding "r")
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

  (kbd "<down>") 'Info-scroll-up
  (kbd "<up>") 'Info-scroll-down
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
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vterm
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'vterm
  ;; let Emacs handle these keys (don’t send them straight to the pty)
  (add-to-list 'vterm-keymap-exceptions "M-<right>")
  (add-to-list 'vterm-keymap-exceptions "M-<left>"))

(with-eval-after-load 'evil
  (with-eval-after-load 'vterm
    ;; only for vterm + evil emacs state
    (evil-define-key 'emacs vterm-mode-map (kbd "M-<right>")
      (lambda () (interactive) (vterm-send-string "\ef"))) ; ESC f = forward-word
    (evil-define-key 'emacs vterm-mode-map (kbd "M-<left>")
      (lambda () (interactive) (vterm-send-string "\eb"))))) ; ESC b = backward-word

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
  (kbd "SPC") (my/evil-normal-binding "SPC")
  "h" (my/evil-normal-binding "h")
  "l" (my/evil-normal-binding "l")
  "r" (my/evil-normal-binding "r")
  "n" 'evil-search-next
  "N" 'evil-search-previous
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key 'emacs magit-mode-map
  ;; vim
  ;; doesn't work because emacs leader doesn't work (see keymaps.el)
  (kbd "SPC") (my/evil-normal-binding "SPC")
  "h" (my/evil-normal-binding "h")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Term
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key 'emacs term-raw-map
  ;; vim
  "S-v" 'term-paste
  )
