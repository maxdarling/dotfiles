;;;;;;;;;;;;;;;;;;;;;;;;;;;  -*- lexical-binding: t; -*-
;; Todo
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; done:
;; - continue comment by default (elisp), S-RET to not.
;; - try vterm a bit, get M-<right/left> working
;; - setup fzf file and dir.
;; - debug/fix custom-file
;; - setup vscode M-<up>/<down>
;; - fix lexical binding warnings (and understand them...)
;; - make s-, switch to init file
;; - cleanup my-theme.el
;;   - factor out frame sizing.
;;   - refactor into options for background color toggles
;; - keymap revamp phase 1 (and cleanups)
;; - early-init.el (for window stuff)
;; - try harpoon plugin in emacs
;; - bind find-file, find-dir, grep (NOT LEADER)
;; - rg for grep and rgrep
;; - auto-fill-mode for comments only (and learned M-q manual approach too)
;; - project: "H" for custom duplicate and comment func.

;; projects:
;; - emacs as a note taking app
;;   - google docs is the baseline. not my favorite.
;;   - could use obsidian, for sure. but emacs is fun as a hobby? and it
;;   should be able to do all the same stuff.
;; - keyboard diagram visualizer
;; - harpoon: show list in minibuffer (for better ergo looking down than up)
;; - port code-jump from vscode (superior to avy, imo!)

;; emacs as pager deep-dive:
;; - problem: i'm using delta as my differ. not great!
;;   - i want next hunk, normal search, and next file.
;;   - also want files more clearly highlighted
;;   - how good is ediff??
;; - emacs pros:
;;   - supports desires above (keybinds, theming, etc)
;;   - bonus magit features (if i want to learn)
;; - open questions:
;;   - n/N for hunk conflicts with vim search.
;;     - i should look into isearch
;;     - first guess: bind "{" and "}" to file jumps?
;;     - second guess: use M-n/p
;;     - third guess: use s-f style (e.g. vscode-style)
;;     - read up on view mode.
;;       - u and d is mega extra comfort.
;;       - how about motion mode as my own sort of view mode? in magit but also just for perusal
;;     - yak shave reminder: this is essentially keybind planning, it makes me want

;; todo:
;; - bug: fix transient-magit incompatibility (e.g. when pressing "d" to bring up a split diff)
;; - cleanup keymaps + mode keymaps. total mess. too many old comments.
;; - get fill (M-q) to work for not just comments (didn't work for defun docstring)
;; - research autosave (and why .#init.el is being generated)
;; - setup format-on-save (a good pattern IMO)
;; - consult omni search bound to s-p (vscode style)
;; - corfu show "src" next to popup (e.g. like in the screenshots here:
;; https://github.com/minad/corfu)
;; - revamp keymaps (ongoing)
;; 
;; - cleanup init file
;;   - research user-lisp-directory
;;   - general cleanups
;;   - outline mode?
;; - literate emacs config? (via org)
;;   - this would give me functionality for bullets in comments: autocontinue and tab to nest
;;     - this is more like gdocs and markdown

;; vterm notes:
;; - except C-c? Big choice, but likely worth. C-z is my escape hatch.
;; - how to get multi-tab?
;; - explore packages: vterm-toggle, vterm hotkey, vtm

;; LEARNING:
;; - what is xref?
;; - compile commands
;; - elisp fundamentals
;; - recentf mode
;; - imenu

;; IMPORTANT:
;; - visual marks (e.g. IntelliJ style. highlight the line. or, use left "gutter", e.g. like bookmarks)
;; - disable corfu inside comments (no problem in elisp, but it is in go...?)
;; - setup treesitter (elisp/scheme highlighting??) (evil-treesitter-text-obj!) (read karthinks post)

;; BACKBURNER:
;; - paredit (and aggressive-indent-mode)
;; - use emacs transpose functionality. seems powerful. e.g. word, sexp, line with M-t, C-M-t, C-x C-t
;; seems evil has unbound these, though
;; - use sexp-based movement. e.g. C-M-f forward-sexp (but, this doesn't go to next lines, wtf?
;; note that C-M-b/n are related. b works nicely. but n won't let me skip to new lines, wtf.
;; - setup advanced abbrev completion system, like Xah (http://xahlee.info/emacs/emacs/emacs_interactive_abbrev.html)
;; - try ace-window (and combine with embark)
;; - yank-from-kill-ring
;; - TAB -> xah-elisp-complete-or-indent (https://www.youtube.com/live/bHGVp9c7fPg?si=2sPVPRMfTvpqHByF&t=1030)
;; I don't see this in XFK, it must be part of his elisp mode. but I do see xah-reformat-lines.
;; - xah-comment-dwim (set to ;). kinda lit.
;; - copy his syntax highlighting for elisp. see `custom-elisp-mode.el`

(add-to-list 'load-path "~/.emacs.d/lisp")

(setq my/init-file "~/.emacs.d/init.el")
(setq initial-buffer-choice my/init-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)

(setq package-install-upgrade-built-in t)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu". "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
;; (package-refresh-contents) ; tip: only do this manually when desired

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(setq custom-file "~/.emacs.d/customization.el")
(load-file custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; (setq diff-hl-fringe-bmp-function #'diff-hl-fringe-bmp-from-type) ;; adds symbols. bad.
  (setq diff-hl-update-async t)
  (setq diff-hl-draw-borders nil)
  ;; to test:
  ;; - async update on?
  ;; - bmp max width
  ;; - draw borders
  ;; - face ("change", "delete")
  )

(use-package evil
  :init
  (setq evil-search-module 'isearch) ;; bug 8/2/24: 'evil-search breaks search in help/dired/info modes
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)
  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)

  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)
  (use-package evil-surround)
  (global-evil-surround-mode 1)
  (use-package evil-visualstar)
  (global-evil-visualstar-mode t)
  (setq evil-vsplit-window-right t)
  (load-file "~/.emacs.d/lisp/move-text.el"))

(use-package agent-shell
  :ensure t
  :ensure-system-package
  ;; Add agent installation configs here
  ((codex . "brew intstall codex")
   (codex-acp . "brew install codex-acp")))
;; ((claude . "brew install claude-code")
;; (claude-agent-acp . "npm install -g @zed-industries/claude-agent-acp")))

(use-package avy
  :config
  (setq avy-keys '(?a ?h ?e ?t ?i ?s ?c ?n ?y ?x ?w ?v ?u ?r ?p ?o ?m ?l ?k ?j ?g ?f ?d ?b))
  (setq avy-all-windows nil)
  (setq avy-case-fold-search nil)
  (setq avy-timeout-seconds 0.3))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package magit)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

(use-package fireplace)

(use-package wgrep)

(use-package corfu
  :init
  (global-corfu-mode)
  (use-package cape)
  :config
  (setq corfu-auto t
	    corfu-auto-delay .1) ;; .2 default
  (add-to-list 'completion-styles 'flex)

  ;; order matters (first in list = highest priority)
  (advice-add #'cape-dabbrev :around #'cape-wrap-inside-code)
  ;; (defalias 'defensive-cape-line (cape-capf-inside-code #'cape-line))
  ;; (add-to-list 'completion-at-point-functions #'defensive-cape-line) ;; sometimes overpowers elisp?
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-history) ;; haven't tried.
  (add-to-list 'completion-at-point-functions #'cape-file) ;; this is sick...
  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ;; note: dabbrev capf doesn't work.

  ;; todo? scheme keyword complete with prefix length 2 (currently 3)
  ;; to get more completion in scheme mode.
  )

;; fido mode
(fido-vertical-mode 1)
(setq completions-detailed t) ;; add docstrings to each item in completion prompt
(with-eval-after-load 'icomplete
  ;; below is needed when e.g. creating a new file that's a subseq of another
  (define-key icomplete-minibuffer-map (kbd "M-RET") #'exit-minibuffer))
;; (use-package vertico
;; :init
;; (vertico-mode 1))

(use-package consult)

;; (use-package embark
;;   ;; :bind (:map minibuffer-mode-map
;;   ;; note: can't be these, i use these as emacs editing commands in minibuffer
;;   ;; (("C-e" . embark-act)
;;   ;; ("C-a" . embark-dwim)))
;;   :config
;;   ;; my hack to use embark-dwim to do "other-window" stuff in 1 keypress.
;;   ;; see 'lisp/hacks/embark-hacks.el' for more ideas
;;   (setq embark-default-action-overrides
;; 	    '(((buffer . switch-to-buffer) . switch-to-buffer-other-window) ;; works. must be 'buffer'
;; 	      ((command . execute-extended-command) . describe-symbol) ;; this works!! wow!
;; 	      ;; opens dired, and not even on right file.
;; 	      ;; note: same behavior as builtin find-file 'o' option.
;; 	      ((file . find-file) . find-file-other-window)
;; 	      )
;; 	    )
;;   )

(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :config
  (setq visual-fill-column-width 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; treesitter
(require 'treesit)
;; (language definitions built via https://github.com/casouri/tree-sitter-module.git)
;; resources:
;; - https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter
;; - https://www.masteringemacs.org/article/how-to-get-started-tree-sitter

;; (use-package go-mode
;;   ;; :mode ("\.go$") ;; needed?
;;   :init
;;   ;; work around GUI emacs path being different than term/system
;;   (add-to-list 'exec-path "/Users/mhd/go/bin/") ;; gopls
;;   )

(use-package typescript-ts-mode
  :mode ("\.ts$") ;; needed?
  )

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l") ;; needs to happen initially?
  :config
  ;; (setq lsp-keymap-prefix "C-c l")
  (setq lsp-ui-sideline-show-code-actions t)
  ;; (setq lsp-ui-sideline-show-diagnostics t) ;; useless?
  ;; (setq lsp-ui-sideline-show-hover t) ;; just types, very noisy

  :hook (
	     (go-mode . lsp) ;; maybe lsp-deferred?
	     (typescript-ts-mode . lsp))
  :commands (lsp lsp-deferred))
(use-package lsp-ui :commands lsp-ui-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing
;;;;;;;;;;;;;;;;;;;;;;;;;
;; global enable abbrevs
(add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))
(add-hook 'prog-mode-hook (lambda () (abbrev-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;
;; essentials
(set-frame-font "Menlo-15" t t) ;; todo: try prot

;; line numbers for prog-mode only
(setq-default display-line-numbers nil)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(blink-cursor-mode -1)

;; hl line (not in some buffers)
(global-hl-line-mode 1)
(dolist (hook '(ediff-control-mode-hook
                ediff-mode-hook
                term-mode-hook
                vterm-mode-hook
                eshell-mode-hook
                shell-mode-hook
                magit-status-mode-hook
                magit-log-mode-hook
                magit-diff-mode-hook
                agent-shell-mode-hook))
  (add-hook hook (lambda () (setq-local global-hl-line-mode nil))))

;; frame: see 'early-init.el'

(use-package modus-themes
  :demand t
  :bind (("s-t" . modus-themes-toggle))
  :config
  (setq modus-themes-common-palette-overrides
        '(
          ;; Mode line: borderless. Set to `unspecified' for a thinner border.
          (border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)
          ;; Mode line colors.
          (bg-mode-line-active bg-blue-subtle)
          (fg-mode-line-active fg-main)

	      ;; org heading 1 is color same as text. hard to read.
          (fg-heading-1 cyan)
          (fg-heading-5 fg-main)
	      ;; (fg-heading-2 "#d2b580") ;; change operandi to vivendi-tinted
          ))

  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        ;; my/modus-themes-pair '(modus-operandi modus-vivendi-tinted)
        my/modus-themes-pair '(modus-operandi-tinted modus-vivendi-tinted)
        modus-themes-to-toggle my/modus-themes-pair)
  (modus-themes-load-theme (car my/modus-themes-pair)))


;; modeline
(load-file "~/.emacs.d/lisp/themes/modeline.el")

;; Hide continuation arrows in the fringe without affecting other indicators.
(setq-default fringe-indicator-alist
              (let ((alist (copy-tree fringe-indicator-alist)))
                (setcdr (assq 'continuation alist) '(nil nil))
                alist))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Binds
;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/lisp/keymaps.el")
(load-file "~/.emacs.d/lisp/mode-keymaps.el")
(load-file "~/.emacs.d/lisp/mystuff.el")

;; use XFK functions, discard all mappings
;; (setq xah-fly-use-control-key nil)
;; (setq xah-fly-use-meta-key nil)
;; (setq xah-fly-use-isearch-arrows nil)
;; (load-file "~/.emacs.d/lisp/pedagogy/xah-fly-keys.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (load-file "~/.emacs.d/lisp/hacks/org-hacks.el")
(load-file "~/.emacs.d/lisp/packages/org-boring-lists.el") ;; do i need this and the require below?
(require 'org-boring-lists)
(add-hook 'org-mode-hook #'org-boring-lists-mode)
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'org-indent-mode)

;; Less surprising indentation behavior
(setq org-adapt-indentation nil)
;; Allow alphabetical list markers
(setq org-list-allow-alphabetical t)

(setq org-capture-templates
      '(("j" "Journal note"
         entry
         (file+datetree "~/org/journal.org")
         "* %<%H:%M>\n\n%?")))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Settings
;;;;;;;;;;;;;;;;;;;;;;;;;
;; so we can edit files from terminal with the "emacsclient" command.
(server-start)

;; differentiate emacs kill ring and system clipboard
(setq x-select-enable-clipboard nil)
(global-set-key (kbd "s-v") 'clipboard-yank)
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-x") 'clipboard-kill-region)

;; misc low-level
(setq vc-follow-symlinks t)
(setq use-short-answers t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)
(setq dired-listing-switches "-alh --group-directories-first")
(setq insert-directory-program "gls") ;; needed to get above option
(setq help-window-select t) ;; start cursor in help windows. easy exit with 'q'
;; (global-prettify-symbols-mode 1) ;; e.g. lambda
(setq garbage-collection-messages t) ;; starting point before gcmh
(setq shell-command-prompt-show-cwd t)
(setq gc-cons-threshold (* 100 1024 1024) ;; recommended for LSP
      read-process-output-max (* 1024 1024))

;; misc
(winner-mode 1)
(setq which-key-idle-delay 0.2)
(which-key-mode 1)
(savehist-mode 1)

(setq indent-tabs-mode nil)
(setq tab-width 4)

;; autofill comments (or, just M-q it manually)
(column-number-mode 1)
(setq-default fill-column 90)
;; (auto-fill-mode 1)
;; (setq comment-auto-fill-only-comments t)

(setq auto-save-default nil) ;; stop creating autosave files e.g. #file#
(setq make-backup-files nil) ; stop creating ~ files
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(global-set-key [remap list-buffers] 'ibuffer) ;; replace list-buffers with ibuffer

;; rg for grep
(setq grep-command "rg -nS --no-heading "
      grep-use-null-device nil)

;; ediff
(setq ediff-window-setup-function #'ediff-setup-windows-plain)
(setq ediff-split-window-function #'split-window-horizontally)
(setq ediff-keep-variants nil)
(setq ediff-auto-refine 'on)

(run-at-time
 0 nil
 (lambda ()
   (message "Init loaded in %.2fs"
            (float-time (time-subtract after-init-time before-init-time)))))
