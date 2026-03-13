;;; org-boring-lists.el --- Boring list-editing ergonomics for Org -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Max Darling
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org "9.6"))
;; Keywords: outlines, wp, convenience
;; URL: https://example.com/org-boring-lists

;;; Commentary:

;; org-boring-lists provides boring, common-editor list ergonomics for Org
;; plain lists, closer to Google Docs / many Markdown editors:
;;
;; - RET on a non-empty list item continues the list at the same level
;; - RET on an empty list item either:
;;   - outdents one level at a time, or
;;   - fully outdents to column 0
;;   and at top level removes the item marker entirely
;; - Checkbox items continue as checkbox items
;; - TAB indents the current item subtree
;; - S-TAB outdents the current item subtree
;; - Outside plain lists, normal Org behavior is preserved
;;
;; Enable with:
;;
;;   (require 'org-boring-lists)
;;   (add-hook 'org-mode-hook #'org-boring-lists-mode)
;;
;; Or enable per-buffer with:
;;
;;   M-x org-boring-lists-mode

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'subr-x)

(defgroup org-boring-lists nil
  "Boring list-editing ergonomics for Org plain lists."
  :group 'org
  :prefix "org-boring-lists-")

(defcustom org-boring-lists-empty-item-behavior 'one-level
  "How RET on an empty plain-list item should outdent.

When RET is pressed on an empty item:
- `one-level' outdents one level per keypress
- `full' outdents repeatedly until the item reaches column 0

If the item is already top-level, RET removes the item marker entirely."
  :type '(choice (const :tag "Outdent one level per RET" one-level)
                 (const :tag "Outdent fully to column 0" full))
  :group 'org-boring-lists)

(defcustom org-boring-lists-keep-checkboxes t
  "Whether continuing a checkbox item should create another checkbox item."
  :type 'boolean
  :group 'org-boring-lists)

(defcustom org-boring-lists-disable-blank-line-before-new-item t
  "Whether to suppress Org's automatic blank line before new plain-list items."
  :type 'boolean
  :group 'org-boring-lists)

(defvar-local org-boring-lists--saved-blank-before-new-entry nil
  "Saved original value of `org-blank-before-new-entry' for this buffer.")

(defun org-boring-lists--looking-at-item-p ()
  "Return non-nil if point is on an Org plain-list item."
  (org-at-item-p))

(defun org-boring-lists--current-indentation ()
  "Return indentation of current line."
  (save-excursion
    (beginning-of-line)
    (current-indentation)))

(defun org-boring-lists--top-level-item-p ()
  "Return non-nil if current item is top-level."
  (and (org-boring-lists--looking-at-item-p)
       (= (org-boring-lists--current-indentation) 0)))

(defun org-boring-lists--empty-item-p ()
  "Return non-nil if current item is an empty Org plain-list item."
  (and (org-boring-lists--looking-at-item-p)
       (save-excursion
         (beginning-of-line)
         (when (looking-at org-list-full-item-re)
           (string-blank-p
            (buffer-substring-no-properties
             (match-end 0)
             (line-end-position)))))))

(defun org-boring-lists--checkbox-item-p ()
  "Return non-nil if current item is a checkbox item."
  (and (org-boring-lists--looking-at-item-p)
       (org-at-item-checkbox-p)))

(defun org-boring-lists--item-marker-end ()
  "Return buffer position after the current item's marker, or nil."
  (save-excursion
    (beginning-of-line)
    (when (looking-at org-list-full-item-re)
      (match-end 0))))

(defun org-boring-lists--strip-current-item-marker ()
  "Remove the current item's marker entirely.
Intended for empty top-level items."
  (save-excursion
    (beginning-of-line)
    (when (looking-at org-list-full-item-re)
      (replace-match ""))
    (delete-horizontal-space))
  (beginning-of-line))

(defun org-boring-lists--continue-item ()
  "Continue current plain-list item at the same level."
  (let ((checkboxp (and org-boring-lists-keep-checkboxes
                        (org-boring-lists--checkbox-item-p))))
    (org-insert-item)
    (when checkboxp
      (insert "[ ] "))))

(defun org-boring-lists--outdent-once ()
  "Outdent current item subtree by one level."
  (org-outdent-item-tree))

(defun org-boring-lists--outdent-fully ()
  "Outdent current item subtree to column 0."
  (while (and (org-boring-lists--looking-at-item-p)
              (> (org-boring-lists--current-indentation) 0))
    (org-boring-lists--outdent-once)))

(defun org-boring-lists--handle-empty-item-ret ()
  "Handle RET on an empty item according to user preference."
  (cond
   ((org-boring-lists--top-level-item-p)
    (org-boring-lists--strip-current-item-marker))
   ((eq org-boring-lists-empty-item-behavior 'full)
    (org-boring-lists--outdent-fully))
   (t
    (org-boring-lists--outdent-once))))

(defun org-boring-lists-return ()
  "RET with boring plain-list behavior.
Outside plain lists, preserve normal Org behavior."
  (interactive)
  (cond
   ((org-boring-lists--empty-item-p)
    (org-boring-lists--handle-empty-item-ret))
   ((org-boring-lists--looking-at-item-p)
    (org-boring-lists--continue-item))
   (t
    (org-return nil))))

(defun org-boring-lists-indent-item ()
  "Indent current plain-list item subtree.
Outside plain lists, preserve normal Org TAB behavior."
  (interactive)
  (if (org-boring-lists--looking-at-item-p)
      (org-indent-item-tree)
    (org-cycle)))

(defun org-boring-lists-outdent-item ()
  "Outdent current plain-list item subtree.
Outside plain lists, preserve normal Org S-TAB behavior."
  (interactive)
  (if (org-boring-lists--looking-at-item-p)
      (org-boring-lists--outdent-once)
    (org-shifttab)))

(defun org-boring-lists--enable-buffer-settings ()
  "Enable buffer-local settings used by `org-boring-lists-mode'."
  (when org-boring-lists-disable-blank-line-before-new-item
    (setq-local org-boring-lists--saved-blank-before-new-entry
                org-blank-before-new-entry)
    (setq-local org-blank-before-new-entry
                '((heading . auto)
                  (plain-list-item . nil)))))

(defun org-boring-lists--disable-buffer-settings ()
  "Restore buffer-local settings used by `org-boring-lists-mode'."
  (when org-boring-lists-disable-blank-line-before-new-item
    (setq-local org-blank-before-new-entry
                org-boring-lists--saved-blank-before-new-entry)
    (kill-local-variable 'org-boring-lists--saved-blank-before-new-entry)))

(defvar org-boring-lists-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-boring-lists-return)
    (define-key map (kbd "TAB") #'org-boring-lists-indent-item)
    (define-key map (kbd "<backtab>") #'org-boring-lists-outdent-item)
    map)
  "Keymap for `org-boring-lists-mode'.")

;;;###autoload
(define-minor-mode org-boring-lists-mode
  "Minor mode for boring, common-editor Org plain-list ergonomics."
  :lighter " OBL"
  :keymap org-boring-lists-mode-map
  (if org-boring-lists-mode
      (org-boring-lists--enable-buffer-settings)
    (org-boring-lists--disable-buffer-settings)))

(provide 'org-boring-lists)

;;; org-boring-lists.el ends here
