;; -*- lexical-binding: t; -*-
;; commentary: make org list indentation behave more like markdown editors (and GDocs)
;;
;; default experience:
;; - RET (`org-return') does not continue list items and indents like a subitem...
;;   ...however this is fixed trivially by using M-RET (`org-meta-return')
;; - checkboxes do not get continued automatically
;; - M-<left> and M-<right> for outdent/indent. not good for prose typing flow.
;;
;; desired experience:
;; - RET continues list items at the same level, *including* checkboxes
;; - RET on empty items outdents them (or removes item marker at top-level)
;; - TAB and S-TAB to indent/outdent. much better for prose typing flow.

(with-eval-after-load 'org
  ;; Stop Org from auto-inserting blank lines before new list items.
  (setq org-blank-before-new-entry
	'((heading . auto)
          (plain-list-item . nil)))

  (defun my/org-tab-dwim ()
    (interactive)
    (cond
     ;; Evil insert-state bindings outrank Corfu's map, so forward TAB when popup is open.
     ((and (bound-and-true-p corfu-mode)
           (bound-and-true-p corfu--candidates))
      (let ((cmd (and (boundp 'corfu-map)
                      (lookup-key corfu-map (kbd "TAB")))))
	(if (commandp cmd)
            (call-interactively cmd)
          (completion-at-point))))
     ((org-at-item-p) (org-metaright))
     (t (org-cycle))))

  (defun my/org-backtab-dwim ()
    (interactive)
    (cond
     ;; Same for S-TAB: let Corfu handle candidate navigation first.
     ((and (bound-and-true-p corfu-mode)
           (bound-and-true-p corfu--candidates))
      (let ((cmd (and (boundp 'corfu-map)
                      (lookup-key corfu-map (kbd "<backtab>")))))
	(if (commandp cmd)
            (call-interactively cmd)
          (when (fboundp 'corfu-previous)
            (call-interactively #'corfu-previous)))))
     ((org-at-item-p) (org-metaleft))
     (t (org-shifttab))))

  (defun my/org-empty-item-p ()
    "Return non-nil if current Org list item is empty."
    (and (org-at-item-p)
	 (save-excursion
           (beginning-of-line)
           (looking-at org-list-full-item-re)
           (string-blank-p
            (buffer-substring-no-properties
             (match-end 0)
             (line-end-position))))))

  (defun my/org-item-top-level-p ()
    "Return non-nil if current Org item has zero indentation."
    (and (org-at-item-p)
	 (save-excursion
           (beginning-of-line)
           (= (current-indentation) 0))))

  (defun my/org-strip-current-item-marker ()
    "Remove bullet / checkbox / numbering from current empty item."
    (beginning-of-line)
    (when (looking-at org-list-full-item-re)
      (replace-match ""))
    (delete-horizontal-space))

  (defun my/org-insert-item-preserve-checkbox ()
    "Insert next sibling item, preserving checkbox if present."
    (let ((checkboxp (org-at-item-checkbox-p)))
      (org-insert-item)
      (when checkboxp
	(insert "[ ] "))))

  (defun my/org-return-dwim ()
    "RET in Org lists like common markdown editors."
    (interactive)
    (cond
     ;; Empty item: outdent one level each time; at top level remove marker.
     ((my/org-empty-item-p)
      (if (my/org-item-top-level-p)
          (my/org-strip-current-item-marker)
	(org-outdent-item-tree)))

     ;; Non-empty item: continue same list level.
     ((org-at-item-p)
      (my/org-insert-item-preserve-checkbox))

     ;; Else normal Org RET.
     (t
      (org-return nil))))

  )
