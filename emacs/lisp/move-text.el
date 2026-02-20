;; 2/9/2026: tried 'https://github.com/emacsfodder/move-text', but it doesn't work for  -*- lexical-binding: t; -*-
;; regions (in evil). ChatGPT 5.2 gave me this first try and it works.

;; Move line/region up/down and preserve Evil visual selection.
(defun my/move-text--region (start end n)
  "Move the region START..END by N lines."
  (let ((text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((new-start (point)))
      (insert text)
      (setq deactivate-mark nil)
      (cons new-start (point)))))

(defun my/move-text-up ()
  "Move current line or active region up by 1."
  (interactive)
  (if (use-region-p)
      (let* ((v (and (boundp 'evil-state) (evil-visual-state-p)))
             (vtype (and v evil-visual-selection))
             (start (region-beginning))
             (end   (region-end))
             (new (my/move-text--region start end -1)))
        (when v
          (evil-visual-select (car new) (cdr new) vtype)))
    ;; no region: move line
    (transpose-lines 1)
    (forward-line -2)))

(defun my/move-text-down ()
  "Move current line or active region down by 1."
  (interactive)
  (if (use-region-p)
      (let* ((v (and (boundp 'evil-state) (evil-visual-state-p)))
             (vtype (and v evil-visual-selection))
             (start (region-beginning))
             (end   (region-end))
             (new (my/move-text--region start end 1)))
        (when v
          (evil-visual-select (car new) (cdr new) vtype)))
    ;; no region: move line
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))
