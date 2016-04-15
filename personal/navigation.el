(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(defun my-set-mark (arg)
  (interactive "p")
  (if (> arg 0)
      (call-interactively 'set-mark-command)
    (unpop-to-mark-command)))

(provide 'navigation)
