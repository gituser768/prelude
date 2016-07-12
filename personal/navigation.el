(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(defun mark-to-end-of-paragraph (&optional arg)
  (interactive "p")
  (call-interactively 'set-mark-command)
  (call-interactively 'mark-paragraph))

(defun kill-to-end-of-sexp (&optional arg)
  (interactive "p")
  (call-interactively 'set-mark-command)
  (call-interactively 'sp-up-sexp)
  (call-interactively 'backward-char)
  (call-interactively 'kill-region))

(defun kill-to-beginning-of-sexp (&optional arg)
  (interactive "p")
  (call-interactively 'set-mark-command)
  (call-interactively 'sp-backward-up-sexp)
  (call-interactively 'forward-char)
  (call-interactively 'kill-region))

(defun my-set-mark (arg)
  (interactive "p")
  (if (> arg 0)
      (call-interactively 'set-mark-command)
    (unpop-to-mark-command)))

(require 'avy)
(advice-add 'avy-goto-word-1 :before (lambda (&rest r)
                                       (interactive (list (read-char "char: " t)
                                                          (point)))
                                       (set-mark (nth 2 r))
                                       (set-mark (nth 2 r))))

(provide 'navigation)
