(defun timestamp-yyyymmddhhmmss ()
  (interactive)
  (insert (format-time-string "%Y%m%d%H%M%S")))

(define-minor-mode code-review-mode
  (if code-review-mode
      (progn
        (set-face-attribute 'region nil :background "#444155")
        (text-scale-decrease 2))
    (progn
      (text-scale-increase 2)
      (set-face-attribute 'region nil :background "firebrick"))))
