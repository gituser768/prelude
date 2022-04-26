(defun timestamp-yyyymmddhhmmss ()
  (interactive)
  (insert (format-time-string "%Y%m%d%H%M%S")))
