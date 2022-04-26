(defun crux-duplicate-current-line-or-region (arg)
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (crux-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (dotimes (_i arg)
      (goto-char end)
      (newline)
      (insert region)
      (setq end (point)))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun crux-duplicate-and-comment-current-line-or-region (arg)
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (crux-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (comment-or-uncomment-region beg end)
    (setq end (line-end-position))
    (dotimes (_ arg)
      (goto-char end)
      (newline)
      (insert region)
      (setq end (point)))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun move-to-first-alpha (arg)
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (beginning-of-line)
    (re-search-forward "[a-zA-Z]")
    (backward-char)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun move-to-next-term ()
  (interactive)
  (skip-chars-forward "^, ")
  (skip-chars-forward "^a-zA-Z"))

(defun move-to-prev-term ()
  (interactive)
  (skip-chars-backward "^ ")
  (skip-chars-backward " ")
  (skip-chars-backward "^ "))

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
  (set-mark (point))
  (call-interactively 'mark-paragraph))

(defun kill-to-end-of-sexp (&optional arg)
  (interactive "p")
  (set-mark (point))
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
      (if (region-active-p)
          (deactivate-mark)
        (call-interactively 'set-mark-command))
    (unpop-to-mark-command)))

(require 'avy)
(advice-add 'avy-goto-word-1 :before (lambda (&rest r)
                                       (interactive (list (read-char "char: " t)
                                                          (point)))
                                       (set-mark (nth 2 r))
                                       (set-mark (nth 2 r))))

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(defun first-non-whitespace ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((cur-pt (point))
          (new-pt (re-search-forward "[^[:space:]]")))
      (when new-pt
        (- new-pt cur-pt)))))

(defun end-of-coffee-block ()
  (interactive)
  (let ((init-col (first-non-whitespace)))
    (forward-line)
    (while (> (first-non-whitespace) init-col)
      (forward-line)))
  (forward-line -1)
  (end-of-line))

(defun up-one-coffee-block ()
  (interactive)
  (let ((init-col (first-non-whitespace)))
    (when (not (= init-col 1))
      (forward-line -1)
      (while (>= (first-non-whitespace) init-col)
        (forward-line -1))))
  (call-interactively 'crux-move-beginning-of-line))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun yank-and-pop ()
  (interactive)
  (call-interactively 'yank)
  (pop kill-ring))

(setq filter-buffer-substring-function
      (lambda (beg end delete)
        (save-excursion
          (let ((killed-string (chomp (buffer-substring beg end))))
            (when delete (delete-region beg end))
            killed-string))))

(defun easy-kill-on-buffer-file-name (n)
  "Get `buffer-file-name' or `default-directory'.
If N is zero, remove the directory part; -, remove the file name
part; +, full path."
  (if (easy-kill-get mark)
      (easy-kill-echo "Not supported in `easy-mark'")
    (pcase (or buffer-file-name default-directory)
      (`nil (easy-kill-echo "No `buffer-file-name'"))
      (file (let* ((file (directory-file-name file))
                   (text (pcase n
                           (`- (file-name-directory file))
                           (`0 (file-name-nondirectory file))
                           (`9 (rng-relative-path file (projectile-project-root)))
                           (`8 (projectile-project-name))
                           (_ file))))
              (easy-kill-adjust-candidate 'buffer-file-name text))))))

(provide 'navigation)
