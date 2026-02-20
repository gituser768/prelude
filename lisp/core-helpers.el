;;; core-helpers.el --- Utility functions -*- lexical-binding: t; -*-

(use-package s)

;;; --- From helpers.el ---

(defun join-symbols (&rest symbols)
  (make-symbol (apply 's-concat (mapcar 'symbol-name symbols))))

(defmacro redef-internal (func func-to-redef redef)
  "For the duration of FUNC, redefine FUNC-TO-REDEF to REDEF."
  `(defadvice ,func (around ,(join-symbols 'my- func) activate)
     (let (orig)
       (fset 'orig (symbol-function (quote ,func-to-redef)))
       (fset (quote ,func-to-redef) ,redef)
       (unwind-protect
           ad-do-it
         (fset (quote ,func-to-redef) (symbol-function 'orig))))))

(defun line-length ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((init (point)))
      (end-of-line)
      (- (point) init))))

;;; --- From navigation.el ---

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
  "Unfill the region, joining text paragraphs into a single logical line."
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
                           (`9 (file-relative-name file (projectile-project-root)))
                           (`8 (projectile-project-name))
                           (_ file))))
              (easy-kill-adjust-candidate 'buffer-file-name text))))))

;;; --- From my_init.el ---

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match the user's shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$" "" (shell-command-to-string
                                          "$SHELL --login -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(require 'ansi-color)
(defun dh-display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun dh-get-weather ()
  (interactive)
  (with-temp-buffer
    (shell-command "curl wttr.in" "*weather*")
    (switch-to-buffer "*weather*")
    (mark-whole-buffer)
    (dh-display-ansi-colors)
    (deactivate-mark)
    (read-only-mode)
    (delete-other-windows)
    (beginning-of-buffer)))

(defun my/localize-default-directory-in-nonfile-buffers ()
  "If a non-file buffer inherits a remote default-directory, reset it."
  (when (and (not buffer-file-name)
             (stringp default-directory)
             (file-remote-p default-directory))
    (setq default-directory (expand-file-name "~"))))
(add-hook 'after-change-major-mode-hook #'my/localize-default-directory-in-nonfile-buffers)

(defun my/ensure-local-default-directory (orig-fun &rest args)
  (let ((default-directory (if (and (stringp default-directory)
                                    (file-remote-p default-directory))
                               (expand-file-name "~")
                             default-directory)))
    (apply orig-fun args)))

(provide 'core-helpers)
;;; core-helpers.el ends here
