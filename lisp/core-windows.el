;;; core-windows.el --- Window management + escreen -*- lexical-binding: t; -*-

;;; --- Windmove with super ---
(windmove-default-keybindings 'super)

;;; --- Split-and-find helpers ---
(defun find-at-point-other-window (&optional arg)
  (interactive)
  (other-window 1)
  (crux-switch-to-previous-buffer)
  (find-file-at-point))

(global-set-key (kbd "C-x 4 RET") 'find-at-point-other-window)

(defun split-horiz-find (arg)
  (interactive "p")
  (split-window-below)
  (other-window arg)
  (helm-find-files-1 default-directory))

(defun split-vert-find (arg)
  (interactive "p")
  (split-window-right)
  (other-window arg)
  (helm-find-files-1 default-directory))

;;; --- Escreen ---
(load "escreen")
(escreen-install)

(provide 'core-windows)
;;; core-windows.el ends here
