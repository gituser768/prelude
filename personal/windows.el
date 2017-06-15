(use-package windmove
  :config
  (windmove-default-keybindings 'super))

(use-package crux
  :config
  (defun find-at-point-other-window (&optional arg)
    (interactive)
    (other-window 1)
    (crux-switch-to-previous-buffer)
    (find-file-at-point))
  (global-set-key (kbd "C-x 4 RET") 'find-at-point-other-window))

(use-package helm-files
  :config
  (defun split-horiz-find (arg)
    (interactive "p")
    (split-window-below)
    (other-window arg)
    (helm-find-files-1 default-directory))

  (defun split-vert-find (arg)
    (interactive "p")
    (split-window-right)
    (other-window arg)
    (helm-find-files-1 default-directory)))

(use-package escreen
  :config
  ;; (load "escreen")
  (escreen-install))

(provide 'windows)
