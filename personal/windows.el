(global-set-key (kbd "<C-s-up>")     'buf-move-up)
(global-set-key (kbd "<C-s-down>")   'buf-move-down)
(global-set-key (kbd "<C-s-left>")   'buf-move-left)
(global-set-key (kbd "<C-s-right>")  'buf-move-right)

(windmove-default-keybindings 'super)

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

(provide 'windows)
