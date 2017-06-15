(use-package jade-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))
  (add-to-list 'auto-mode-alist '("\\.pug\\'" . jade-mode))
  (add-hook 'jade-mode-hook 'subword-mode))

(use-package sws-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.styl\\'" . stylus-mode))
  (add-hook 'sws-mode-hook 'subword-mode)
  (add-hook 'stylus-mode-hook 'subword-mode))

(use-package skewer-mode
  :config
  (add-hook 'js2-mode-hook 'skewer-mode))

(use-package skewer-css
  :config
  (add-hook 'css-mode-hook 'skewer-css-mode))

(use-package skewer-html
  :config
  (add-hook 'html-mode-hook 'skewer-html-mode))

(provide 'web-config)
