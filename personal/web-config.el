(add-to-list 'load-path "~/.emacs.d/vendor/jade-mode")

(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl\\'" . stylus-mode))
(add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))
(add-to-list 'auto-mode-alist '("\\.pug\\'" . jade-mode))
(add-hook 'sws-mode-hook 'subword-mode)
(add-hook 'jade-mode-hook 'subword-mode)
(add-hook 'stylus-mode-hook 'subword-mode)

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

(provide 'web-config)
