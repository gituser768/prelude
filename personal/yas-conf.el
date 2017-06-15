;; Turn on snippets
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode)
  (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1))))
