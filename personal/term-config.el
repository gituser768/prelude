(add-hook 'term-mode-hook (lambda () (company-mode -1)))

(global-hl-line-mode)
(make-variable-buffer-local 'global-hl-line-mode)
(add-hook 'term-mode-hook (lambda () (setq global-hl-line-mode nil)))

(require 'multi-term)
(setq multi-term-program "/bin/zsh")

(provide 'term-config)
