(add-to-list 'load-path "~/.emacs.d/personal")
(load "navigation.el")
(load "js.el")
(load "windows.el")
(load "keychords.el")
(load "org-config.el")
(load "keybindings.el")
(load "magit-config.el")
(load "yas-conf.el")

(require 'which-key)
(require 'modalka)

(which-key-mode)

(add-hook 'term-mode-hook (lambda () (company-mode -1)))

(desktop-save-mode 1)

(rainbow-delimiters-mode)

(define-minor-mode code-review-mode
  (if code-review-mode
      (progn
        (set-face-attribute 'region nil :background "#2B2B2B")
        (text-scale-decrease 2))
    (progn
      (text-scale-increase 2)
      (set-face-attribute 'region nil :background "#19ff00"))))

(setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

(setq semanticdb-global-mode t)

(diff-hl-flydiff-mode)

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

(add-to-list 'Info-default-directory-list "~/info")

(global-hl-line-mode)
(make-variable-buffer-local 'global-hl-line-mode)
(add-hook 'term-mode-hook (lambda () (setq global-hl-line-mode nil)))

(require 'multi-term)
(setq multi-term-program "/bin/zsh")

(set-face-attribute 'default nil :height 150)

(add-to-list 'load-path "~/.emacs.d/vendor/jade-mode")
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl\\'" . stylus-mode))
(add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))
(add-to-list 'auto-mode-alist '("\\.pug\\'" . jade-mode))
(add-hook 'sws-mode-hook 'subword-mode)
(add-hook 'jade-mode-hook 'subword-mode)
(add-hook 'stylus-mode-hook 'subword-mode)

(require 'avy)
(advice-add 'avy-goto-word-1 :before (lambda (&rest r)
                                       (interactive (list (read-char "char: " t)
                                                          (point)))
                                       (set-mark (nth 2 r))
                                       (set-mark (nth 2 r))))

(setq company-tooltip-flip-when-above nil)

(provide 'my-init)
