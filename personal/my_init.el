(add-to-list 'load-path "~/.emacs.d/personal")
(load "navigation.el")
(load "js.el")
(load "windows.el")
(load "keychords.el")
(load "org-config.el")
(load "keybindings.el")
(load "magit-config.el")
(load "yas-conf.el")
(load "web-config.el")
(load "term-config.el")

(require 'which-key)
(require 'modalka)

(which-key-mode)

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

(add-to-list 'Info-default-directory-list "~/info")

(set-face-attribute 'default nil :height 200)

(setq company-tooltip-flip-when-above nil)

(advice-add 'save-buffer :before 'whitespace-cleanup)

(provide 'my-init)
