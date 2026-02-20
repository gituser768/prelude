;;; mod-dev.el --- Development tooling -*- lexical-binding: t; -*-

;;; --- Flycheck ---
(use-package flycheck
  :config
  (setq flycheck-mode-line '(:eval " FC")))

;;; --- Company ---
(use-package company
  :config
  (global-company-mode)
  (setq company-tooltip-flip-when-above nil)
  (diminish 'company-mode))

;;; --- Copilot ---
(use-package copilot
  :vc (:url "https://github.com/zerolfx/copilot.el"
       :branch "main"
       :rev :newest)
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
         ("<tab>" . copilot-accept-completion)
         ("TAB"   . copilot-accept-completion)))

;;; --- Inheritenv ---
(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

;;; --- Eat ---
(use-package eat)

;;; --- Vterm ---
(use-package vterm
  :config
  (setq vterm-max-scrollback 100000))

;;; --- Monet (IDE integration for claude-code) ---
(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

;;; --- Claude-code ---
(use-package claude-code
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :after monet
  :config
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)
  (claude-code-mode)
  (setq claude-code-vterm-buffer-multiline-output t)
  (setq claude-code-vterm-multiline-delay 0.1)
  (setq claude-code-terminal-backend 'vterm)
  :bind-keymap ("C-c c" . claude-code-command-map)
  :bind (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

;;; --- Magit ---
(use-package magit
  :config
  (global-auto-revert-mode -1)
  (magit-auto-revert-mode -1)
  ;; Speed up magit-status open
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  ;; TRAMP avoidance for magit
  (advice-add 'magit-status   :around #'my/ensure-local-default-directory)
  (advice-add 'magit-dispatch :around #'my/ensure-local-default-directory))

;;; --- Git timemachine ---
(use-package git-timemachine :defer t)

;;; --- Git modes ---
(use-package git-modes :defer t)

;;; --- Ag ---
(use-package ag :defer t)

;;; --- PDF tools ---
(use-package pdf-tools
  :config
  (pdf-tools-install))

;;; --- Dired subtree ---
(use-package dired-subtree :defer t)

;;; --- Swiper (standalone, no ivy-mode) ---
(use-package swiper :defer t)

;;; --- Move-text ---
(use-package move-text)

;;; --- Dash ---
(use-package dash)

;;; --- Language modes ---
(use-package markdown-mode :defer t)
(use-package yaml-mode :defer t)
(use-package json-mode :defer t)
(use-package web-mode :defer t)
(use-package csv-mode :defer t)

;;; --- Visual aids ---
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package rainbow-mode
  :defer t
  :hook (prog-mode . rainbow-mode))

;;; --- Elisp navigation ---
(use-package elisp-slime-nav
  :defer t
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

;;; --- Code folding ---
(use-package origami :defer t)

;;; --- GitHub gists ---
(use-package gist :defer t)

;;; --- Imenu across buffers ---
(use-package imenu-anywhere :defer t)

;;; --- Discover keybindings ---
(use-package discover-my-major :defer t)

(provide 'mod-dev)
;;; mod-dev.el ends here
