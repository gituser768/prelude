;;; core-defaults.el --- Editor defaults -*- lexical-binding: t; -*-

;;; --- Indentation & basic editing ---
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq require-final-newline t)
(delete-selection-mode t)
(setq tab-always-indent 'complete)

;;; --- Backups & autosaves in temp dir ---
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; --- Hippie expand ---
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
(setq hippie-expand-verbose t)

;;; --- Smartparens ---
(use-package smartparens
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (setq sp-highlight-pair-overlay nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  (add-hook 'comint-mode-hook 'turn-off-show-smartparens-mode))

;;; --- Disable blink-matching-paren (smartparens handles it) ---
(setq blink-matching-paren nil)

;;; --- Diminish ---
(use-package diminish)

;;; --- Uniquify ---
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;;; --- Save-place ---
(setq save-place-file (expand-file-name "saveplace" my-savefile-dir))
(save-place-mode 1)

;;; --- Savehist ---
(require 'savehist)
(setq savehist-additional-variables '(search-ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (expand-file-name "savehist" my-savefile-dir))
(savehist-mode +1)

;;; --- Recentf ---
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" my-savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      recentf-auto-cleanup 'never)
(recentf-mode +1)

;;; --- Windmove ---
(require 'windmove)
(windmove-default-keybindings)

;;; --- Super-save ---
(use-package super-save
  :config
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1)
  (diminish 'super-save-mode)
  ;; Extra save triggers
  (advice-add 'delete-window :before
              (lambda (&optional window) (super-save-command-advice)))
  (advice-add 'select-window :before
              (lambda (window &optional norecord) (super-save-command-advice)))
  (advice-add 'other-frame :before
              (lambda (arg) (super-save-command-advice))))

;;; --- Highlight current line (disable in term-mode) ---
(global-hl-line-mode +1)
(make-variable-buffer-local 'global-hl-line-mode)

;;; --- Volatile highlights ---
(use-package volatile-highlights
  :config
  (volatile-highlights-mode t)
  (diminish 'volatile-highlights-mode))

;;; --- Crux ---
(use-package crux
  :config
  (require 'rect)
  (crux-with-region-or-line kill-region))

;;; --- Projectile ---
(use-package projectile
  :config
  (setq projectile-cache-file (expand-file-name "projectile.cache" my-savefile-dir))
  (setq projectile-mode-line
        '(:eval (if (file-remote-p default-directory)
                    " Projectile"
                  (format " %s" (projectile-project-name)))))
  (projectile-mode t))

;;; --- Avy ---
(use-package avy
  :config
  (setq avy-background t)
  (setq avy-style 'at-full))

;;; --- Anzu ---
(use-package anzu
  :config
  (diminish 'anzu-mode)
  (global-anzu-mode)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

;;; --- Dired ---
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-dwim-target t)
(require 'dired-x)

;;; --- Ediff ---
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; --- Compilation ---
(require 'compile)
(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error)

(require 'ansi-color)
(defun my-colorize-compilation-buffer ()
  "Colorize compilation output."
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))
(add-hook 'compilation-filter-hook #'my-colorize-compilation-buffer)

;;; --- Winner mode ---
(winner-mode +1)

;;; --- Diff-hl ---
(use-package diff-hl
  :config
  (global-diff-hl-mode +1)
  (diff-hl-margin-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;; --- Easy-kill ---
(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

;;; --- Expand region ---
(use-package expand-region
  :config
  (setq expand-region-fast-keys-enabled nil))

;;; --- WS-butler (whitespace cleanup) ---
(use-package ws-butler
  :config
  (setq ws-butler-keep-whitespace-before-point t)
  (ws-butler-global-mode)
  (add-to-list 'ws-butler-global-exempt-modes 'org-mode))

;;; --- Enable narrowing, case, erase-buffer ---
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;;; --- Bookmark ---
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" my-savefile-dir)
      bookmark-save-flag 1)

;;; --- Re-builder ---
(require 're-builder)
(setq reb-re-syntax 'string)

;;; --- Imenu ---
(set-default 'imenu-auto-rescan t)

;;; --- Shell scripts ---
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; --- Eshell savefile ---
(setq eshell-directory-name (expand-file-name "eshell" my-savefile-dir))

;;; --- So-long mode ---
(global-so-long-mode 1)

;;; --- Recursive minibuffers ---
(setq enable-recursive-minibuffers t)

;;; --- Version control ---
(setq vc-handled-backends '(Git))

;;; --- Zop-to-char ---
(use-package zop-to-char)

;;; --- Ace-window ---
(use-package ace-window)

(provide 'core-defaults)
;;; core-defaults.el ends here
