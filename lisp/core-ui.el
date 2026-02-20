;;; core-ui.el --- Theme, font, modeline, visual settings -*- lexical-binding: t; -*-

;;; --- Cursor & bell ---
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)

;;; --- Startup ---
(setq inhibit-startup-screen t)

;;; --- Scroll settings ---
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;;; --- Mode line indicators ---
(line-number-mode 1)
(column-number-mode t)
(size-indication-mode -1)

;;; --- y-or-n-p ---
(fset 'yes-or-no-p 'y-or-n-p)

;;; --- Frame title ---
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;; --- Theme ---
(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

;;; --- Font ---
(set-face-attribute 'default nil :font "Source Code Pro")

;;; --- Smart mode line ---
(use-package smart-mode-line
  :config
  (sml/setup))

;;; --- Custom mode-line-format (with magit branch) ---
(setq-default mode-line-format
              '("%e" mode-line-front-space mode-line-mule-info mode-line-client
                mode-line-modified mode-line-remote mode-line-frame-identification
                mode-line-buffer-identification sml/pos-id-separator
                mode-line-position evil-mode-line-tag smartrep-mode-line-string
                sml/pre-modes-separator mode-line-modes mode-line-misc-info
                (:eval (magit-get-current-branch)) mode-line-end-spaces))

;;; --- Centered window mode ---
(use-package centered-window
  :config
  (centered-window-mode))

;;; --- Rainbow delimiters ---
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; --- Which-key ---
(use-package which-key
  :defer t)

;;; --- VLF (view large files) ---
(use-package vlf
  :config
  (require 'vlf-setup))

;;; --- Midnight (clean up old buffers) ---
(require 'midnight)
(setf midnight-hook '())

;;; --- No scroll bars in new frames ---
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (modify-frame-parameters frame
                                     '((vertical-scroll-bars . nil)
                                       (horizontal-scroll-bars . nil)))))

;;; --- Disable which-function-mode ---
(which-function-mode -1)

;;; --- Eldoc ---
(global-eldoc-mode -1)
(setq eldoc-echo-area-use-multiline-p nil)

;;; --- Claude code repl face ---
(custom-set-faces
 '(claude-code-repl-face ((t (:family "JuliaMono")))))

(provide 'core-ui)
;;; core-ui.el ends here
