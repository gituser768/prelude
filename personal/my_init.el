(add-to-list 'load-path "~/.emacs.d/personal")
(load "helpers.el")
(load "js.el")
(load "keybindings.el")
(load "keychords.el")
(load "gnus-config.el")
(load "latex-config.el")
(load "magit-config.el")
(load "navigation.el")
(load "org-config.el")
(load "term-config.el")
(load "web-config.el")
(load "windows.el")
(load "yas-conf.el")
(load "stump-config.el")
(load "os-config.el")
(load "cl.el")

(require 'use-package)

(use-package which-key
  :config (which-key-mode))

(use-package rainbow-delimiters
  :config (rainbow-delimiters-mode))

(use-package whitespace
  :config
  (setq whitespace-style
        '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab))
  (advice-add 'save-buffer :before 'whitespace-cleanup))

(use-package hippie-exp
  :config (setf hippie-expand-verbose t))

(use-package emacs-tertestrial
  :load-path "../vendor/emacs-tertestrial/"
  :config
  (add-hook 'find-file-hook 'test-file-hook))
(defun test-file-hook ()
  (when (or (string-match-p "test" buffer-file-name)
            (string-match-p "spec" buffer-file-name))
    (tertestrial-mode)))

(use-package crux
  :config
  (add-hook 'file-file-hook 'crux-reopen-as-root))
(defun better-kill-line (&optional arg)
  (interactive "p")
  (if mark-active
      (kill-region (region-beginning) (region-end))
    (crux-kill-whole-line arg)))

(use-package projectile
  :config
  (setf projectile-mode-line
        '(:eval (if (file-remote-p default-directory)
                    " Projectile"
                  (format " %s" (projectile-project-name))))))

(use-package cider
  :config
  (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))"))

(use-package flycheck
  :config
  (setq flycheck-mode-line '(:eval " FC"))
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'js2-mode))

(use-package diff-hl-flydiff
  :config (diff-hl-flydiff-mode))

(use-package company
  :bind (:map company-active-map
              ("<escape>" . company-abort)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("<tab>" . company-complete-common-or-cycle))
  :config (setf company-tooltip-flip-when-above nil))

(use-package smartparens
  :init (use-package comint)
  :config (add-hook 'comint-mode-hook 'turn-off-show-smartparens-mode))

(use-package ivy
  :config
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))
  (setq ivy-initial-inputs-alist '()))

(use-package expand-region
  :config (setf expand-region-fast-keys-enabled nil))

;; ;;; Prelude undo configs
;; (remove-hook 'after-save-hook
;;              (lambda ()
;;                (when (and
;;                       (string-prefix-p prelude-dir (file-truename buffer-file-name))
;;                       (file-exists-p (byte-compile-dest-file buffer-file-name)))
;;                  (emacs-lisp-byte-compile)))
;;              t)

;; (beacon-mode -1)

;; (setq prelude-flyspell nil)


;;; Base Emacs config
(global-hl-line-mode)
(make-variable-buffer-local 'global-hl-line-mode)

(setq enable-recursive-minibuffers t)

(advice-add 'delete-window :before
            (lambda (&optional window)
              (when (buffer-file-name) (save-buffer))))
(advice-add 'other-frame :before
            (lambda (&optional window)
              (when (buffer-file-name) (save-buffer))))
(toggle-scroll-bar -1)
(add-hook 'after-make-frame-functions
          '(lambda (frame)
             (modify-frame-parameters frame
                                      '((vertical-scroll-bars . nil)
                                        (horizontal-scroll-bars . nil)))))


;;; Visual
(load-theme 'spacemacs-dark)
(set-face-attribute 'default nil :font "Source Code Pro")

(size-indication-mode -1)

(setq-default mode-line-format '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification sml/pos-id-separator mode-line-position evil-mode-line-tag smartrep-mode-line-string sml/pre-modes-separator mode-line-modes mode-line-misc-info (:eval (magit-get-current-branch)) mode-line-end-spaces))

(condition-case nil
    (server-start)
  (error (server-running-p)))


(provide 'my-init)
