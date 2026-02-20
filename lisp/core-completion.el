;;; core-completion.el --- Helm ecosystem -*- lexical-binding: t; -*-

;;; --- Helm ---
(use-package helm
  :config
  (helm-mode 1)
  (setq helm-split-window-in-side-p t
        helm-buffers-fuzzy-matching t
        helm-M-x-show-short-doc nil)

  ;; Global bindings (from prelude-helm-everywhere)
  :bind (("M-x"     . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("M-y"     . helm-show-kill-ring)
         ("C-x b"   . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-h f"   . helm-apropos)
         ("C-h r"   . helm-info-emacs)
         ("C-h C-l" . helm-locate-library)
         ("M-:"     . helm-eval-expression-with-eldoc)
         :map helm-map
         ("C-j"     . helm-previous-line)
         ("TAB"     . helm-execute-persistent-action)
         ("C-<tab>" . helm-select-action)))

;; Minibuffer/isearch/shell helm bindings
(with-eval-after-load 'helm
  (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
  (define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch))

(with-eval-after-load 'shell
  (define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring))

;;; --- Helm + eshell history ---
(add-hook 'eshell-mode-hook
          (lambda ()
            (require 'helm-eshell)
            (substitute-key-definition 'eshell-list-history 'helm-eshell-history eshell-mode-map)))

;;; --- Helm etags ---
(substitute-key-definition 'find-tag 'helm-etags-select global-map)

;;; --- Helm-projectile ---
(use-package helm-projectile
  :config
  (helm-projectile-on))

;;; --- Helm-descbinds ---
(use-package helm-descbinds
  :config
  (helm-descbinds-mode))

;;; --- TRAMP avoidance for helm-M-x ---
(with-eval-after-load 'helm
  (defun my/ensure-local-default-directory (orig-fun &rest args)
    (let ((default-directory (if (and (stringp default-directory)
                                      (file-remote-p default-directory))
                                 (expand-file-name "~")
                               default-directory)))
      (apply orig-fun args)))
  (advice-add 'helm-M-x :around #'my/ensure-local-default-directory))

(provide 'core-completion)
;;; core-completion.el ends here
