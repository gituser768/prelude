;;; mod-python.el --- Python configuration -*- lexical-binding: t; -*-

(require 'python)

(setq python-indent-offset 2)

;;; --- Conda ---
(use-package conda
  :config
  (setq-default conda-project-env-path "/home/dany.haddad/miniconda3/envs/")
  (setq-default conda-env-home-directory "/home/dany.haddad/miniconda3/")
  (conda-env-autoactivate-mode -1)
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

(setq-default mode-line-format (cons '(:exec conda-env-current-name) mode-line-format))
(setq-default flycheck-disabled-checkers '(python-flake8 python-pycompile python-mypy))

;;; --- Python keybindings ---
(define-key python-mode-map [remap backward-sentence] nil)
(define-key python-mode-map [remap forward-sentence] nil)
(define-key python-mode-map (kbd "C-M-<backspace>") 'sp-backward-kill-sexp)
(define-key python-mode-map (kbd "C-M-k") 'sp-kill-sexp)

;;; --- Anaconda mode ---
(use-package anaconda-mode :defer t)
(use-package company-anaconda :defer t)

(defun my-python-mode-defaults ()
  (subword-mode +1)
  (anaconda-mode 1)
  (eldoc-mode 1)
  (when (fboundp #'python-imenu-create-flat-index)
    (setq-local imenu-create-index-function
                #'python-imenu-create-flat-index))
  (add-hook 'post-self-insert-hook
            #'electric-layout-post-self-insert-function nil 'local))

(setq my-python-mode-hook 'my-python-mode-defaults)
(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'my-python-mode-hook)))

(setq anaconda-mode-localhost-address "127.0.0.1")

(provide 'mod-python)
;;; mod-python.el ends here
