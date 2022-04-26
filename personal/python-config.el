(require 'python)

(setf python-indent-offset 2)

;; (exec-path-from-shell-copy-env "ANACONDA_HOME")
(require 'conda)
(setq-default conda-project-env-path "/Users/dany.haddad/miniconda3/envs/")
(conda-env-autoactivate-mode -1)
(conda-env-initialize-interactive-shells)
(conda-env-initialize-eshell)
(custom-set-variables
 '(conda-anaconda-home "/Users/dany.haddad/miniconda3/"))

(setq-default mode-line-format (cons '(:exec conda-env-current-name) mode-line-format))

(setq-default flycheck-disabled-checkers '(python-flake8))

(define-key python-mode-map [remap backward-sentence] nil)
(define-key python-mode-map [remap forward-sentence] nil)
(define-key python-mode-map (kbd "C-M-<backspace>") 'sp-backward-kill-sexp)
(define-key python-mode-map (kbd "C-M-k") 'sp-kill-sexp)

(remove-hook 'python-mode-hook (car(last python-mode-hook)))

(defun my-python-mode-defaults ()
  (subword-mode +1)
  (anaconda-mode 1)
  (eldoc-mode 1)
  (when (fboundp #'python-imenu-create-flat-index)
    (setq-local imenu-create-index-function
                #'python-imenu-create-flat-index))
  (add-hook 'post-self-insert-hook
            #'electric-layout-post-self-insert-function nil 'local)
  (add-hook 'after-save-hook 'prelude-python-mode-set-encoding nil 'local))

(setq my-python-mode-hook 'my-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'my-python-mode-hook)))

(setq anaconda-mode-localhost-address "127.0.0.1")

(provide 'python-config)
