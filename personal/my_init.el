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
(load "latex-config.el")

(require 'which-key)
(require 'modalka)

(which-key-mode)

(setq jiralib-url "https://dmgxteam.atlassian.net")

(rainbow-delimiters-mode)

(setq thesaurus-bhl-api-key "a6f1f7cbef19ca997bc341270494fad2")

(setq hippie-expand-verbose t)

(size-indication-mode -1)
(set-face-attribute 'mode-line nil  :height 150)
(set-face-attribute 'mode-line-inactive nil  :height 150)

(define-minor-mode code-review-mode
  (if code-review-mode
      (progn
        (set-face-attribute 'region nil :background "#2B2B2B")
        (text-scale-decrease 2))
    (progn
      (text-scale-increase 2)
      (set-face-attribute 'region nil :background "firebrick"))))

(defun test-file-hook ()
  (when (string-match-p "test" buffer-file-name)
    (tertestrial-mode)))
(add-hook 'find-file-hook 'test-file-hook)

;; (defun git-clone (repo-path)
;;   (interactive "sGit repo: ")
;;   (async-shell-command (concat "git clone " repo-path "/Users/dany/Documents/")))

(setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

(setq semanticdb-global-mode t)

(require 'projectile)
(setq projectile-mode-line
      '(:eval (if (file-remote-p default-directory)
                  " Projectile"
                (format " %s" (projectile-project-name)))))

(require 'flycheck)
(setq flycheck-mode-line '(:eval " FC"))

(diff-hl-flydiff-mode)

(add-to-list 'Info-default-directory-list "~/info")

(set-face-attribute 'default nil :height 200)

(setq company-tooltip-flip-when-above nil)

(advice-add 'save-buffer :before 'whitespace-cleanup)

(add-hook 'comint-mode-hook 'turn-off-show-smartparens-mode)

(beacon-mode -1)

(setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order)))
(setq ivy-initial-inputs-alist '())

(setq prelude-flyspell nil)

(toggle-scroll-bar -1)

(load-theme 'solarized-dark)

(server-start)

(provide 'my-init)
