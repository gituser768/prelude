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

(require 'which-key)
(require 'modalka)
(require 'test-switcher)
(require 'god-mode)

(require 'harp-mode)

(which-key-mode)

(setq jiralib-url "https://dmgxteam.atlassian.net")

(setq enable-recursive-minibuffers t)

(rainbow-delimiters-mode)

;;(face tabs empty trailing lines-tail)
(setq whitespace-style
      '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab))

(setq thesaurus-bhl-api-key "a6f1f7cbef19ca997bc341270494fad2")

(setq hippie-expand-verbose t)

(setq cljr-warn-on-eval nil)

(setq rcirc-fill-column 'window-text-width)
(setq rcirc-default-nick "monoda")

(define-minor-mode code-review-mode
  (if code-review-mode
      (progn
        (set-face-attribute 'region nil :background "#2B2B2B")
        (text-scale-decrease 2))
    (progn
      (text-scale-increase 2)
      (set-face-attribute 'region nil :background "firebrick"))))

(defun test-file-hook ()
  (when (or (string-match-p "test" buffer-file-name)
            (string-match-p "spec" buffer-file-name))
    (tertestrial-mode)))
(add-hook 'find-file-hook 'test-file-hook)
(add-hook 'file-file-hook 'crux-reopen-as-root)
(add-hook 'prelude-prog-mode-hook 'paredit-everywhere-mode t)
(add-hook 'prelude-prog-mode-hook 'turn-off-smartparens-mode t)

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

(cond
 ((string= system-type "gnu/linux") (set-face-attribute 'default nil :height 150))
 ((string= system-type "darwin") (set-face-attribute 'default nil :height 180)))

(setq company-tooltip-flip-when-above nil)

(advice-add 'save-buffer :before 'whitespace-cleanup)
(advice-add 'delete-window :before
            (lambda (&optional window)
              (when (buffer-file-name) (save-buffer))))
(advice-add 'other-frame :before
            (lambda (&optional window)
              (when (buffer-file-name) (save-buffer))))

(add-hook 'comint-mode-hook 'turn-off-show-smartparens-mode)

(beacon-mode -1)

(setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order)))
(setq ivy-initial-inputs-alist '())

(setq prelude-flyspell nil)

(add-hook 'after-make-frame-functions
          '(lambda (frame)
             (modify-frame-parameters frame
                                      '((vertical-scroll-bars . nil)
                                        (horizontal-scroll-bars . nil)))))

(toggle-scroll-bar -1)

(setf expand-region-fast-keys-enabled nil)

;; (load-theme 'solarized-dark)
;; (load-theme 'gruvbox)
(load-theme 'spacemacs-dark)
(set-face-attribute 'default nil :font "Source Code Pro")

(size-indication-mode -1)
(set-face-attribute 'mode-line nil  :height 150)
(set-face-attribute 'mode-line-inactive nil  :height 150)

(setq-default mode-line-format '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification sml/pos-id-separator mode-line-position evil-mode-line-tag smartrep-mode-line-string sml/pre-modes-separator mode-line-modes mode-line-misc-info (:eval (magit-get-current-branch)) mode-line-end-spaces))

(condition-case nil
    (server-start)
  (error (server-running-p)))

(provide 'my-init)
