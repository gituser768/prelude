(require 'magit)
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
					  ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)


;; (require 'nov)

;; (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(setq default-directory (concat (getenv "HOME") "/"))
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(require 'projectile)
(setq projectile-mode-line "Projectile")
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
(setq tramp-verbose 1)
(setq vc-handled-backends '(Git))
(require 'diff-hl)
(diff-hl-margin-mode)

(global-so-long-mode 1)

(defun load-env-vars-from-file (file)
  "Load environment variables from a FILE with 'export' syntax."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (while (re-search-forward "^\\([^=]+\\)=\\(.*\\);?$" nil t)
      (let ((var (match-string 1))
            (value (match-string 2)))
        (setenv var value)))))

;; Usage:
(load-env-vars-from-file "~/.emacs.d/personal/secrets")

(setq my-org-path "~/org/")

(require 'use-package)
(require 'quelpa)
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el")))

(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(sml/setup)
(line-number-mode 1)

;; (require 'dash)
;; (setq prelude-packages
;;       (-remove-item 'undo-tree prelude-packages))

(add-to-list 'load-path "~/.emacs.d/personal")
(load "helpers.el")
(load "keybindings.el")
(load "keychords.el")
(load "magit-config.el")
(load "navigation.el")
(load "term-config.el")
(load "yas-conf.el")
(load "org-config.el")
(load "os-config.el")
(load "octave-config.el")
(load "cuda-config.el")
(load "python-config.el")
(load "lang-config.el")

(pdf-tools-install)

(require 'which-key)
;; (require 'test-switcher)
(require 'god-mode)
;; (require 'harp-mode)
;; (require 'emacs-tertestrial)

(require 'vlf-setup)
(require 'midnight)
(setf midnight-hook '())

(require 'centered-window)
(centered-window-mode)

;;(which-key-mode)
(setq enable-recursive-minibuffers t)
(rainbow-delimiters-mode)
(setq whitespace-style
      '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab))
(setq hippie-expand-verbose t)

(remove-hook 'after-save-hook
             (lambda ()
               (when (and
                      (string-prefix-p prelude-dir (file-truename buffer-file-name))
                      (file-exists-p (byte-compile-dest-file buffer-file-name)))
                 (emacs-lisp-byte-compile)))
             t)

(mapc 'diminish '(editorconfig-mode helm-mode company-mode cider-mode projectile-mode))

(setf yas/snippet-dirs '("~/.emacs.d/snippets"))

(defun test-file-hook ()
  (when (or (string-match-p "test" buffer-file-name)
            (string-match-p "spec" buffer-file-name))
    (tertestrial-mode)))
;; (add-hook 'find-file-hook 'test-file-hook)
(add-hook 'file-file-hook 'crux-reopen-as-root)
;;(add-hook 'prelude-prog-mode-hook 'paredit-everywhere-mode t)

(require 'ansi-color)
(defun dh-display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun dh-get-weather ()
  (interactive)
  (with-temp-buffer
    (shell-command "curl wttr.in" "*weather*")
    (switch-to-buffer "*weather*")
    (mark-whole-buffer)
    (dh-display-ansi-colors)
    (deactivate-mark)
    (read-only-mode)
    (delete-other-windows)
    (beginning-of-buffer)))

(setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

(require 'projectile)
(setq projectile-mode-line
      '(:eval (if (file-remote-p default-directory)
                  " Projectile"
                (format " %s" (projectile-project-name)))))

(require 'flycheck)
(setq flycheck-mode-line '(:eval " FC"))

(diff-hl-flydiff-mode)

(cond
 ((string= system-type "gnu/linux") (set-face-attribute 'default nil :height 150))
 ((string= system-type "darwin") (set-face-attribute 'default nil :height 180)))

(setq company-tooltip-flip-when-above nil)

(setq prelude-whitespace nil)
;; (setf prelude-clean-whitespace-on-save nil)
(require 'ws-butler)
(setf ws-butler-keep-whitespace-before-point t)
(ws-butler-global-mode)
(add-to-list 'ws-butler-global-exempt-modes 'org-mode)

(advice-add 'delete-window :before
            (lambda (&optional window) (super-save-command-advice)))
(advice-add 'select-window :before
            (lambda (window &optional norecord) (super-save-command-advice)))
(advice-add 'other-frame :before
            (lambda (arg) (super-save-command-advice)))

(cond
 ((string= system-type "gnu/linux")
  (progn
    (setf browse-url-browser-function 'browse-url-firefox)))
 ((string= system-type "darwin")
  (progn
    (setq browse-url-browser-function (quote browse-url-generic))
    (setq browse-url-generic-program "open"))))


(setq tramp-default-method "ssh")
(setq tramp-auto-save-directory "~/tmp/tramp/")
(tramp-set-completion-function "ssh"
                               '((tramp-parse-sconfig "~/.ssh/config")))
(setq tramp-chunksize 2000)
(setq tramp-ssh-controlmaster-options
      (concat
       "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
       "-o ControlMaster=auto -o ControlPersist=yes"))

(menu-bar-mode 1)

(defvar s3-bucket)
(defun s3cmd-put (filepath)
  ;; (start-process "googlecloadput" nil "~/Downloads/google-cloud-sdk/bin/gsutil" "rsync" "/home/dany/org" "gs://dmh-org/")
  )

(require 'dash)

(advice-add 'save-buffer :after
            (lambda (&optional arg)
              (when (and (boundp 's3-bucket)
                         (buffer-file-name))
                (s3cmd-put (buffer-file-name)))
              (when (and (buffer-file-name)
                         (cl-search "gtd.org" (buffer-file-name)))
                (progn
                  (org-html-export-to-html)
                  (setf kill-ring (rest kill-ring))
                  (s3cmd-put (replace-regexp-in-string "gtd.org" "gtd.html" (buffer-file-name)))))))

(add-hook 'comint-mode-hook 'turn-off-show-smartparens-mode)

(setf sp-highlight-pair-overlay nil)

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

;;(toggle-scroll-bar -1)
(which-function-mode -1)

(setq prelude-guru nil)

(setf expand-region-fast-keys-enabled nil)

(load-theme 'spacemacs-dark)
(set-face-attribute 'default nil :font "Source Code Pro")

(size-indication-mode -1)

(setq-default mode-line-format '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification sml/pos-id-separator mode-line-position evil-mode-line-tag smartrep-mode-line-string sml/pre-modes-separator mode-line-modes mode-line-misc-info (:eval (magit-get-current-branch)) mode-line-end-spaces))

(condition-case nil
    (server-start)
  (error (server-running-p)))

;; install straight:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
            (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun load-dot-env ()
  (interactive)
  (let ((env-file (expand-file-name ".env" default-directory)))
    (when (file-exists-p env-file)
      (with-temp-buffer
        (insert-file-contents env-file)
        (goto-char (point-min))
        (while (re-search-forward "^\\([^=]+\\)=\\(.*\\)$" nil t)
          (setenv (match-string 1) (match-string 2)))))))

(load-dot-env)

(define-key input-decode-map "\e[s-s" [s-s])
(define-key input-decode-map "\e[C-'" (kbd "C-'"))
(define-key input-decode-map "\e[s-e" [s-e])
(define-key input-decode-map "\e[s-r" [s-r])
(define-key input-decode-map "\e[s-n" [s-n])
(define-key input-decode-map "\e[s-p" [s-p])
(define-key input-decode-map "\e[s-b" [s-b])
(define-key input-decode-map "\e[s-u" [s-u])

(global-eldoc-mode -1)
(setq eldoc-echo-area-use-multiline-p nil)

(use-package aider
  :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :config
  ;; Use claude-3-5-sonnet cause it is best in aider benchmark
  ;; (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
  (setq aider-args '("--model" "gpt-4.1" "--no-auto-commits"))
  (setenv "OPENAI_API_KEY" (getenv "OPENAI_API_KEY"))
  ;; Or use chatgpt model since it is most well known
  ;; (setq aider-args '("--model" "o3-mini"))
  ;; (setenv "OPENAI_API_KEY" <your-openai-api-key>)
  ;; Or use gemini v2 model since it is very good and free
  ;; (setq aider-args '("--model" "gemini/gemini-exp-1206"))
  ;; (setenv "GEMINI_API_KEY" <your-gemini-api-key>)
  ;; Or use your personal config file
  ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
  ;; ;;
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

(require 'prelude-helm-everywhere)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(provide 'my-init)
