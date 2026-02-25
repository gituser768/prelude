;;; init.el --- Standalone Emacs configuration -*- lexical-binding: t; -*-

;; Reset GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024)))) ; 16MB

;;; --- Directory layout ---
(defvar my-emacs-dir (file-name-directory load-file-name))
(defvar my-lisp-dir (expand-file-name "lisp" my-emacs-dir))
(defvar my-vendor-dir (expand-file-name "vendor" my-emacs-dir))
(defvar my-savefile-dir (expand-file-name "savefile" my-emacs-dir))

(unless (file-exists-p my-savefile-dir)
  (make-directory my-savefile-dir t))

;; Add lisp/ and vendor/ (including subdirs) to load-path
(add-to-list 'load-path my-lisp-dir)
(add-to-list 'load-path my-vendor-dir)
(dolist (dir (directory-files my-vendor-dir t "\\`[^.]"))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))

;;; --- Package bootstrap ---
(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;; --- Custom file ---
(setq custom-file (expand-file-name "custom.el" my-emacs-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;;; --- Fundamentals ---
(setq default-directory (concat (getenv "HOME") "/"))
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

;;; --- Load env secrets ---
(defun load-env-vars-from-file (file)
  "Load environment variables from a FILE with 'export' syntax."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^\\([^=]+\\)=\\(.*\\);?$" nil t)
        (let ((var (match-string 1))
              (value (match-string 2)))
          (setenv var value))))))

(defun load-dot-env ()
  "Load .env file from `default-directory'."
  (interactive)
  (let ((env-file (expand-file-name ".env" default-directory)))
    (when (file-exists-p env-file)
      (with-temp-buffer
        (insert-file-contents env-file)
        (goto-char (point-min))
        (while (re-search-forward "^\\([^=]+\\)=\\(.*\\)$" nil t)
          (setenv (match-string 1) (match-string 2)))))))

(let ((secrets-file (expand-file-name "secrets" my-emacs-dir)))
  (when (file-exists-p secrets-file)
    (load-env-vars-from-file secrets-file)))
(load-dot-env)

;;; --- Load modules in order ---
(load "core-defaults")
(load "core-ui")
(load "core-helpers")
(load "core-completion")
(load "core-keybindings")
(load "core-keychords")
(load "core-windows")
(load "mod-dev")
(load "mod-python")
(load "mod-term")
(load "mod-os")
(load "mod-latex")

;;; --- Start server ---
(condition-case nil
    (server-start)
  (error (server-running-p)))

;;; init.el ends here
