;;; mod-os.el --- OS-specific configuration -*- lexical-binding: t; -*-

;;; --- exec-path-from-shell (macOS) ---
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :config
  (exec-path-from-shell-initialize))

;;; --- OS-specific font heights and browser ---
(cond
 ((eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :height 150)
  (set-face-attribute 'mode-line nil :height 130)
  (set-face-attribute 'mode-line-inactive nil :height 130)
  (add-to-list 'exec-path "/home/dany/bin")
  (setq browse-url-browser-function 'browse-url-firefox))
 ((eq system-type 'darwin)
  (set-face-attribute 'default nil :height 180)
  (set-face-attribute 'mode-line nil :height 150)
  (set-face-attribute 'mode-line-inactive nil :height 150)
  (setq browse-url-browser-function 'browse-url-generic)
  (setq browse-url-generic-program "open")))

;;; --- TRAMP ---
(setq tramp-default-method "ssh")
(setq tramp-auto-save-directory "~/tmp/tramp/")
(setq tramp-verbose 1)
(setq tramp-chunksize 2000)
(setq tramp-ssh-controlmaster-options
      (concat
       "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
       "-o ControlMaster=auto -o ControlPersist=yes"))
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
(with-eval-after-load 'tramp
  (tramp-set-completion-function "ssh"
                                 '((tramp-parse-sconfig "~/.ssh/config"))))
;; Enable TRAMP
(setq tramp-mode t)

;;; --- Open remote dired files locally ---
(defun dired-open-file-locally ()
  "Copy remote file to tmp and open it with the default local program."
  (interactive)
  (let* ((file (dired-get-file-for-visit))
         (local-file (if (file-remote-p file)
                         (let ((tmp (make-temp-file "tramp-" nil
                                      (concat "." (file-name-extension file)))))
                           (copy-file file tmp t)
                           tmp)
                       file)))
    (call-process "open" nil 0 nil local-file)))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c o") #'dired-open-file-locally))

;;; --- Input decode map for terminal super/ctrl keys ---
(define-key input-decode-map "\e[s-s" [s-s])
(define-key input-decode-map "\e[C-'" (kbd "C-'"))
(define-key input-decode-map "\e[s-e" [s-e])
(define-key input-decode-map "\e[s-r" [s-r])
(define-key input-decode-map "\e[s-n" [s-n])
(define-key input-decode-map "\e[s-p" [s-p])
(define-key input-decode-map "\e[s-b" [s-b])
(define-key input-decode-map "\e[s-u" [s-u])

;;; --- TeX path for macOS ---
(when (eq system-type 'darwin)
  (setenv "PATH"
          (concat "/Library/TeX/texbin/" ":"
                  (getenv "PATH"))))

(provide 'mod-os)
;;; mod-os.el ends here
