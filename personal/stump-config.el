; (add-to-list 'load-path "~/slime/")  ; your SLIME directory
;; In my case /path/to/quicklisp is ~/quicklisp
;;(defvar quicklisp-path "~/quicklisp")
;;
;; Load slime-helper, this sets up various autoloads:
;;
;; (load (concat quicklisp-path "/slime-helper"))
;; ;;
;; ;; Set up slime with whatever modules you decide to use:
;; ;;
;; (slime-setup '(slime-fancy slime-mrepl slime-banner slime-tramp
;;                            slime-xref-browser slime-highlight-edits
;;                            slime-sprof))
;;
;; Decide where to put a slime scratch file, if you want one, and set
;; it up with:
;;
                                        ;(setf slime-scratch-file "./.slime-scratch.lisp")
;;
;; Unfortunately there is no hook for the slime-scratch mode so if you
;; want to automatically enable/disable various minor modes in the
;; slime scratch buffer you must do something like:
;;
;; (defadvice slime-scratch
;;     (after slime-scratch-adjust-modes () activate compile)
;;   (turn-some-mode-off)
;;   (turn-some-other-mode-on))

;; (setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
;; (require 'slime)
;; (slime-setup)

(defun stump-init ()
  (interactive)
  (slime-connect "localhost" 4005)
  (slime-repl-send-string "(in-package :stumpwm)"))

(defun helm-kill-ring-frame ()
  "This is meant to be called with emacsclient -c -n -e '(helm-kill-ring-frame)'"
  (delete-other-windows)
  (let ((helm-full-frame t)
        (frame (window-frame)))
    (helm-show-kill-ring)
    ;; if the Helm session was cancelled without copying an entry, we need to
    ;; delete the frame now.
    (ignore-errors
      (delete-frame frame))))

(defun git-dir-name (git-path)
  (with-temp-buffer
    (insert git-path)
    (re-search-backward "/\\([^/]+\\).git/\?$")
    (match-string 1)))

(defun open-git-project (clone-path git-path)
  (let ((dir-name (git-dir-name git-path)))
    (helm-escreen-create-screen dir-name)
    (find-file (string-join clone-path dir-name))))
