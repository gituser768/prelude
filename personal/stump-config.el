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
    (find-file (concat clone-path dir-name))))
