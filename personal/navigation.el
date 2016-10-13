(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(defun mark-to-end-of-paragraph (&optional arg)
  (interactive "p")
  (call-interactively 'set-mark-command)
  (call-interactively 'mark-paragraph))

(defun kill-to-end-of-sexp (&optional arg)
  (interactive "p")
  (call-interactively 'set-mark-command)
  (call-interactively 'sp-up-sexp)
  (call-interactively 'backward-char)
  (call-interactively 'kill-region))

(defun kill-to-beginning-of-sexp (&optional arg)
  (interactive "p")
  (call-interactively 'set-mark-command)
  (call-interactively 'sp-backward-up-sexp)
  (call-interactively 'forward-char)
  (call-interactively 'kill-region))

(defun my-set-mark (arg)
  (interactive "p")
  (if (> arg 0)
      (if (region-active-p)
          (deactivate-mark)
          (call-interactively 'set-mark-command))
    (unpop-to-mark-command)))

(require 'avy)
(advice-add 'avy-goto-word-1 :before (lambda (&rest r)
                                       (interactive (list (read-char "char: " t)
                                                          (point)))
                                       (set-mark (nth 2 r))
                                       (set-mark (nth 2 r))))

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(setq filter-buffer-substring-function
      (lambda (beg end delete)
        (save-excursion
          (let ((killed-string (chomp (buffer-substring beg end))))
            (when delete (delete-region beg end))
            killed-string))))



(setq projectile-test-suffix-function
      (lambda (project-type)
        (cond
         ((member project-type '(rebar)) "_SUITE")
         ((member project-type '(emacs-cask)) "-test")
         ((member project-type '(rails-rspec ruby-rspec)) "_spec")
         ((member project-type '(rails-test ruby-test lein-test boot-clj go elixir)) "_test")
         ((member project-type '(scons)) "test")
         ((member project-type '(maven symfony)) "Test")
         ((member project-type '(gradle gradlew grails)) "Spec")
         ((member project-type '(sbt)) "Spec")
         ((member project-type '(clojure)) "-test")
         ((member project-type '(generic)) "_test"))))

(provide 'navigation)
