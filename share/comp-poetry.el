(require 'cl-lib)
(require 'poetry)
(require 'company)

(defun company-poetry-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-poetry-backend))
    (prefix (and (and (boundp 'poetry-mode) poetry-mode)
                 (company-grab-word)))
    (candidates
     (setq rhymes (save-excursion
                    (call-interactively 'previous-line)
                    (call-interactively 'move-end-of-line)
                    (poetry-find-rhyme-val (company-grab-word))))
     (mapcar 'car (s-match-strings-all "[A-za-z]+" rhymes)))
    (meta (format "This value is named %s" arg))))

(add-to-list 'company-backends 'company-poetry-backend)
