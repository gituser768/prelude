(require 'org)
(require 'ox-latex)

(setq org-latex-pdf-process (list "latexmk -pdf -bibtex %f"))
(setq org-latex-pdf-process '("%latex -interaction nonstopmode -output-directory %o %f" "%latex -interaction nonstopmode -output-directory %o %f" "%latex -interaction nonstopmode -output-directory %o %f"))
(setf diary-file (concat my-org-path "diary"))

(setf org-agenda-include-diary t)

(require 'appt)
(setf appt-time-msg-list nil)
(setf appt-display-interval '2)
(setf appt-message-warning-time '10  ;; send first warning 10 minutes before appointment
      appt-display-format 'window)   ;; pass warnings to the designated window function
(appt-activate 1)

(org-agenda-to-appt)
(run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view

;; set up the call to terminal-notifier
(defvar my-notifier-path "notify-send")
(defun my-appt-send-notification (title msg)
  (cond
   ((string= system-type "gnu/linux")
    (shell-command
     (concat my-notifier-path " -u normal " msg " " title)))
   ((string= system-type "darwin")
    (progn
      (message (concat "osascript -e 'display notification \""
                       (substring msg 1 -1)
                       "\" with title \"Org mode "
                       (substring title 1 -1) "\"'" ))
      (shell-command
       (concat "osascript -e 'display notification \""
               (substring msg 1 -1)
               "\" with title \"Org mode "
               (substring title 1 -1) "\"'" ))))))

(defun my-appt-display (min-to-app new-time msg)
  (if (listp msg)
      (cl-mapc (lambda (min-to-app new-time msg)
                 (my-appt-send-notification
                  (format "'Appointment in %s minutes'" (substring-no-properties min-to-app))
                  (format "'%s'" (substring-no-properties msg))))
               min-to-app
               new-time
               msg)
    (my-appt-send-notification
     (format "'Appointment in %s minutes'" min-to-app)
     (format "'%s'" (substring-no-properties msg)))))

(setq appt-disp-window-function (function my-appt-display))
(set-face-foreground 'org-hide "DimGray")
(set-face-background 'org-scheduled-previously "#3F3F3F")

(setf org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setf org-clock-into-drawer 3)

(setq org-capture-templates
      `(("t" "Todo" entry (file+headline ,(concat my-org-path "gtd.org") "unfiled")
         "* TODO %?\n %i\n")
        ("a" "Ask about" entry (file+headline ,(concat my-org-path "gtd.org") "Ask about")
         "* %?\n  %i\n")
        ("r" "Review" entry (file+headline ,(concat my-org-path "gtd.org") "Review")
         "* TODO %?\n  %i\n")
        ("c" "Clocked Todo" entry (file+headline ,(concat my-org-path "gtd.org") "unfiled")
         "* TODO %?\n  %i\n" :clock-in t)
        ("m" "Misc" entry (file+headline ,(concat my-org-path "gtd.org") "Misc")
         "* TODO %?\n  %i\n")
        ("j" "Journal" entry (file+datetree ,(concat my-org-path "journal.org"))
         "* %?\nEntered on %U\n  %i\n")
        ("l" "Look Here" entry (file+headline ,(concat my-org-path "gtd.org") "Look Here")
         "* TODO %?\n %i\n %a\n")
        ;; ("r" "Remember this" entry (file+datetree (concat my-org-path "remember.org"))
        ;;  "* %?\nEntered on %U\n  %i\n  %a\nSCHEDULED: %^t")
        ("s" "Appointment" entry
         (file+headline ,(concat my-org-path "gtd.org") "Appointments")
         "* %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t) t)")
        ))

(org-babel-do-load-languages 'org-babel-load-languages '((awk . t)
                                                         (C . t)
                                                         (R . t)
                                                         (calc . t)
                                                         (clojure . t)
                                                         (css . t)
                                                         (dot . t)
                                                         (emacs-lisp . t)
                                                         (fortran . t)
                                                         (gnuplot . t)
                                                         (haskell . t)
                                                         (java . t)
                                                         (js . t)
                                                         (latex . t)
                                                         (ledger . t)
                                                         (lisp . t)
                                                         (makefile . t)
                                                         (maxima . t)
                                                         (matlab . t)
                                                         (mscgen . t)
                                                         (ocaml . t)
                                                         (octave . t)
                                                         (org . t)
                                                         (perl . t)
                                                         (picolisp . t)
                                                         (python . t)
                                                         (ruby . t)
                                                         (sass . t)
                                                         (scheme . t)
                                                         (screen . t)
                                                         (sql . t)
                                                         (sqlite . t)))

(setq org-agenda-files (list my-org-path))
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))))
(eval-after-load "org"
  (lambda ()
    (org-defkey org-mode-map (kbd "s-<tab>") 'outline-hide-subtree)
    (org-defkey org-mode-map (kbd "RET") 'org-meta-return)
    (org-defkey org-mode-map (kbd "C-M-u") 'org-up-element)
    (org-defkey org-mode-map (kbd "C-M-d") 'org-down-element)
    (org-defkey org-mode-map (kbd "C-M-n") 'org-next-visible-heading)
    (org-defkey org-mode-map (kbd "C-M-p") 'org-previous-visible-heading)
    (org-defkey org-mode-map (kbd "C-M-f") 'org-forward-element)
    (org-defkey org-mode-map (kbd "C-M-b") 'org-backward-element)
    (org-defkey org-mode-map (kbd "s-j") 'windmove-down)
    (org-defkey org-mode-map (kbd "s-h") 'windmove-left)
    (org-defkey org-mode-map (kbd "s-l") 'windmove-right)
    (org-defkey org-mode-map (kbd "s-k") 'windmove-up)
    (org-defkey org-mode-map (kbd "s-x f") 'org-metaright)
    (org-defkey org-mode-map (kbd "s-x b") 'org-metaleft)
    (org-defkey org-mode-map (kbd "s-x p") 'org-metaup)
    (org-defkey org-mode-map (kbd "s-x n") 'org-metadown)
    (org-defkey org-mode-map (kbd "s-x F") 'org-shiftmetaright)
    (org-defkey org-mode-map (kbd "s-x B") 'org-shiftmetaleft)
    (org-defkey org-mode-map (kbd "s-x P") 'org-shiftmetaup)
    (org-defkey org-mode-map (kbd "s-x N") 'org-shiftmetadown)))

(setq org-src-fontify-natively t)

(setenv "PATH"
        (concat
         "/Library/TeX/texbin/" ":"
         (getenv "PATH")))

(define-key global-map "\C-cc" 'org-capture)

(add-hook 'org-mode-hook 'org-indent-mode)

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries (lambda ()
                     (org-archive-subtree)
                     (setq org-map-continue-from (outline-previous-heading)))
                   "/DONE" 'file))

(setq org-priority-faces '((?A . (:foreground "firebrick" :weight bold))
                           (?B . (:foreground "DarkOrange3"))
                           (?C . (:foreground "ForestGreen"))))

(provide 'org-config)
