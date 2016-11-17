;; (define-key magit-mode-map (kbd "o") (lambda (&optional arg)
;;                                        (interactive "d")
;;                                        (magit-visit-thing -1)))
;; (define-key magit-mode-map (kbd ",s") 'magit-stash-popup)

(global-auto-revert-mode -1)

(provide 'magit-config)
