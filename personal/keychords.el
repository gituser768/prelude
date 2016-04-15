(require 'key-chord)

(key-chord-mode 1)
(key-chord-define-global "``" 'helm-mini)
(key-chord-define-global "`1" 'helm-imenu-in-all-buffers)
(key-chord-define-global "`2" 'helm-imenu)
(key-chord-define-global "zz" 'zop-up-to-char)
(key-chord-define-global "zb" (lambda (&optional arg)
                                (interactive "p")
                                (zop-up-to-char -1)))

(key-chord-define-global ",," 'repeat)
(key-chord-define-global ",f" 'fastnav-sprint-forward)
(key-chord-define-global ",b" 'fastnav-sprint-backward)
(key-chord-define-global ",x" 'helm-M-x)
(key-chord-define-global ",g" 'magit-status)
(key-chord-define-global ",c" 'magit-checkout)
(key-chord-define-global ",p" 'magit-push)

(key-chord-define-global "lj" nil)

(provide 'keychords)
;;; keychords ends here
