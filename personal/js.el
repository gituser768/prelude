(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

(flycheck-add-mode 'javascript-eslint 'js2-mode)

(add-hook 'typescript-mode-hook
          (lambda ()
            ;;(tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)
            (company-mode-on)
            (subword-mode)))

(add-hook 'js2-mode-hook
          (lambda ()
            (subword-mode)
            (js2-mode-hide-warnings-and-errors)))

(require 'coffee-mode)

(setf coffee-command
      (cond
       ((string= system-type "gnu/linux") "/home/dany/coffeescript/bin/coffee")
       ((string= system-type "darwin") "/Users/dany/better-coffeescript-repl/bin/coffee")))

(setq coffee-lambda-regexp "\\(?:coroutine\\s-*\\)?\\(?:([^)]*)\\)?\\s-*\\(->\\|=>\\)")

(setq coffee-imenu-index-regexp
      (concat "^\\(\\s-*\\)" ; $1
              "\\(?:"
              coffee-assign-regexp ; $2
              "\\s-*"
              coffee-lambda-regexp
              "\\|"
              coffee-namespace-regexp ; $4
              "\\|"
              "\\(@?[_[:word:]:.$]+\\)\\s-*=\\(?:[^>]\\|$\\)" ; $5 match prototype access too
              "\\(?:" "\\s-*" "\\(" coffee-lambda-regexp "\\)" "\\)?" ; $6
              "\\)"))


(provide 'js)
