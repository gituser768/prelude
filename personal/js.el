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
            (subword-mode)))

;; (defun comint-lodash-filter (input)
;;   (message (replace-regexp-in-string "_\s*=\s*require\s*'lodash'" "lodash = require 'lodash'" input))
;;   (thread-last input
;;     (replace-regexp-in-string "_\s*=\s*require\s*'lodash'" "lodash = require 'lodash'")
;;     (replace-regexp-in-string "_\\." "lodash.")
;;     (replace-regexp-in-string "\\(lodash\.partial.+\\)_" "\\1lodash")
;;     (replace-regexp-in-string "^_$" "lodash")))
;; (defun comint-lodash-filter (input) input)


;; (defadvice coffee-send-region (around my-coffee-send-region activate)
;;   (let (orig-comint-simple-send)
;;     (fset 'orig-comint-simple-send (symbol-function 'comint-simple-send))
;;     (fset 'comint-simple-send (lambda (proc input) (orig-comint-simple-send proc (comint-lodash-filter input))))
;;     (unwind-protect
;;         ad-do-it
;;       (fset 'comint-simple-send (symbol-function 'orig-comint-simple-send)))))

(defun coffee-repl-advice ()
  (subword-mode)
  (add-hook 'comint-preoutput-filter-functions
            (lambda (output) (truncate-string-to-width output 1000000))
            nil
            t))

(advice-add 'coffee-repl :after 'coffee-repl-advice)

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
