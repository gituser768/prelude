(use-package gnus
  :config
  (setq user-mail-address "danyhaddad43@gmail.com"
        user-full-name "Dany Haddad"
        gnus-select-method '(nnml "")
        gnus-secondary-select-methods '((nntp "news.gmane.org")
                                        (nntp "news.eternal-september.org"))))
