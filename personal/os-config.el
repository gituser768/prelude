(cond
 ((string= system-type "gnu/linux") (add-to-list 'exec-path "/home/dany/bin"))
 ((string= system-type "darwin") nil))

(provide 'os-config)
