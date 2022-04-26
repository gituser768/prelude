(cond
 ((string= system-type "gnu/linux")
  (progn
    (add-to-list 'exec-path "/home/dany/bin")
    (set-face-attribute 'mode-line nil  :height 130)
    (set-face-attribute 'mode-line-inactive nil  :height 130)))
 ((string= system-type "darwin")
  (progn
    (exec-path-from-shell-initialize)
    (set-face-attribute 'mode-line nil  :height 150)
    (set-face-attribute 'mode-line-inactive nil  :height 150))))

(provide 'os-config)
