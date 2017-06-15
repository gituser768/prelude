(cond
 ((string= system-type "gnu/linux")
  (progn
    (add-to-list 'exec-path "/home/dany/bin")
    (set-face-attribute 'default nil :height 150)
    (set-face-attribute 'mode-line nil  :height 130)
    (set-face-attribute 'mode-line-inactive nil  :height 130)))
 ((string= system-type "darwin")
  (progn
    (set-face-attribute 'default nil :height 180)
    (set-face-attribute 'mode-line nil  :height 150)
    (set-face-attribute 'mode-line-inactive nil  :height 150))))


(provide 'os-config)
