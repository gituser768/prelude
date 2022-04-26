(require 'octave)

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(define-key octave-mode-map (kbd "C-c C-z") 'inferior-octave)
(define-key octave-mode-map (kbd "C-c C-l") 'octave-send-line)
(define-key octave-mode-map (kbd "C-c C-b") 'octave-send-buffer)
