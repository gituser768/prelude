(defun latex-comment (start end)
  (interactive "r")
  (goto-char end)
  (forward-line -1)
  (while (> (point) start)
    (end-of-line)
    (insert "%")
    (forward-line -1))
  (end-of-line)
  (insert "%"))

(use-package latex
  :bind (:map LaTeX-mode-map
              ("C-M-;" . latex-comment)))

(provide 'latex-config)
