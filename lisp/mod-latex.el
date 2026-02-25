;;; mod-latex.el --- LaTeX editing configuration -*- lexical-binding: t; -*-

;;; --- AUCTeX ---
(use-package auctex
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-close-quote "")
  (setq TeX-open-quote "")
  (setq-default TeX-master nil)
  (setq TeX-PDF-mode t)

  ;; macOS: open PDFs/DVI/HTML with system viewer
  (when (eq system-type 'darwin)
    (setq TeX-view-program-selection
          '((output-dvi "DVI Viewer")
            (output-pdf "PDF Viewer")
            (output-html "HTML Viewer")))
    (setq TeX-view-program-list
          '(("DVI Viewer" "open %o")
            ("PDF Viewer" "open %o")
            ("HTML Viewer" "open %o")))))

;;; --- CDLaTeX (fast math entry) ---
(use-package cdlatex
  :defer t
  :hook (LaTeX-mode . turn-on-cdlatex))

;;; --- Company-AUCTeX (completions) ---
(use-package company-auctex
  :defer t
  :after (company auctex)
  :config
  (company-auctex-init))

;;; --- LaTeX mode hook ---
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (abbrev-mode +1)
            (smartparens-mode +1)
            (turn-on-reftex)
            (LaTeX-math-mode 1)))

(setq reftex-plug-into-AUCTeX t)

(provide 'mod-latex)
;;; mod-latex.el ends here
