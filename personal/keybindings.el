(define-key key-translation-map (kbd "C-?") (kbd "C-h"))
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(define-key key-translation-map (kbd "M-h") (kbd "M-DEL"))
(define-key key-translation-map (kbd "C-M-h") (kbd "C-M-<backspace>"))

(global-unset-key (kbd "M-u"))
(global-unset-key (kbd "s-a"))

(use-package helm
  :bind (:map helm-map
              ("C-j" . helm-previous-line)
              ("C-<tab>" . helm-select-action)
              ("TAB" . helm-execute-persistent-action)))


;; (require 'paredit-everywhere)
;; (define-key paredit-everywhere-mode-map (kbd "M-s") nil)
;; (define-key paredit-everywhere-mode-map (kbd "M-)") nil)
;; (define-key paredit-everywhere-mode-map (kbd "M-(") nil)

(use-package ido
  :bind (:map ido-common-completion-map
              ("C-f" . ido-next-match)
              ("C-b" . ido-prev-match)))

(use-package winner)
(use-package windmove)
(use-package helm-eval)
(use-package smart-parens)
(use-package term-config)
(use-package navigation)
(use-package swiper)
(use-package diff-hl)
(use-package hippie-exp)
(use-package org)
(use-package test-switcher
  :load-path "../vendor/test-switcher/")
(use-package expand-region)

(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c DEL") 'winner-undo)
    (define-key map (kbd "C-x 4 2 f") 'split-horiz-find)
    (define-key map (kbd "C-x 4 3 f") 'split-vert-find)
    (define-key map (kbd "S-s-h") 'buf-move-left)
    (define-key map (kbd "S-s-l") 'buf-move-right)
    (define-key map (kbd "S-s-k") 'buf-move-up)
    (define-key map (kbd "S-s-j") 'buf-move-down)
    (define-key map (kbd "s-h") 'windmove-left)
    (define-key map (kbd "s-l") 'windmove-right)
    (define-key map (kbd "s-k") 'windmove-up)
    (define-key map (kbd "s-j") 'windmove-down)
    (define-key map (kbd "s-n") 'next-line)
    (define-key map (kbd "s-p") 'previous-line)
    (define-key map (kbd "M-:") 'helm-eval-expression-with-eldoc)
    (define-key map (kbd "M-u") 'sp-splice-sexp-killing-backward)
    (define-key map (kbd "C-M-<backspace>") 'backward-kill-sexp)
    (define-key map (kbd "M-l") 'move-to-window-line-top-bottom)
    (define-key map (kbd "C-<return>") 'move-past-close-and-reindent)
    (define-key map (kbd "C-c t") 'term-project-root)
    (define-key map (kbd "M-)") 'kill-to-end-of-sexp)
    (define-key map (kbd "M-(") 'kill-to-beginning-of-sexp)
    (define-key map (kbd "C-M-q") 'unfill-region)
    (define-key map (kbd "M-m") 'delete-indentation)
    (define-key map (kbd "s-s") 'swiper)
    (define-key map (kbd "s-p") 'diff-hl-previous-hunk)
    (define-key map (kbd "s-n") 'diff-hl-next-hunk)
    (define-key map (kbd "s-a") 'move-to-first-alpha)
    (define-key map (kbd "s-b") 'up-one-coffee-block)
    (define-key map (kbd "C-M-y") 'yank-and-pop)
    (define-key map (kbd "C-M-i") 'hippie-expand)
    (define-key map (kbd "s-t") (lambda ()
                                  (interactive)
                                  (if (eq major-mode
                                          'org-mode)
                                      (org-todo)
                                    (test-switcher-toggle-between-implementation-and-test))))
    (define-key map (kbd "C-w") 'better-kill-line)
    (define-key map (kbd "M-s") 'sp-splice-sexp)
    (define-key map (kbd "C-j") 'previous-line)
    (define-key map (kbd "C-'") 'er/expand-region)
    (define-key map (kbd "C-;") (lambda () (interactive) (insert "-")))
    (define-key map (kbd "M-j") 'mark-paragraph)
    (define-key map (kbd "C-M-j") 'mark-to-end-of-paragraph)
    (define-key map (kbd "s-v") 'end-of-coffee-block)
    (define-key map (kbd "s-i") 'other-frame)
    (define-key map (kbd "s-2") (lambda (&optional arg)
                                  (interactive)
                                  (split-window-below)
                                  (windmove-down)))
    (define-key map (kbd "s-3") (lambda (&optional arg)
                                  (interactive)
                                  (split-window-right)
                                  (windmove-right)))
    (define-key map (kbd "s-1") 'delete-other-windows)
    (define-key map (kbd "s-0") 'delete-window)
    (define-key map (kbd "s-5") 'make-frame-command)
    (define-key map (kbd "s-<backspace>") 'winner-undo)
    (define-key map (kbd "s-f") 'helm-projectile-find-file)
    (define-key map (kbd "<f9> c") 'calendar)
    (define-key map (kbd "<f9> o") 'dh-make-org-scratch)
    (define-key map (kbd "<f9> n") 'org-narrow-to-subtree)
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter "")

(my-keys-minor-mode 1)

(defun dh-make-org-scratch ()
  (interactive)
  (crux-create-scratch-buffer)
  (org-mode))

(use-package helm-escreen
  :init
  (use-package escreen
    :bind (:map escreen-map
                ("n" . escreen-goto-next-screen)
                ("C-\\" . escreen-goto-last-screen)
                ("c" . helm-escreen-create-screen)
                ("s" . helm-escreen-select-escreen)
                ("k" . helm-escreen-kill-escreen)
                ("r" . helm-escreen-rename-escreen)
                ("w" . helm-escreen-current-escreen-name))))

(setq indent-rigidly-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-b")  'indent-rigidly-left)
        (define-key map (kbd "C-f") 'indent-rigidly-right)
        (define-key map (kbd "C-S-b")  'indent-rigidly-left-to-tab-stop)
        (define-key map (kbd "C-S-f") 'indent-rigidly-right-to-tab-stop)
        map))

(defun copy-line ()
  (interactive)
  (save-excursion
    (back-to-indentation)
    (set-mark-command nil)
    (move-end-of-line nil)
    (kill-ring-save (mark) (point))))

(defun end-of-line-p ()
  (interactive)
  (let ((cur-col (current-column))
        (end-col (save-excursion (end-of-line) (current-column))))
    (= cur-col end-col)))

(defun mark-at-end-or-copy-line ()
  (interactive)
  (if (or (use-region-p) (call-interactively 'end-of-line-p))
      (copy-line)
    (progn
      (set-mark-command nil)
      (move-end-of-line nil)
      (exchange-point-and-mark))))

(global-unset-key (kbd "s-y"))

(global-set-key (kbd "s-y") 'mark-at-end-or-copy-line)

(defun dh-get-relative-path ()
  (interactive)
  (insert (file-relative-name (read-string "Absolute Path: "))))

(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  "Don't allow esc esc esc to destroy other windows"
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(define-key dired-mode-map (kbd "TAB") 'dired-subtree-toggle)

(use-package helm-ag
  :bind (:map helm-ag-mode-map
              ("n" . next-logical-line)
              ("p" . previous-logical-line)
              ("o" . helm-ag-mode-jump-other-window)))

(global-unset-key (kbd "s-e"))
(global-unset-key (kbd "s-r"))

(global-set-key (kbd "s-e") (lambda () (interactive) (scroll-up-command 3)))
(global-set-key (kbd "s-r") (lambda () (interactive) (scroll-down-command 3)))

(define-key global-map (kbd "C-x C-c") nil)

(provide 'keybindings)
