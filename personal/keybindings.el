(require 'god-mode)

(define-key key-translation-map (kbd "C-?") (kbd "C-h"))
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(define-key key-translation-map (kbd "M-h") (kbd "M-DEL"))
(define-key key-translation-map (kbd "C-M-h") (kbd "C-M-<backspace>"))

(global-unset-key (kbd "M-u"))
(global-unset-key (kbd "s-a"))

(require 'helm)
(define-key helm-map (kbd "C-j") 'helm-previous-line)
(define-key helm-map (kbd "C-<tab>") 'helm-select-action)
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)

(require 'paredit-everywhere)
(define-key paredit-everywhere-mode-map (kbd "M-s") nil)
(define-key paredit-everywhere-mode-map (kbd "M-)") nil)
(define-key paredit-everywhere-mode-map (kbd "M-(") nil)

(require 'ido)
(define-key ido-common-completion-map (kbd "C-f") 'ido-next-match)
(define-key ido-common-completion-map (kbd "C-b") 'ido-prev-match)

(require 'term)
(define-key term-raw-map (kbd "C-v") 'scroll-up-command)
(define-key term-raw-map (kbd "M-v") 'scroll-down-command)
(define-key term-raw-map (kbd "M-f") 'forward-word)
(define-key term-raw-map (kbd "M-b") 'backward-word)
(define-key term-raw-map (kbd "C-f") 'forward-char)
(define-key term-raw-map (kbd "C-b") 'backward-char)
(define-key term-raw-map (kbd "C-b") 'backward-char)

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
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter "")

(my-keys-minor-mode 1)

(require 'crux)
(defun better-kill-line (&optional arg)
  (interactive "p")
  (if mark-active
      (kill-region (region-beginning) (region-end))
    (crux-kill-whole-line arg)))

(require 'escreen)
(require 'helm-escreen)
(define-key escreen-map "n" 'escreen-goto-next-screen)
(define-key escreen-map (kbd "C-\\") 'escreen-goto-last-screen)
(define-key escreen-map "c" 'helm-escreen-create-screen)
(define-key escreen-map "s" 'helm-escreen-select-escreen)
(define-key escreen-map "k" 'helm-escreen-kill-escreen)
(define-key escreen-map "r" 'helm-escreen-rename-escreen)
(define-key escreen-map "w" 'helm-escreen-current-escreen-name)

(dumb-jump-mode -1)
(define-key dumb-jump-mode-map (kbd "C-M-p") nil)
(define-key dumb-jump-mode-map (kbd "C-M-g") nil)
(define-key dumb-jump-mode-map (kbd "C-M-.") 'dumb-jump-go)
(define-key dumb-jump-mode-map (kbd "C-x 4 C-M-.") 'dumb-jump-go-other-window)
(define-key dumb-jump-mode-map (kbd "C-M-,") 'dumb-jump-back)
(define-key dumb-jump-mode-map (kbd "C-M-j") 'dumb-jump-quick-look)

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
  ;;(unless god-global-mode (god-mode-all))
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(require 'coffee-mode)
(define-key coffee-mode-map (kbd "C-c C-c r") 'coffee-compile-region)
(define-key coffee-mode-map (kbd "C-c C-c b") 'coffee-compile-buffer)

(define-key dired-mode-map (kbd "TAB") 'dired-subtree-toggle)

(define-key company-active-map (kbd "<escape>") 'company-abort)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)

(require 'helm-ag)

(define-key helm-ag-mode-map (kbd "n") 'next-logical-line)
(define-key helm-ag-mode-map (kbd "p") 'previous-logical-line)
(define-key helm-ag-mode-map (kbd "o") 'helm-ag-mode-jump-other-window)

(global-unset-key (kbd "s-e"))
(global-unset-key (kbd "s-r"))

(global-set-key (kbd "s-e") (lambda () (interactive) (scroll-up-command 3)))
(define-key prelude-mode-map (kbd "s-r") (lambda () (interactive) (scroll-down-command 3)))

(global-set-key (kbd "s-`") #'modalka-global-mode)

;;; Modalka Mode
(add-hook 'modalka-global-mode-hook
          (lambda ()
            (if modalka-global-mode
                (progn
                  (set-cursor-color "#3BBBBB"))
              (progn
                (set-cursor-color "#FFFFEF")))))

(modalka-remove-kbd "h")
(modalka-remove-kbd "j")
(modalka-remove-kbd "k")
(modalka-remove-kbd "l")

(modalka-define-kbd "h" "C-b")
(modalka-define-kbd "j" "C-n")
(modalka-define-kbd "k" "C-p")
(modalka-define-kbd "l" "C-f")

(modalka-define-kbd "M-h" "M-b")
(modalka-define-kbd "M-j" "M-e")
(modalka-define-kbd "M-k" "M-a")
(modalka-define-kbd "M-l" "M-f")

(modalka-define-kbd "s-h" "s-<left>")
(modalka-define-kbd "s-j" "s-<down>")
(modalka-define-kbd "s-k" "s-<up>")
(modalka-define-kbd "s-l" "s-<right>")

(setq modalka-cursor-type 'box)

(define-key prelude-mode-map (kbd "s-h") nil)
(define-key prelude-mode-map (kbd "s-j") nil)
(define-key prelude-mode-map (kbd "s-k") nil)
(define-key prelude-mode-map (kbd "s-l") nil)

(define-key global-map (kbd "C-x C-c") nil)

;; God mode config
(define-key god-local-mode-map (kbd ".") 'repeat)
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)
(define-key god-local-mode-map (kbd "i") 'god-mode-all)
(define-key god-local-mode-map (kbd "h") 'sp-backward-delete-char)

(require 'god-mode-isearch)
(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)

(define-key prelude-mode-map (kbd "s-g") 'god-mode-all)

(add-to-list 'god-exempt-major-modes 'dired-mode)
(add-to-list 'god-exempt-major-modes 'magit-mode)
(add-to-list 'god-exempt-major-modes 'term-mode)

(provide 'keybindings)
