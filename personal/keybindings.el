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
    (define-key map (kbd "C-SPC") 'my-set-mark)
    (define-key map (kbd "M-:") 'helm-eval-expression-with-eldoc)
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter "")

(my-keys-minor-mode 1)

(defun copy-line ()
  (interactive)
  (save-excursion
    (back-to-indentation)
    (set-mark-command nil)
    (move-end-of-line nil)
    (kill-ring-save (mark) (point))))

(global-unset-key (kbd "s-y"))

(global-set-key (kbd "s-y s-y") 'copy-line)

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

(define-key company-active-map (kbd "<escape>") 'company-abort)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)

(global-unset-key (kbd "s-e"))
(global-unset-key (kbd "s-r"))

(global-set-key (kbd "s-e") 'scroll-up-line)
(define-key prelude-mode-map (kbd "s-r") 'scroll-down-line)

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

(provide 'keybindings)
