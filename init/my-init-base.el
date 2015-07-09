;;; Some basic settings

(require 'icomplete+)
(require 'flex-isearch)
(require 'dired-x)
(require 'smex)
(require 'fill-column-indicator)

(global-whitespace-mode t)              ; Show some white space characters.
(global-hl-line-mode t)                 ; Highlight line with cursor.
(global-auto-revert-mode t)             ; Reload files changed on disk without asking.
(delete-selection-mode t)               ; Replace active region by default after typing something.

;; Enable enhanced completions in Emacs minibuffer
(ido-mode t)
(ido-ubiquitous-mode t)                 ; The real ido-everywhere
(icomplete-mode t)                      ; Nice preview of completions
(icompletep-cycling-mode t)             ; Enable cycling in icomplete


(global-flex-isearch-mode)

(global-set-key [?\M-x] 'smex)
(global-set-key [?\M-X] 'smex-major-mode-commands)
(global-set-key [?\C-x ?\C-b] 'ibuffer)

(defun smex-update-after-load (&optional unused)
  (when (boundp 'smex-cache)
    (smex-update)))
(add-hook 'after-load-functions #'smex-update-after-load)

(setq dired-bind-jump t)                ; Bind jump to keys

;; Legacy M-x
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(setq mouse-wheel-progressive-speed nil) ; Don't accelerate scrolling.
(setq mouse-wheel-follow-mouse 't)       ; Scroll window under mouse, like on Mac.

(setq-default cursor-type 'bar)
(setq-default cursor-in-non-selected-windows 'hollow)

(setq inhibit-startup-message t)        ; ... and go straight to scratch buffer.
(setq ring-bell-function #'ignore)
(setq require-final-newline t)
(fset 'yes-or-no-p 'y-or-n-p)           ; Yes, I'm that lazy.

;; Let completion ignore case of entered text.
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

(setq-default major-mode 'text-mode)
(setq-default fill-column 78)           ; Reasonable default for all modes

(make-variable-buffer-local 'fci-rule-column)
(setq-default fci-rule-column 100)

;; Fill settings for text-like modes
(defun my-text-fill-hook ()
  (setq fill-column 72)
  (setq fci-rule-column nil)
  )           ; Show rule at fill column
(loop
 for mode-hook in '(text-mode-hook message-mode-hook gnus-article-mode-hook)
 do (add-hook mode-hook #'my-text-fill-hook))

(define-globalized-minor-mode global-fci-mode
  fci-mode fci-mode)

(provide 'my-init-base)
