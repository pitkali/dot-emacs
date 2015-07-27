;;; Lisp-related configuration

(require 'cl)
(require 'eldoc)
(require 'geiser)

(require 'my-init-tools)

(eval-after-load "geiser-mode"
  '(define-key geiser-mode-map [?\C-.] nil))

(defun my-paredit-keys ()
  "Add my extra custom paredit key bindings."
  (my-local-set-keys '(([?\C-\;] . paredit-open-round)
                       ([?\C-,] . paredit-backward)
                       ([?\C-.] . paredit-forward))))

(defun my-paredit-space-for-delimiter-predicates-racket (endp delimiter)
  "Do not automatically insert a space after some reader directives."
  (or endp
      (cond ((eq (char-syntax delimiter) ?\()
             (and (not (looking-back "\\(#\\|#fl\\|#fx\\)[0-9]*")) ; Vectors
                  (not (looking-back "#\\(s\\|hash\\|hasheq\\|hasheqv\\)")))) ; Structs and hashes
            ((eq (char-syntax delimiter) ?\")
             (not (looking-back "#\\|#rx\\|#px")))
            (t t))))

(defun my-paredit-setup (&optional arg)
  "Enable paredit instead of autopair with extra configuration.
This will enable electrify return, so that pressing return before
closing parenthesis will move the paren to the next line.
Additionally, some handy key bindings will added to the current
keymap.

If provided a prefix argument, it will disable paredit, and
enable autopair back, disabling electrify return as well, as
autopair does not remove those added extra newlines. It will not,
however, remove the added keybindings."
  (interactive "P")
  (cond
   ((not arg)
    (autopair-mode 0)
    (paredit-mode t)
    (my-paredit-keys))
   (t
    (paredit-mode 0)
    (autopair-mode 1))))

(defun my-surround-with-eval-when ()
  "Surrounds current top-level form with full eval-when
invocation, while preserving the point and reindenting
everything."
  (interactive)
  (save-excursion
    (beginning-of-defun-raw)
    (let ((p (point)))
      (paredit-wrap-sexp)
      (insert "eval-when (:compile-toplevel :load-toplevel :execute)\n")
      (goto-char p))
    (indent-sexp))
  ;; Fix highlight of active parentheses.
  (when (fboundp #'hl-paren-color-update)
    (hl-paren-color-update)))

(defun my-insert-eval-when ()
  "Inserts full evel-when invocation at point."
  (interactive)
  (insert "(eval-when (:compile-toplevel :load-toplevel :execute)\n"
          "\n)")
  (backward-sexp)
  (indent-sexp)
  (forward-sexp)
  (forward-line -1)
  (indent-according-to-mode))

(defun my-lisp-common-hook ()
  (my-paredit-setup)
  (eldoc-mode)
  (eldoc-add-command
   #'paredit-backward-delete
   #'paredit-close-round
   #'electrify-return-if-match)
  (local-set-key "\C-m" #'electrify-return-if-match))

(defun my-lisp-hook ()
  (my-lisp-common-hook)
  (my-local-set-keys '(("\C-cew" . my-surround-with-eval-when)
                       ("\C-cei" . my-insert-eval-when))))

(defun my-scheme-hook ()
  (my-lisp-common-hook)
  (when (fboundp #'auto-complete-mode)
    (auto-complete-mode 0))
  (geiser-mode)
  (add-to-list (make-local-variable 'paredit-space-for-delimiter-predicates)
               #'my-paredit-space-for-delimiter-predicates-racket))

(loop
 for hook in '(emacs-lisp-mode-hook
               lisp-mode-hook
               lisp-interaction-mode-hook
               slime-repl-mode-hook)
 do (add-hook hook #'my-lisp-hook))
(add-hook 'scheme-mode-hook #'my-scheme-hook)


;;; Some clojure/cider goodness

(add-hook 'clojure-mode-hook #'my-lisp-common-hook)
(add-hook 'cider-repl-mode-hook #'my-lisp-common-hook)
(setq cider-repl-use-clojure-font-lock t)


;;; Slime setup

(let ((ql-slime (expand-file-name "~/quicklisp/slime-helper.el")))
  (when (file-exists-p ql-slime)
    (load ql-slime)))
(setq inferior-lisp-program "ccl64")
(setq slime-net-coding-system 'utf-8-unix)
(slime-setup '(slime-company slime-repl))

(add-hook 'slime-mode-hook
          #'(lambda ()
              (unless (slime-connected-p)
                (save-excursion (slime)))))

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook
          #'override-slime-repl-bindings-with-paredit)

;; Slime selector
(global-set-key "\C-cs" #'slime-selector)

;;; End:

(provide 'my-init-lispy)
