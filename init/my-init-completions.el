;;; Support for code auto completion

(require 'cl)

;;; company

;; Better key bindings for company mode.
(require 'company)
(require 'my-init-tools)

(my-batch-define-key
 company-active-map
 `(([?\C-n] . company-select-next)
   ([?\C-p] . company-select-previous)
   ([?\C-d] . company-show-doc-buffer)
   ([?\C-v] . company-show-location)
   (,(kbd "<tab>") . company-complete)))

;; Run completions only upon changing something, rather than just moving
;; around.
(setq company-begin-commands '(self-insert-command))

(add-hook 'python-mode-hook
          #'(lambda ()
              (add-to-list 'company-backends 'company-anaconda)))
(add-hook 'after-init-hook 'global-company-mode)

;; Force completions start
(global-set-key [?\C-/] #'company-complete)

;; Make AC and company way of specifying clang arguments synonymous.
(makunbound 'ac-clang-flags)
(defvaralias 'ac-clang-flags 'company-clang-arguments)

;; Explain to Emacs that strings and lists of strings are safe clang
;; arguments, when set from file or directory local variables.
(defun my-safe-clang-arguments (value)
  "Safe clang arguments are: strings, lists of strings."
  (or (stringp value)
      (and (listp value)
           (reduce (lambda (a b) (and a b))
                   (mapcar #'stringp value)
                   :initial-value t))))

(put 'ac-clang-flags 'safe-local-variable
     #'my-safe-clang-arguments)
(put 'company-clang-arguments 'safe-local-variable
     #'my-safe-clang-arguments)

;;; End:

(provide 'my-init-completions)
