;;; Configuration for C-like languages

(defun* my-c-select-next-argument ()
  "Select next argument of a function call in C-like syntax.

This is intended to iterate over arguments after inserting code
completion that contains not only function name, but also whole
argument list (e.g. when inserting completion generated by
ac-source-clang on Mac OS X).

If point is just after closing parenthesis, it first moves
backward over argument list. This is the only movement that may
span multiple lines.

Note that next argument may not be after next comma or
parenthesis. If point is between a comma and a first
non-indentation character after this comma, the next argument
starts after indentation and extends to the next comma or paren."
  (interactive)
  (condition-case nil
      (progn
        (cond ((equal (preceding-char) ?\)) ; Just after insertion of a completion ...
               (backward-sexp))             ; ... move backward over all of the arguments.
              ((not (looking-back "[(,][ 	]*" (point-at-bol))) ; Not at start of an argument ...
               (search-forward-regexp "[(,]" (point-at-eol))) ; ... so find end of current one.
              (t nil))
        ;; Find start of an argument.
        (search-forward-regexp "[^(, 	]" (point-at-eol))
        (goto-char (1- (point))))
    (search-failed (return-from my-c-select-next-argument)))

  ;; Select all of the argument contents.
  (push-mark (point) nil t)
  (if (search-forward-regexp "[),]" (point-at-eol) t)
      (goto-char (1- (point)))
    (goto-char (point-at-eol))))

(require 'google-c-style)
(c-add-style "Google" google-c-style nil)

(setq-default c-basic-offset 4)

(defun my-c-common-hook ()
  ;; Text width control
  (turn-on-auto-fill)
  (my-highlight-todos)

  ;; Doxygen comments syntax colouring
  (require 'doxymacs)
  (doxymacs-mode t)
  (doxymacs-font-lock)

  ;; Tags and other supporting functionalities
  (ggtags-mode t)
  (define-key c-mode-base-map [?\C-'] #'my-c-select-next-argument))
(add-hook 'c-mode-common-hook #'my-c-common-hook)
(add-hook 'c-mode-common-hook #'dtrt-indent-mode)

(defun my-java-hook ()
  (require 'java-mode-indent-annotations)
  (java-mode-indent-annotations-setup))
(add-hook 'java-mode-hook #'my-java-hook)

;;; End:

(provide 'my-init-c)