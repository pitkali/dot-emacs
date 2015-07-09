;;; Common development related setup

(require 'autopair)
(require 'ggtags)

(require 'my-init-tools)

(autopair-global-mode t)
(setq autopair-skip-whitespace t)

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode highlight-parentheses-mode)
(global-highlight-parentheses-mode t)

(defvar electrify-return-match
  "[\]}\)]"
  "If this regexp matches the text after the cursor, do an \"electric\"
return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
open and indent an empty line between the cursor and the text.  Move the
cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(global-set-key (kbd "RET") #'newline-and-indent)
(setq-default indent-tabs-mode nil)

(defun my-highlight-todos ()
  "Sets warning face for FIXMEs, TODOs and such."
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|BUG\\)\\(([^)]*)\\)?:"
          1 font-lock-warning-face t))))

(autoload 'ggtags-mode "ggtags" "" t)

(setq magit-last-seen-setup-instructions "1.4.0")

;; (define-prefix-command 'my-gtags-map nil "Gtags")
;; (global-set-key [?\C-\M-.] 'my-gtags-map)

;; (my-batch-define-key
;;  my-gtags-map
;;  '(("d" . gtags-visit-rootdir)
;;    ("f" . gtags-find-file)
;;    ("t" . gtags-find-tag)
;;    ("o" . gtags-find-tag-other-window)
;;    ("r" . gtags-find-rtag)
;;    ("s" . gtags-find-symbol)
;;    ("g" . gtags-find-with-grep)
;;    ("." . gtags-find-tag-from-here)
;;    ("\M-." . gtags-find-tag-from-here)))

;; (global-set-key [?\M-.] 'gtags-find-tag-from-here)
;; (global-set-key [?\M-*] 'gtags-pop-stack)

;; (my-batch-define-key
;;  gtags-select-mode-map
;;  '(([?\C-m] . gtags-select-tag)
;;    ([?\M-.] . gtags-select-tag)
;;    ([?\C-o] . gtags-select-tag-other-window)
;;    ([?j]    . next-line)
;;    ([?k]    . previous-line)
;;    ([?n]    . next-line)
;;    ([?p]    . previous-line)
;;    ([?q]    . gtags-pop-stack)))

;;; End:

(provide 'my-init-dev-common)
