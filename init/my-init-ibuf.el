;;; Initialisation of ibuffer and extensions

(require 'ibuf-ext)
(defvar my-ibuffer-filter-groups
  '(("emacs" (or
              (name . "^\\*scratch\\*$")
              (name . "^\\*Messages\\*$")
              (name . "^\\*Help\\*$")))
    ("gnus" (or
             (mode . message-mode)
             (mode . bbdb-mode)
             (mode . mail-mode)
             (mode . gnus-group-mode)
             (mode . gnus-summary-mode)
             (mode . gnus-article-mode)
             (name . "^\\.bbdb$")
             (name . "^\\.newsrc-dribble")))
    ("gtags" (name . "^\\*GTAGS SELECT\\*"))
    ("special" (name . "^\\*.*\\*$"))
    ("dired" (mode . dired-mode)))
  "My custom pre-set groups for ibuffer.")

(defun my-generate-ibuffer-filter-groups ()
  "Generate my very own grouping of buffers. Just for fun."
  (append my-ibuffer-filter-groups (ibuffer-vc-generate-filter-groups-by-vc-root)))

(defun my-set-ibuffer-filter-groups ()
  "Sets filter groups, and refreshes ibuffer."
  (setq ibuffer-filter-groups (my-generate-ibuffer-filter-groups))
  (ibuffer-update nil t))

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

(defun my-ibuffer-hook ()
  (interactive)
  (setq ibuffer-filter-groups (my-generate-ibuffer-filter-groups))
  (ibuffer-do-sort-by-vc-status)
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)))
  (ibuffer-update nil t))

(add-hook 'ibuffer-hook #'my-ibuffer-hook)
(define-key ibuffer-mode-map [?s ?v] #'ibuffer-do-sort-by-vc-status)
(define-key ibuffer-mode-map [?/ ?0] #'my-set-ibuffer-filter-groups)

(provide 'my-init-ibuf)
