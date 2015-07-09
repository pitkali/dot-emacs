;;; Utilities for writing the main init file

(require 'cl)

(defun my-load-path-add-subdirs (directory)
  "Adds DIRECTORY and all its immediate subdirectories to load-path."
  (loop for item in (directory-files directory t)
        when (and (file-directory-p item)
                  (not (string-match-p "/\.\.$" item))) ; exclude parent directory
        collecting item into subdirs
        finally (setq load-path (nconc subdirs load-path))))

(defun my-batch-add-keys (fn keydefs)
  "Add key bindings in batch. KEYDEFS should be a list of dotted
pairs (key . definition). FN should be a function taking 2
arguments: that key and definition."
  (loop for (key . def) in keydefs
        do (funcall fn key def)))

(defun my-batch-define-key (keymap keydefs)
  (my-batch-add-keys #'(lambda (key def)
                         (define-key keymap key def))
                     keydefs))

(defun my-local-set-keys (keydefs)
  (my-batch-add-keys #'local-set-key keydefs))
(defun my-global-set-keys (keydefs)
  (my-batch-add-keys #'global-set-key keydefs))

(defun my-file-finder (filename)
  "Returns interactive function that opens FILENAME."
  `(lambda ()
     (interactive)
     (find-file ,filename)))

(defun my-three-pane-layout ()
  (interactive)
  (let ((pane-width (/ (window-width) 3))
        (orig-window (selected-window)))
    (select-window (split-window-right pane-width))
    (split-window-right pane-width)
    (select-window orig-window)))

(defun my-recompile-init ()
  (interactive)
  (loop for d in '("site-packages" "elpa" "init")
        do (byte-recompile-directory (concat "~/.emacs.d/" d) 0)))

(provide 'my-init-tools)
