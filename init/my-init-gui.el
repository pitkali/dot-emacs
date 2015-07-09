;;; Initialise themes and such

(require 'cl)

(load-theme 'solarized-light t)
(set-face-attribute 'default nil :family "Consolas" :height 140)

(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 105))

(defun my-make-frame-hook ()
  "Fixes delete key behaviour in graphical frames, and activates
new frames on Mac OS X."
  (when (window-system)
    (normal-erase-is-backspace-mode t)
    (when (eq window-system 'ns)
      (ns-do-applescript "tell application \"Emacs\" to activate"))))

;; Do this for initial frame...
(my-make-frame-hook)

;; ...and for any subsequently created frame.
(add-hook 'after-make-frame-functions
          #'(lambda (frame)
              (select-frame frame)
              (my-make-frame-hook)))

(setq truncate-partial-width-windows 100)

(unless (eq window-system 'ns)
  (defun auto-fci-mode (&optional unused)
    "Attempts to automatically enable fci mode for text and file
buffers, but only if window width is larger than
fci-fill-column."
    (if (or (equalp 'major-mode 'text-mode)
            (buffer-file-name))
        (let ((rule-column (or fci-rule-column fill-column)))
          (fci-mode (if (> (window-width) rule-column) 1 0)))
      (fci-mode 0)))
  (add-hook 'after-change-major-mode-hook #'auto-fci-mode)
  (add-hook 'window-configuration-change-hook #'auto-fci-mode))

(defun my-toggle-fullscreen ()
  "Toggle fullscreen mode on both ns and mac window systems."
  (interactive)
  (cond
   ((eq window-system 'mac)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
   ((eq window-system 'ns)
    (ns-toggle-fullscreen))
   (t nil)))

(defun my-theme-loader (theme)
  "Returns interactive function loading a THEME."
  `(lambda ()
     (interactive)
     (load-theme (quote ,theme) t)))

(defun my-toggle-solarized ()
  "Toggles between light and dark solarized theme."
  (interactive)
  (let ((current-theme (car custom-enabled-themes)))
    (cond
     ((eql current-theme 'solarized-dark)
      (load-theme 'solarized-light t))
     ((eql current-theme 'solarized-light)
      (load-theme 'solarized-dark t))
     (t nil))))

;;; End:

(provide 'my-init-gui)
