(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(c-default-style (quote ((awk-mode . "awk") (other . "bsd"))))
 '(c-doc-comment-style nil)
 '(c-offsets-alist (quote ((case-label . +) (arglist-intro . ++))))
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(desktop-save-mode t)
 '(geiser-default-implementation (quote racket))
 '(ggtags-global-abbreviate-filename nil)
 '(gnus-build-sparse-threads (quote more))
 '(gnus-buttonized-mime-types (quote ("multipart/signed")))
 '(gnus-thread-indent-level 2)
 '(gnus-treat-display-smileys nil)
 '(ido-enable-flex-matching t)
 '(ido-use-filename-at-point (quote guess))
 '(imenu-sort-function (quote imenu--sort-by-name))
 '(ispell-dictionary "british")
 '(mm-discouraged-alternatives (quote ("text/html" "text/richtext")))
 '(ns-alternate-modifier (quote none))
 '(ns-command-modifier (quote meta))
 '(ns-right-alternate-modifier (quote none))
 '(package-archives
   (quote
    (("melpa" . "http://melpa.milkbox.net/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(project-tags-form-default nil)
 '(quack-default-program "racket")
 '(quack-global-menu-p nil)
 '(save-place t nil (saveplace))
 '(semantic-default-submodes
   (quote
    (global-semantic-stickyfunc-mode global-semantic-idle-scheduler-mode global-semantic-idle-summary-mode global-semantic-mru-bookmark-mode)))
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(spell-command "aspell")
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(user-mail-address "pitkali@gmail.com")
 '(whitespace-style (quote (face trailing tab-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'cl)
(package-initialize)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/init/"))

(require 'my-init-tools)

;; Add site-packages and all immediate subdirectories to the load-path.
(my-load-path-add-subdirs "~/.emacs.d/site-packages/")

(require 'my-init-gui)
(require 'my-init-base)
(require 'my-init-ibuf)

(global-fci-mode t)

(autoload 'project-mode "project-mode" "Project Mode" t)
(autoload 'dtrt-indent-mode "dtrt-indent"
  "Adapt to foreign indentation offsets" t)

(global-set-key [(control meta down-mouse-3)] 'imenu)

(require 'my-init-completions)
(require 'my-init-dev-common)
(require 'my-init-lispy)
(require 'my-init-c)


;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

;; (add-hook 'python-mode-hook 'anaconda-mode)
;; (add-hook 'python-mode-hook 'eldoc-mode)


(require 'windmove)
(define-prefix-command 'my-windmove)
(global-set-key [?\C-8] my-windmove)
(my-batch-define-key
 my-windmove
 '(("l" . windmove-right)
   ("h" . windmove-left)
   ("j" . windmove-down)
   ("k" . windmove-up)))

;; Disable some of the default flyspell bindings. They'd override some of mine.
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mode-map [?\C-,] nil)
     (define-key flyspell-mode-map [?\C-.] nil)))

(my-global-set-keys
 '(([f5] . (lambda () (interactive)
             (revert-buffer t t)))
   ([f6] . delete-trailing-whitespace)

   ;; Various new flyspell bindings
   ([f7] . flyspell-mode)
   ([S-f7] . flyspell-prog-mode)
   ([M-f7] . flyspell-goto-next-error)
   ([f8] . ispell-word)
   ([S-f8] . flyspell-check-previous-highlighted-word)
   ([M-f8] . (lambda () (interactive)
               (flyspell-goto-next-error)
               (ispell-word)))
   ([M-S-f8] . flyspell-buffer)
   ([C-S-f8] . flyspell-region)

   ([f9] . my-three-pane-layout)
   ([f10] . my-toggle-fullscreen)
   ([?\C-c ?1] . fill-paragraph)
   ([?\C-c ?2] . fill-region)
   ([?\C-9]    . previous-buffer)
   ([?\C-0]    . next-buffer)
   ([?\C-\M-0] . kill-this-buffer)
   ([A-left]  . backward-word)
   ([A-right] . forward-word)))


;; Prefix for my special key bindings
(define-prefix-command 'my-map)
(global-set-key [?\C-\\] my-map)

(require 'remember)
(setq org-directory "~")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-default-todo-file  (concat org-directory "/todo.org"))

(my-batch-define-key
 my-map
 `(("," . ,(my-file-finder "~/.emacs.d/init.el"))
   ("r" . org-remember)
   ("n" . ,(my-file-finder org-default-notes-file))
   ("t" . ,(my-file-finder org-default-todo-file))
   ("w" . toggle-truncate-lines)
   ("F" . global-fci-mode)
   ("f" . fci-mode)
   ("g" . magit-status)
   ("s" . idomenu)
   ("o" . ff-find-other-file)
   (";" . ispell-word)))

(setq desktop-dirname "~/.emacs.d/")
(define-prefix-command 'my-desktop)
(define-key my-map "d" my-desktop)

(my-batch-define-key
 my-desktop
 '(("d" . desktop-save-in-desktop-dir)
   ("s" . desktop-save)
   ("l" . desktop-read)
   ("c" . desktop-change-dir)
   ("r" . desktop-revert)
   ("x" . desktop-clear)))

(define-prefix-command 'my-colour-themes)
(define-key my-map "c" my-colour-themes)

(my-batch-define-key
 my-colour-themes
 `(("z" . ,(my-theme-loader 'zenburn))
   ("d" . ,(my-theme-loader 'solarized-dark))
   ("l" . ,(my-theme-loader 'solarized-light))
   ("c" . my-toggle-solarized)))

(let ((site-locals "~/.emacs.d/site-locals.el"))
  (when (file-readable-p site-locals)
    (load site-locals)))

(require 'server)
(unless (server-running-p)
  (server-start))
