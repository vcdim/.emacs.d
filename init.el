(defun my/org-babel-tangle-config()
  (when (string-equal
	 (buffer-file-name)
	 (expand-file-name "init.org" user-emacs-directory)
	 )
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook
 'org-mode-hook
 (lambda ()
   (add-hook 'after-save-hook 'my/org-babel-tangle-config)
   ))

(defvar bootstrap-version)
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 5))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(toggle-frame-maximized)
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(display-battery-mode t)

(straight-use-package 'doom-themes)
(load-theme 'doom-dracula t)

(straight-use-package 'doom-modeline)
(doom-modeline-mode 1)

(use-package cnfonts
  :straight t
  :custom
  (cnfonts-personal-fontnames
   '(("MesloLGS NF" "JetBrains Mono")
     ()
     ()))
  :config
  (cnfonts-enable)
  )

(straight-use-package 'counsel)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

(straight-use-package 'selectrum)
(selectrum-mode +1)

(straight-use-package 'prescient)
(straight-use-package 'selectrum-prescient)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)

(use-package consult
  :straight t)

(use-package marginalia
  :straight t
  :init
  (marginalia-mode)
  )

(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

(savehist-mode 1)
(desktop-save-mode 1)
(recentf-mode 1)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(with-eval-after-load 'org
  (require 'org-tempo)
  (setq org-startup-indented t)
  (setq org-edit-src-content-indentation 0)
  (setq org-support-shift-select t)
  (add-to-list 'org-structure-template-alist '("el" . "src elisp"))
  (toggle-word-wrap 1)
  (toggle-truncate-lines 1)

  (defun insert-zero-width-space () (interactive) (insert-char #x200b))
  (defun my-latex-filter-zws (text backend info)
    (when (org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string "\x200B" "{}" text)))
  (global-set-key (kbd "C-*") 'insert-zero-width-space)

  (setq org-emphasis-alist
        '(("*" (bold :foreground "magenta"))
          ("/" (italic :foreground "cyan"))
          ("_" underline)
          ("=" org-verbatim verbatim)
          ("~" org-code verbatim)
          ("+" (:strike-through t)))
        )
  (setq org-hide-emphasis-markers t)
  (setq org-emphasis-regexp-components '("-[:multibyte:][:space:]('\"{" "-[:multibyte:][:space:].,:!?;'\")}\\[" "[:space:]" "." 1))
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (org-element-update-syntax)
  
  )

(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-db-update-on-save t)
  (org-roam-setup)
  :custom
  (org-roam-directory "~/SynologyDrive/zettelkasten/")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %U %?" :if-new
  (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
		 (display-buffer-in-side-window)
		 (side . right)
		 (slot . 0)
		 (window-width . 0.33)
		 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  )

(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
  )

(use-package org-superstar
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda() (org-superstar-mode 1)))
  )

(use-package which-key
  :straight t
  :diminish
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0)
  )

(use-package company
  :straight t
  :config
  )

(use-package magit
  :straight t
  )

(straight-use-package 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package vterm
  :straight t
  )
