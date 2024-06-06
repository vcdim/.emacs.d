(defun my/org-babel-tangle-config()
  (when (string-equal (buffer-file-name) (expand-file-name "config.org" user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil)) (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook 'my/org-babel-tangle-config)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'use-package)
(setq use-package-always-ensure t)

;; enable smooth scrolling experience
(pixel-scroll-precision-mode)

;; default behavior of <home>/<end> is the head/end of document
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

;; this is mostly for Windows
(global-unset-key (kbd "C-z"))

;; delete directly
(setq delete-by-moving-to-trash t)

;; use y and n, instead of yes and no
(setopt use-short-answers t)

;; put backup files into temp file
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; change the tab behavior
(setq tab-always-indent 'complete)

;; move the auto generated custom code to a file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessge)

;; auto-revert
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; save-place
(save-place-mode 1)

;; I like to use shift to select
(setq org-support-shift-select t)

;; This will fix the annoying auto formatting in the elisp code of org src
(setq org-src-preserve-indentation t)

;; Add shortcut to org mode, e.g.
;; <el<TAB> will insert a elisp code block
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src elisp"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(defun my/set-font ()
  (let ((default-font (font-spec :name "Iosevka" :size 15))
	(cn-font (font-spec :name "Sarasa Mono SC")))
    (set-face-attribute 'default nil :font default-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font t charset cn-font)))
  )

(defun my/frame-behaviors (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (my/set-font)
    ))

;; for server
(add-hook 'server-after-make-frame-hook 'my/frame-behaviors)

;; for normal start
(my/frame-behaviors)

(use-package magit)

(use-package markdown-mode)

(use-package vertico)
(vertico-mode)

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package corfu)
(global-corfu-mode)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia)
(marginalia-mode)

(use-package all-the-icons-completion)
(all-the-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)

(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)

(use-package vterm
  :init
  (setq vterm-always-compile-module t))

(use-package multi-vterm)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-expert t)

(use-package diredfl)
(setq diredfl-global-mode t)

(use-package treemacs)
(global-set-key (kbd "C-x t t") 'treemacs)

(use-package org-roam
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n j" . org-roam-dailies-capture-today)
   ("C-c n d" . org-roam-dailies-goto-today)
   )
  )
(setq org-roam-directory "~/Niuwa/03_Qun/roam/")

;; relative path to org-roam-directory
(setq org-roam-dailies-directory "daily/")

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %?" :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

;; global org-capture
(defun my/org-capture ()
  (interactive)
  (delete-other-windows)
  (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'switch-to-buffer))
    (condition-case err (org-roam-dailies-capture-today)
      (error (when (equal err '(error "Abort")) (delete-frame))))))

(defadvice org-capture-finalize (after delete-capture-frame activate)
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(use-package citar
  :custom
  (citar-bibliography '("~/Niuwa/03_Qun/common/gq.bib"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  (markdown-mode . citar-capf-setup))

;; better looking
(defvar citar-indicator-files-icons (citar-indicator-create
   :symbol (all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1)
   :function #'citar-has-files :padding "  " :tag "has:files"))
(defvar citar-indicator-links-icons (citar-indicator-create
   :symbol (all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01)
   :function #'citar-has-links :padding "  " :tag "has:links"))
(defvar citar-indicator-notes-icons (citar-indicator-create
   :symbol (all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3)
   :function #'citar-has-notes :padding "  " :tag "has:notes"))
(defvar citar-indicator-cited-icons (citar-indicator-create
   :symbol (all-the-icons-faicon "circle-o" :face 'all-the-icon-green)
   :function #'citar-is-cited :padding "  " :tag "is:cited"))
(setq citar-indicators
      (list citar-indicator-files-icons citar-indicator-links-icons
            citar-indicator-notes-icons citar-indicator-cited-icons))

;; open pdf by default viewer
(add-to-list 'citar-file-open-functions '("pdf" . citar-file-open-external))
