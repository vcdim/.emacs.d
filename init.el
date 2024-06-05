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

;; default behavior of <home> is the head of document
(global-set-key (kbd "<home>") 'beginning-of-line)

;; default behavior of <end> is the end of document
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
