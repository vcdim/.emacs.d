(defun my/org-babel-tangle-config()
  (when (string-equal (buffer-file-name) (expand-file-name "init.org" user-emacs-directory))
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
