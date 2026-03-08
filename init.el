(defun my/org-babel-tangle-config()
  (when (string-equal (buffer-file-name) (expand-file-name "config.org" user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil)) (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook 'my/org-babel-tangle-config)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; avoid checking signature to avoid error. not a great practise.
(setq package-check-signature nil)

(require 'use-package)
(setq use-package-always-ensure t)

;; keep the .emacs.d folder clean
(setq no-littering-etc-directory (expand-file-name "~/.config/emacs/etc/"))
(setq no-littering-var-directory (expand-file-name "~/.config/emacs/var/"))

;; we need to have the variable BEFORE the setting!
(use-package no-littering)

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

;; change the tab behavior
(setq tab-always-indent 'complete)

;; move the auto generated custom code to a file
(setq custom-file (concat no-littering-var-directory "custom-vars.el"))
(load custom-file 'noerror 'nomessge)

;; auto-revert
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; save-place
(save-place-mode 1)
(setq save-place-forget-unreadable-files nil)

;; move backup file (afile~) autosave file (#afile#) to tmp
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; enable truncate lines and visual-line-mode (word-wrap) by default
(set-default 'truncate-lines t)
(global-set-key (kbd "C-z") 'toggle-truncate-lines)
(global-visual-line-mode t)

(setq column-number-mode t)

(display-battery-mode)

(scroll-bar-mode -1)

(savehist-mode 1)

;; system default encoding
(prefer-coding-system 'utf-8)

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

(setq org-hide-leading-stars t)
(setq org-hide-emphasis-markers t)

;; org-refile
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file) 
(setq org-outline-path-complete-in-steps nil)
(setq org-tags-column -100)

(defun insert-zero-width-space () (interactive) (insert-char #x200b))
(defun my-latex-filter-zws (text backend info)
  (when (org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string "\x200B" "{}" text)))
(global-set-key (kbd "C-*") 'insert-zero-width-space)

(defun my/monospace-font-p (family)
  "Use buffer to test width length"
  (condition-case nil
      (with-temp-buffer
        (let ((face `(:family ,family :height 200)))
          (insert (propertize "iiii" 'face face))
          (let ((w1 (string-pixel-width
                     (propertize "iiii" 'face face)))
                (w2 (string-pixel-width
                     (propertize "MMMM" 'face face))))
            (and (> w1 0) (= w1 w2)))))
    (error nil)))

(defun my/nerd-mono-fonts ()
  "Local nerd mono fonts"
  (cl-remove-duplicates
   (cl-loop for f in (font-family-list)
            when (and (string-match-p "Nerd Font" f)
		      (not (string-match-p "Symbols" f))
                      (my/monospace-font-p f))
            collect f)
   :test #'string-equal))

(use-package cnfonts
  :init
  (setq cnfonts-directory (expand-file-name "~/.config/emacs/var/cnfonts/"))
  :config
  (cnfonts-mode 1)
  (setq cnfonts-use-face-font-rescale t)
  (setq cnfonts-personal-fontnames
        (list (my/nerd-mono-fonts)
              '("Songti SC" "SimSong" "Adobe Fangsong Std" "Sarasa Mono SC")
              '("Simsun-ExtB"))))

(define-key cnfonts-mode-map (kbd "C--") #'cnfonts-decrease-fontsize)
(define-key cnfonts-mode-map (kbd "C-=") #'cnfonts-increase-fontsize)

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

(when (memq window-system '(mac ns x))
  (use-package vterm
    :init
    (setq vterm-always-compile-module t)
    :bind
    (:map vterm-mode-map("M-j" . my/toggle-vterm-below))
    )
  (use-package multi-vterm)
  )

(defvar my/vterm-window nil
  "The vterm side window.")

(defun my/toggle-vterm-below ()
  "Toggle a vterm window at the bottom of the frame."
  (interactive)
  (if (and my/vterm-window (window-live-p my/vterm-window))
      ;; Close it
      (progn
        (delete-window my/vterm-window)
        (setq my/vterm-window nil))
    ;; Open it
    (let ((vterm-buf (get-buffer "*vterm*")))
      (setq my/vterm-window (split-window-below -15))
      (select-window my/vterm-window)
      (if vterm-buf
          (switch-to-buffer vterm-buf)
        (vterm))
      ;; Prevent other buffers from taking over this window
      (set-window-dedicated-p my/vterm-window t))))

(defun my/vterm-exit-cleanup (buf _msg)
  "Close the vterm window and kill the buffer when the shell exits."
  (let ((win (get-buffer-window buf)))
    (when win (delete-window win)))
  (kill-buffer buf))

(with-eval-after-load 'vterm
  (setq vterm-exit-function #'my/vterm-exit-cleanup))

(global-set-key (kbd "M-j") 'my/toggle-vterm-below)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-expert t)

(use-package diredfl)
(setq diredfl-global-mode t)

(use-package treemacs)
(global-set-key (kbd "C-x t t") 'treemacs)

(use-package citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode))
(setq citar-org-roam-note-title-template "${title}\n#+author: ${author}\n")

(use-package smartparens-mode
  :ensure smartparens
  :hook (prog-mode markdown-mode)
  :config
  (require 'smartparens-config))

(bind-keys
 :map smartparens-mode-map
 ("C-M-a" . sp-beginning-of-sexp)
 ("C-M-e" . sp-end-of-sexp)
 ("C-<down>" . sp-down-sexp)
 ("C-<up>"   . sp-up-sexp)
 ("M-<down>" . sp-backward-down-sexp)
 ("M-<up>"   . sp-backward-up-sexp)
 ("C-M-f" . sp-forward-sexp)
 ("C-M-b" . sp-backward-sexp)
 ("C-M-n" . sp-next-sexp)
 ("C-M-p" . sp-previous-sexp)
 ("C-S-f" . sp-forward-symbol)
 ("C-S-b" . sp-backward-symbol)
 ("C-<right>" . sp-forward-slurp-sexp)
 ("C-<left>"  . sp-backward-slurp-sexp)
 ("M-<right>" . sp-forward-barf-sexp)
 ("M-<left>"  . sp-backward-barf-sexp)
 ("C-M-t" . sp-transpose-sexp)
 ("C-M-k" . sp-kill-sexp)
 ("C-k"   . sp-kill-hybrid-sexp)
 ("M-k"   . sp-backward-kill-sexp)
 ("C-M-w" . sp-copy-sexp)
 ("C-M-d" . delete-sexp)
 ("M-<backspace>" . backward-kill-word)
 ("C-<backspace>" . sp-backward-kill-word)
 ([remap sp-backward-kill-word] . backward-kill-word)
 ("M-[" . sp-backward-unwrap-sexp)
 ("M-]" . sp-unwrap-sexp)
 ("C-x C-t" . sp-transpose-hybrid-sexp)
 ("C-c ("  . sp-wrap-round)
 ("C-c ["  . sp-wrap-square)
 ("C-c {"  . sp-wrap-curly))

(use-package tex
  :ensure auctex)
(setq-default TeX-master nil)
(setq TeX-parse-self t)
(setq TeX-engine 'xetex)
(setq TeX-command-extra-options "-shell-escape")
(setq TeX-electric-sub-and-superscript t)
(setq TeX-auto-save t)
(setq TeX-command-default "XeLaTeX")
(setq TeX-save-query nil)
(setq TeX-show-compilation nil)
(setq TeX-source-correlate-start-server t)
(setq LaTeX-verbatim-environments-local '("minted"))
(add-to-list 'TeX-command-list
	     '("XeLaTeX" "%`xelatex%(mode)%' -shell-escape -synctex=1 %t" TeX-run-TeX nil t))
;; come back to tex file after compilation finishes
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

(use-package cdlatex)
(add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)

(setq org-latex-create-formula-image-program 'dvisvgm)
(plist-put org-format-latex-options :scale 1.5)

(use-package which-key)
(which-key-mode)

(use-package consult)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-miramare t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(use-package visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key global-map (kbd "C-c m") 'vr/mc-mark)

(use-package simple-modeline
  :hook (after-init . simple-modeline-mode))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(use-package linum-relative
  :config
  (setq linum-relative-backend 'display-line-numbers-mode)
  )
