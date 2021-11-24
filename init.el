(defun my/org-babel-tangle-config()
  (when (string-equal (buffer-file-name) (expand-file-name "init.org" user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil)) (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook 'my/org-babel-tangle-config)))

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
(setq-default cursor-type 'bar)
(tool-bar-mode -1)
(set-fringe-mode 10)
(toggle-frame-maximized)
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))
(display-battery-mode t)

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-dracula t)
  (setq doom-themes-enable-italic nil)
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (doom-themes-visual-bell-config)
  )

(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-mode 1)
  )

(use-package cnfonts
  :straight t
  :custom
  (cnfonts-personal-fontnames
   '(("MesloLGS NF" "JetBrains Mono")
     ()
     ()))
  :config
  (cnfonts-enable)
  (setq cnfonts-use-face-font-rescale t)
  ;; 使得放缩时，中文能够跟着一起放缩
  (setq face-font-rescale-alist '(("STSong" . 1.2) ("STXihei" . 1.2) ("STFangsong" . 1.2) ("STKaiti" . 1.2)))
  )

(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil :height 0.7)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d" (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))

(use-package mini-frame
  :straight t
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-indent-environment-list
   '(("minted" current-indentation)
     ("verbatim" current-indentation)
     ("verbatim*" current-indentation)
     ("filecontents" current-indentation)
     ("filecontents*" current-indentation)
     ("tabular" LaTeX-indent-tabular)
     ("tabular*" LaTeX-indent-tabular)
     ("align" LaTeX-indent-tabular)
     ("align*" LaTeX-indent-tabular)
     ("array" LaTeX-indent-tabular)
     ("eqnarray" LaTeX-indent-tabular)
     ("eqnarray*" LaTeX-indent-tabular)
     ("displaymath")
     ("equation")
     ("equation*")
     ("picture")
     ("tabbing")))
 '(mini-frame-show-parameters '((top . 10) (width . 0.7) (left . 0.5)))
 '(safe-local-variable-values '((TeX-command-extra-options . "-shell-escape"))))

(use-package counsel
  :straight t
  :bind
  ("C-x C-f" . counsel-find-file)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> o" . counsel-describe-symbol)
  ("<f1> l" . counsel-find-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  ("M-s-j" . ivy-immediate-done)
  )

(use-package selectrum
  :straight t
  :config
  (selectrum-mode +1)
  )

(use-package prescient
  :straight t
  )
(use-package selectrum-prescient
  :straight t
  )
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)

(use-package consult
  :straight t
  )

(use-package marginalia
  :straight t
  :init
  (marginalia-mode)
  )

(use-package company
  :straight t
  :config
  (setq company-tooltip-limit 20)
  (setq company-show-numbers t)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  )

(use-package company-auctex
  :straight t
  :config
  (company-auctex-init))

(use-package which-key
  :straight t
  :diminish
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0)
  )

(use-package multiple-cursors
  :straight t
  :bind
  ("s-d" . mc/mark-next-like-this)
  ("M-s-<down>" . mc/mark-next-lines)
  ("M-s-<up>" . mc/mark-previous-lines)
  )

(use-package keycast
  :straight t)
(with-eval-after-load 'keycast
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))

  (add-to-list 'global-mode-string '("" mode-line-keycast)))

(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode))

(use-package tree-sitter-langs
  :straight t)

(use-package tree-sitter
  :straight t
  :hook (((python-mode) . tree-sitter-mode)
         ((python-mode) . tree-sitter-hl-mode)))

(use-package csv-mode
  :straight t)

(use-package ibuffer
  :straight t
  :bind
  ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-expert t) ; stop yes no prompt on delete

  (setq ibuffer-saved-filter-groups
	(quote (("DEFAULT"
		 ("DIRED" (mode . dired-mode))
		 ("ORG" (mode . org-mode))
		 ("MAGIT" (name . "^magit"))
		 ("PLANNER" (or
			     (name . "^\\*Calendar\\*$")
			     (name . "^\\*Org Agenda\\*")))
		 ("EMACS" (or
			   (name . "^\\*scratch\\*$")
			   (name . "^\\*Messages\\*$")))))))

  (add-hook 'ibuffer-mode-hook
	    (lambda ()
	      (ibuffer-switch-to-saved-filter-groups "DEFAULT")))
  )

(setq org-pomodoro-start-sound "~/.emacs.d/sounds/focus_bell.wav")
(setq org-pomodoro-short-break-sound "~/.emacs.d/sounds/three_beeps.wav")
(setq org-pomodoro-long-break-sound "~/.emacs.d/sounds/three_beeps.wav")
(setq org-pomodoro-finished-sound "~/.emacs.d/sounds/meditation_bell.wav")

(setq TeX-engine 'xetex)
(setq TeX-command-extra-options "-shell-escape")

(use-package auctex
  :straight t
  :defer t
  )
(add-hook 'LaTeX-mode-hook 
          (lambda()
            (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' -shell-escape %t" TeX-run-TeX nil t))
            (setq TeX-command-default "XeLaTeX")
            (setq TeX-save-query nil)
            (setq TeX-show-compilation t)))

(use-package cdlatex
  :straight t
  :defer t
  )

(use-package zotxt
  :straight t
  :defer t  
  )

(use-package treemacs
  :straight t
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   t
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :straight t
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :straight t
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :straight t
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :straight t
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package magit
  :straight t
  )

(use-package vterm
  :straight t
  )

(use-package multi-vterm
  :straight t
  )

(use-package benchmark-init
  :straight t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate)
  )

(global-set-key (kbd "C-c t") 'toggle-transparency)
;; Change the behavior of =<home>= and =<end>=.
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

(setq user-full-name "Qun Gu")

(straight-use-package 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(savehist-mode 1)
(desktop-save-mode 1)
(recentf-mode 1)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first"))

(with-eval-after-load 'org
  (require 'org-tempo)
  (setq org-startup-indented t)
  (setq org-edit-src-content-indentation 0)
  (setq org-support-shift-select t)
  (add-to-list 'org-structure-template-alist '("el" . "src elisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (toggle-word-wrap 1)
  (toggle-truncate-lines 1)

  (defun insert-zero-width-space () (interactive) (insert-char #x200b))
  (defun my-latex-filter-zws (text backend info)
    (when (org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string "\x200B" "{}" text)))
  (global-set-key (kbd "C-*") 'insert-zero-width-space)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.7))

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

  (setq org-latex-listings 'minted)
  (setq org-latex-minted-options '(("breaklines" "true")
                                   ("breakanywhere" "true")
				   ))
  (setq org-highlight-latex-and-related '(latex script entities))
  (setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-agenda-files (directory-files-recursively "~/SynologyDrive/zettelkasten/daily/" "\\.org$"))

  ;; zotero
  (org-link-set-parameters "zotero" :follow (lambda (zpath) (browse-url(format "zotero:%s" zpath))))
  )

(use-package org-roam
  :straight t
  :defer t
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-db-update-on-save t)
  (setq org-roam-directory "~/SynologyDrive/roam/")
  (setq org-id-extra-files (directory-files-recursively org-roam-directory "\.org$"))
  :custom
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

(use-package org-superstar
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda() (org-superstar-mode 1)))
  )

(use-package org-super-agenda
  :straight t)

(defcustom TeX-buf-close-at-warnings-only t
  "Close TeX buffer if there are only warnings."
  :group 'TeX-output
  :type 'boolean)

(defun my-tex-close-TeX-buffer (_output)
  "Close compilation buffer if there are no errors.
Hook this function into `TeX-after-compilation-finished-functions'."
  (let ((buf (TeX-active-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (progn (TeX-parse-all-errors)
                     (or
                      (and TeX-buf-close-at-warnings-only
                           (null (cl-assoc 'error TeX-error-list)))
                      (null TeX-error-list)))
          (cl-loop for win in (window-list)
                   if (eq (window-buffer win) (current-buffer))
                   do (delete-window win)))))))

(add-hook 'TeX-after-compilation-finished-functions #'my-tex-close-TeX-buffer)

(defun LaTeX-indent-item ()
  "Provide proper indentation for LaTeX \"itemize\",\"enumerate\", and
\"description\" environments.

  \"\\item\" is indented `LaTeX-indent-level' spaces relative to
  the the beginning of the environment.

  Continuation lines are indented either twice
  `LaTeX-indent-level', or `LaTeX-indent-level-item-continuation'
  if the latter is bound."
  (save-match-data
    (let* ((offset LaTeX-indent-level)
           (contin (or (and (boundp 'LaTeX-indent-level-item-continuation)
                            LaTeX-indent-level-item-continuation)
                       (* 2 LaTeX-indent-level)))
           (re-beg "\\\\begin{")
           (re-end "\\\\end{")
           (re-env "\\(itemize\\|\\enumerate\\|description\\)")
           (indent (save-excursion
                     (when (looking-at (concat re-beg re-env "}"))
                       (end-of-line))
                     (LaTeX-find-matching-begin)
                     (current-column))))
      (cond ((looking-at (concat re-beg re-env "}"))
             (or (save-excursion
                   (beginning-of-line)
                   (ignore-errors
                     (LaTeX-find-matching-begin)
                     (+ (current-column)
                        (if (looking-at (concat re-beg re-env "}"))
                            contin
                          offset))))
                 indent))
             ((looking-at (concat re-end re-env "}"))
              indent)
            ((looking-at "\\\\item")
             (+ offset indent))
            (t
             (+ contin indent))))))

(defcustom LaTeX-indent-level-item-continuation 4
  "*Indentation of continuation lines for items in itemize-like
environments."
  :group 'LaTeX-indentation
  :type 'integer)

(eval-after-load "latex"
  '(setq LaTeX-indent-environment-list
         (nconc '(("itemize" LaTeX-indent-item)
                  ("enumerate" LaTeX-indent-item)
                  ("description" LaTeX-indent-item))
                LaTeX-indent-environment-list)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
