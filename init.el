(defun my/org-babel-tangle-config()
  (when (string-equal (buffer-file-name) (expand-file-name "init.org" user-emacs-directory))
    ;; Setting org-confirm-babel-evaluate as nil can skip confirmation
    ;; Function org-bable-tangle can export .org to .el
    (let ((org-confirm-babel-evaluate nil)) (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook 'my/org-babel-tangle-config)))

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(setq package-check-signature nil)

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(setq system-time-locale "C")

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq-default cursor-type 'bar)
(set-fringe-mode 10)
(global-hl-line-mode t)
;; (toggle-frame-maximized)
(set-default 'truncate-lines t)
(set-default 'word-wrap t)
(setq truncate-partial-width-windows nil)
(display-battery-mode t)
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
		    ((numberp (cdr alpha)) (cdr alpha))
		    ((numberp (cadr alpha)) (cadr alpha)))
	      100)
	 '(85 . 50) '(100 . 100)))))
(setq display-line-numbers 'relative)

(use-package doom-themes
  :ensure t
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (setq doom-themes-enable-italic nil)
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (doom-themes-visual-bell-config)
  )

(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-current-symbol "->")
  (setq linum-relative-backend 'display-line-numbers-mode)  
)

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1)
  )

(use-package cnfonts
  :ensure t
  :custom
  (cnfonts-personal-fontnames
   '((
      "MesloLGS NF"
      "Jetbrains Mono"
      "agave Nerd Font"
      "BigBlue_Terminal_437TT NFM"
      "iMWritingMonoS NFM"
      "Hurmit NFM"
      "ShureTechMono NFM"
      "IosevkaTerm NFM"
      "Iosevka NFM"
      "Lekton NFM"
      "CodeNewRoman NFM"
      "FantasqueSansMono NFM"
      "CMU Typewriter Text")
     ()
     ()))
  :config
  (cnfonts-enable)
  (setq cnfonts-use-face-font-rescale t)
  (setq use-default-font-for-symbols nil)
  
  ;; zoom Chinese characters together with
  (setq face-font-rescale-alist '(("STSong" . 1.2) ("STXihei" . 1.2) ("STFangsong" . 1.2) ("STKaiti" . 1.2)))

  ;; a fix for Chinese punctuations
  (defun blaenk/set-char-widths (alist)
    (while (char-table-parent char-width-table)
      (setq char-width-table (char-table-parent char-width-table)))
    (dolist (pair alist)
      (let ((width (car pair))
	    (chars (cdr pair))
	    (table (make-char-table nil)))
	(dolist (char chars)
	  (set-char-table-range table char width))
	(optimize-char-table table)
	(set-char-table-parent table char-width-table)
	(setq char-width-table table))))
  (blaenk/set-char-widths
   `((1 . (,(string-to-char "”")
	   ,(string-to-char "“")
	   ,(string-to-char "…")
	   ))))
  )

(use-package all-the-icons
  :ensure t)

(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil :height 0.7)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d" (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))

(setq calendar-view-diary-initially-flag t
      diary-number-of-entries 7
      diary-display-function #'diary-fancy-display)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

(use-package mini-frame
  :ensure t
  )
(custom-set-variables
 '(mini-frame-show-parameters
   '((top . 10)
     (width . 0.7)
     (left . 0.5))))

(setf (cdr (assq 'continuation fringe-indicator-alist))
      ;; '(nil nil) ;; no continuation indicators
      '(nil right-curly-arrow) ;; right indicator only
      ;; '(left-curly-arrow nil) ;; left indicator only
      ;; '(left-curly-arrow right-curly-arrow) ;; default
      )

(use-package counsel
  :ensure t
  :config
  (setq ivy-use-selectable-prompt t)
  :bind
  ("C-x C-f" . counsel-find-file)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> o" . counsel-describe-symbol)
  ("<f1> l" . counsel-find-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  ("M-s-j" . ivy-immediate-done)
  ("C-s" . swiper)
  )

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package prescient
  :ensure t
  )
(use-package vertico-prescient
  :ensure t
  )
(vertico-prescient-mode +1)
(prescient-persist-mode +1)

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode)
  )

(use-package company
  :ensure t
  :config
  (setq company-tooltip-limit 20)
  (setq company-show-numbers t)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  )
(add-hook 'after-init-hook 'global-company-mode)

(use-package sly
  :ensure t
  )

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0)
  )

(use-package multiple-cursors
  :ensure t
  :bind
  ("s-d" . mc/mark-next-like-this)
  ("M-s-<down>" . mc/mark-next-lines)
  ("M-s-<up>" . mc/mark-previous-lines)
  ("C-S-<mouse-1>" . mc/add-cursor-on-click)
  )

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package ibuffer
  :ensure t
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

(use-package smartparens-config
  :ensure smartparens
  :config (progn (show-smartparens-global-mode t)))
(sp-local-pair 'latex-mode "\{" "\}")
(sp-local-pair 'latex-mode "\|" "\|")

(setq ispell-program-name "hunspell")
(setq ispell-hunspell-dict-paths-alist '(("en_US" "c:/ProgramData/chocolatey/bin/dictpath/en_US.aff")))
(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemcs-collapse-dirs                   (if treemacs-python-executable 3 0)
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
    (treemacs-resize-icons 44)

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
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package magit
  :ensure t
  )
;; a fix for project-switch-commands to be void
;; Reference: https://libredd.it/r/emacs/comments/po9cfj/magit_commands_broken/?sort=new
(setq project-switch-commands t)

(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Change the behavior of =<home>= and =<end>=.
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

;; Disable C-z to minimize
(global-unset-key (kbd "C-z"))

(setq user-full-name "Qun Gu")

(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(savehist-mode 1)
(desktop-save-mode 1)
(setq desktop-load-locked-desktop t)
(recentf-mode 1)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first"))

;; Inserting text while the mark is active causes the selected text to be deleted first
(delete-selection-mode 1)

;; keep the buffer up-to-date
(global-auto-revert-mode t)

(setq delete-by-moving-to-trash t)

(use-package writeroom-mode
  :ensure t)

(use-package paredit
  :ensure t)

(with-eval-after-load 'org
  (require 'org-tempo)
  (setq org-startup-indented t)
  (setq org-edit-src-content-indentation 0)
  (setq org-support-shift-select t)
  (add-to-list 'org-structure-template-alist '("el" . "src elisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (toggle-word-wrap 1)
  (modify-syntax-entry ?_ "w")

  (defun insert-zero-width-space () (interactive) (insert-char #x200b))
  (defun my-latex-filter-zws (text backend info)
    (when (org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string "\x200B" "{}" text)))
  (global-set-key (kbd "C-*") 'insert-zero-width-space)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.7))

  (setq org-emphasis-alist
        '(("*" (bold :foreground "magenta"))
          ("/" (italic :foreground "cyan"))
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
  
  ;; Hide Closed tasks in org-agenda
  (defun org-toggle-agenda-show-closed-logs ()
    (interactive)
    (if (equal (car org-agenda-log-mode-items) 'closed)
	(setq org-agenda-log-mode-items '(clock))
      (setq org-agenda-log-mode-items '(closed clock)))
    (org-agenda-redo)
    )
  
  ;; zotero
  (org-link-set-parameters "zotero" :follow (lambda (zpath) (browse-url(format "zotero:%s" zpath))))
  
  (setq org-agenda-files (directory-files-recursively "~/SynologyDrive/roam/worklog/" "\\.org$"))
  
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "ACTIVE(a)" "|" "DONE(d)" "CANCEL(c)")))
  
  ;; automatic track done time
  (setq org-log-done 'time)

  ;; 显示已经完成的任务
  (setq org-agenda-start-with-log-mode '(closed))
  (setq org-habit-show-done-always-green t)
  (setq org-habit-graph-column 80)
  (setq org-habit-show-all-today t)
  (setq org-agenda-archives-mode t)
  
  (setq org_notes (concat (getenv "HOME") "SynologyDrive/notes/"))
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)
  
;;  (setq org-refile-use-outline-path 'file)
;;  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-targets '((nil :maxlevel . 9)
                                (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (setq org-refile-use-outline-path t)                  ; Show full paths for refiling
  
  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Consolas"))))
   )
  )

(setq org-pomodoro-start-sound "~/.emacs.d/sounds/focus_bell.wav")
(setq org-pomodoro-short-break-sound "~/.emacs.d/sounds/three_beeps.wav")
(setq org-pomodoro-long-break-sound "~/.emacs.d/sounds/three_beeps.wav")
(setq org-pomodoro-finished-sound "~/.emacs.d/sounds/meditation_bell.wav")

(use-package org-download
  :ensure t
  :after org
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-image-actual-width 300)
  (org-download-screenshot-method "/usr/local/bin/pngpaste %s")
  :bind
  ("C-M-y" . org-download-screenshot)
  :config
  (require 'org-download))

(setq org-capture-templates
  '(
    ("d" "TODO" entry (file+headline "~/SynologyDrive/roam/worklog/todo.org" "Tasks for Today")
     "* %?\nCaptured: %U")
    ("p" "org-protocol" entry (file+headline "~/SynologyDrive/roam/worklog/inbox.org")
     "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%:initial\n#+END_QUOTE\n\n\n%?")
    ("l" "org-protocol link" entry (file "~/SynologyDrive/roam/worklog/inbox.org")
     "* %? [[%:link][%:description]] \nCaptured On: %U")
    )
  )

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-db-update-on-save t)
  (setq org-roam-directory "~/SynologyDrive/roam/")
  (setq org-roam-dailies-directory "worklog/")
  (setq org-id-extra-files (directory-files-recursively org-roam-directory "\.org$"))
  :custom
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n d" . org-roam-dailies-goto-today)
         )
  :config
  ;; (setq org-roam-database-connector 'sqlite3)
  (setq
   org-roam-dailies-capture-templates
   '(
     ("t" "task" entry
      "* %?\nSCHEDULED: %T"
      :target
      (file+datetree "journal.org" day)
      )
     )
   )
  (org-roam-db-autosync-mode)
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
"* %?\nEntered on %U\n  %i\n  %a"                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  )

(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :config
  (require 'org-ref)) ; optional: if using Org-ref v2 or v3 citation links

(use-package org-roam-ui
  :ensure t
  :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow nil
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil)
  )

(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda() (org-superstar-mode 1)))
  (setq org-superstar-item-bullet-alist
	 '((?* . ?★)
	   (?+ . ?❒)
     (?- . ?➛))
	 )
  )

(use-package org-super-agenda
  :ensure t
  :config
  (setq org-super-agenda-groups
	'(
	  (:name "Scheduled"
		 :time-grid t
		 )
	  (:name "Done"
		 :todo "DONE")
	  )
	)
  (setq
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-compact-blocks t)
  )

(use-package org-ref
  :ensure t
  :config
  (require 'org-ref-helm)
  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
  )

(require 'org-protocol)

(setq TeX-engine 'xetex)
(setq TeX-command-extra-options "-shell-escape")

(use-package auctex
  :ensure t
  :defer t
  :config
  (setq-default TeX-master nil)
  (setq TeX-parse-self t)
  )
(add-hook 'LaTeX-mode-hook 
          (lambda()
             (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' -synctex=1 %t" TeX-run-TeX nil t))
             (setq TeX-command-default "XeLaTeX")
             (setq TeX-save-query nil)
             (setq TeX-show-compilation nil)))

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

(custom-set-variables
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
     ("tabbing"))))

(setq reftex-label-alist '((nil ?e nil "~\\eqref{%s}" nil nil)))
(setq TeX-style-private (expand-file-name "~/.emacs.d/auctex/styles"))
(setq font-latex-match-reference-keywords
      '(
	("citeauthor" "[{")
	("citep" "[{")))

(add-to-list 'insert-pair-alist (list ?\$ ?\$))
(global-set-key (kbd "M-$") 'insert-pair)

(use-package auctex-latexmk
  :ensure t
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  )

(use-package cdlatex
  :ensure t
  :config
  (add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)
  (setq cdlatex-paired-parens "$[{(|")
  )

(setq TeX-view-program-list '(("SumatraPDF"
  ("\"C\:/Users/guqun/AppData/Local/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
   (mode-io-correlate " -forward-search %b %n")
   " %o"))))
(setq TeX-view-program-selection '(((output-dvi style-pstricks)
  "dvips and start")
 (output-dvi "start")
 (output-pdf "SumatraPDF")
 (output-html "start")))
(setq TeX-source-correlate-mode t)

(setq bibtex-completion-bibliography '("~/SynologyDrive/Library/bib/mybib.bib"))
(setq bibtex-completion-pdf-field "file")
(setq bibtex-completion-notes-path "~/SynologyDrive/notes")
(setq org-cite-follow-processor 'helm-bibtex-org-cite-follow)
(setq bibtex-completion-display-formats
    '((article       . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${journal:40}")
      (inbook        . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
      (incollection  . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
      (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
      (t             . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*}")))
(setq bibtex-completion-additional-search-fields '(keywords))
(setq bibtex-completion-pdf-symbol "#")
(setq bibtex-completion-notes-symbol "N")
(setq bibtex-completion-pdf-open-function
 (lambda (fpath)
	(call-process
	 "C\:\\Users\\guqun\\AppData\\Local\\SumatraPDF\\SumatraPDF.exe" nil 0 nil "-reuse-instance" fpath)))

(use-package zotxt
  :ensure t
  :defer t
  )
(add-hook 'org-mode-hook (lambda () (org-zotxt-mode 1)))
(define-key org-mode-map
  (kbd "C-c \" \"") (lambda () (interactive)
                      (org-zotxt-insert-reference-link '(4))))

(pdf-tools-install)
(setq-default pdf-view-display-size 'fit-height)

(use-package markdown-mode
  :ensure t)

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  )

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
	'("http://arxiv.org/rss/cs.CV"
	  "http://arxiv.org/rss/cs.IR"))

  (setq elfeed-show-mode-hook
      (lambda ()
	(set-face-attribute 'variable-pitch (selected-frame) :font (font-spec :size 24))
	(setq fill-column 120)
	(setq elfeed-show-entry-switch #'my-show-elfeed)))

  (defun my-show-elfeed (buffer)
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (fill-individual-paragraphs (point) (point-max))
      (setq buffer-read-only t))
    (switch-to-buffer buffer))
  )
(defun concatenate-authors (authors-list)
    "Given AUTHORS-LIST, list of plists; return string of all authors concatenated."
    (if (> (length authors-list) 1)
        (format "%s et al." (plist-get (nth 0 authors-list) :name))
      (plist-get (nth 0 authors-list) :name)))

(defun my-search-print-fn (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
        (title (or (elfeed-meta entry :title)
                    (elfeed-entry-title entry) ""))
        (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
        (entry-authors (concatenate-authors
                        (elfeed-meta entry :authors)))
        (title-width (- (window-width) 10
                        elfeed-search-trailing-width))
        (title-column (elfeed-format-column
                        title 80
                        :left))
        (entry-score (elfeed-format-column (number-to-string (elfeed-score-scoring-get-score-from-entry entry)) 10 :left))
        (authors-column (elfeed-format-column entry-authors 40 :left)))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")

    (insert (propertize title-column
                        'face title-faces 'kbd-help title) " ")
    (insert (propertize authors-column
                        'kbd-help entry-authors) " ")
    (insert entry-score " ")))

(setq elfeed-search-print-entry-function #'my-search-print-fn)
(setq elfeed-search-date-format '("%y-%m-%d" 10 :left))
(setq elfeed-search-title-max-width 110)
(setq elfeed-search-filter "@2-week-ago +unread")
(add-hook 'elfeed-search-mode-hook 'elfeed-update)

(use-package elfeed-org
  :ensure t
  :config
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
  (elfeed-org)
  )

(use-package elfeed-score
  :ensure t
  :config
  (elfeed-score-load-score-file "~/.emacs.d/elfeed.score")
  (elfeed-score-enable)
  (define-key elfeed-search-mode-map "=" elfeed-score-map)
)

(use-package org2blog
  :ensure t
  :config
  (setq org2blog/wp-blog-alist
	'(("guqun"
           :url "http://galoisgu.com/wordpress/xmlrpc.php"
           :username "guqun"))
	org2blog/wp-show-post-in-browser 'show
	org2blog/wp-use-wp-latex nil)
  )

(use-package ox-hugo
  :ensure t
  :after ox
  :config
  (setq org-hugo-base-dir "~/SynologyDrive/myhugo/")
  (setq org-hugo-default-section-directory "post")
  )

(use-package easy-hugo
  :ensure t
  :init
  (setq easy-hugo-basedir "~/SynologyDrive/myhugo/")
  (setq easy-hugo-postdir "content/post/")
  (setq easy-hugo-url "http://galoisgu.com/hugo/")
  (setq easy-hugo-sshdomain "aliyun")
  (setq easy-hugo-root "/var/www/html/hugo/")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-default-ext ".org")
  (setq easy-hugo-org-header t)
  (define-key global-map (kbd "C-c C-e") 'easy-hugo)
  )

(set-fontset-font "fontset-default" '(#x2010 . #x2027) "宋体" nil 'prepend)
(load-theme 'doom-one t)
(require 'server)
(or (eq (server-running-p) t)
    (server-start))
