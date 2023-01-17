(setq inhibit-startup-message t)

;; UI-tweaks
(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable toolbar
(tooltip-mode -1)       ; Disable tooltip
(set-fringe-mode 10)    ; Give some breathing room
(menu-bar-mode -1)      ; Disable menu bar

;;; Keybindings
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Switch buffer
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;;; Relative line numbers
(column-number-mode)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Automatch brackets
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)

;;; Face attributes
;; Fixed pitch face
;(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 260)
;; Variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 295 :weight 'regular)

;;; Packages
;; Init package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Init usepackage
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-ivy flymake-diagnostic-at-point flymake-diagnostics-at-point flymake-flycheck lsp-treemacs treemacs-icons-dired treemacs-evil treemacs flycheck lsp-ui company-box csharp-mode org-modern visual-fill-column org-bullets evil-magit magit counsel-projectile projectile lsp-pyright pyvenv pyenv company eglot general all-the-icons helpful ivy-rich which-key rainbow-delimiters undo-fu evil-collection doom-modeline counsel ivy mindre-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Mindre theme download
(use-package mindre-theme
  :ensure t
  :config
  (load-theme 'mindre t))

;; Completion
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))


;; Installing counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-f" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ; Don't start searches with ^

;; Doom modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;;; Evil mode
;; Vim-style undo
(use-package undo-fu)

;; Vim bindings
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

;; Vim bindings | Everywhere
(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Ivy-rich-mode
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))
		    
;; Better help
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Icons. All of them
(use-package all-the-icons)

;;; LSP
;; Header breadcrumbs
(defun ty/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . ty/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'top))

(use-package lsp-ivy)

;; Company
(use-package company
  :ensure t
  :custom
  (company-minimum-prefix-length 1)
  (company--idle-delay 0.0))
(global-company-mode)
(setq eldoc-echo-area-use-multiline-p nil)
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))
;; LSP-mode for python
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp)))) ; Or lsp-deferred

;; C#
(use-package csharp-mode
  :ensure t
  :init
  (add-hook 'csharp-mode-hook #'company-mode)
  (add-hook 'csharp-mode-hook #'rainbow-delimiters-mode)
  :mode "\\.cs\\'"
  :hook (csharp-mode . lsp-defered))

;; General - space as leaderkey
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer my/leader-keys
			  :keymaps '(normal visual emacs)
			  :prefix "SPC"
			  :global-prefix "SPC"))

;; Keybindings with leader key
(my/leader-keys
  "t" '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "load theme")
  "x" '(:ignore x :which-key "execute")
  "xe" '(eval-buffer :which-key "eval-buffer")
  "f" '(dired :which-key "find-file")
  "w" '(:ignore w :which-key "window")
  "wv" '(split-window-right :which-key "vertical split")
  "ws" '(split-window-below :which-key "horizontal split")
  "wl" '(evil-window-right :which-key "right window")
  "wh" '(evil-window-left :which-key "left window")
  "wk" '(evil-window-up :which-key "upper window")
  "wj" '(evil-window-down :which-key "lower window")
  "wt" '(tab-bar-switch-to-next-tab :which-key "next tab")
  "wT" '(tab-bar-switch-to-prev-tab :which-key "previous tab")
  "o" '(:ignore o :which-key "org")
  "oc" '(org-toggle-checkbox :which-key "checkbox")
  "oa" '(org-agenda :which-key "agenda")
  "oS" '(org-schedule :which-key "schedule")
  "oD" '(org-deadline :which-key "deadline")
  "b" '(:ignore b :which-key "buffer")
  "bi" '(ibuffer :which-key "ibuffer")
  "bs" '(counsel-switch-buffer :which-key "switch buffer")
  "b <right>" '(next-buffer :which-key "next buffer")
  "b <left>" '(previous-buffer :which-key "previous buffer")
  "O" '(:ignore O :which-key "Open")
  "Ot" '(treemacs :which-key "treemacs")
  "l" '(:ignore l :which-key "lsp")
  "lf" '(lsp-find-definition :which-key "find definition")
  "lr" '(lsp-find-references :which-key "get references")
  "ld" '(flymake-show-buffer-diagnostics :which-key "diagnostics")
  "l =" '(lsp-format-buffer :which-key "format buffer")
  "lp" '(lsp-ui-peek-find-references :which-key "peek references")
  "ls" '(lsp-ivy--workspace-symbol :which-key "search class"))

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Documents")
    (setq projectile-project-search-path '("~/Documents")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; MAGIT
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;(use-package evil-magit
;  :after magit)

;; Orgmode - Setup
(defun ty/org-mode-setup ()
  (org-indent-mode)
;  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . ty/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t)
  (setq org-agenda-files '("~/Documents/tasks.org"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "Done(d!)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANCELLED(k@)")))
  (setq org-refile-targets
	'(("~/Documents/archive.org" :maxlevel . 1)
	  ("~/Documents/tasks.org" :maxlevel . 1)))
  ; Save org-buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

;; Org-modern
(use-package org-modern)
; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 10)
   (internal-border-width . 10)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(setq
; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────")

(global-org-modern-mode)

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winup-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-display-in-side-window t
	  treemacs-eldoc-display 'simple
	  treemacs-file-extension-regex treemacs-last-period-regex-value
	  treemacs-follow-after-init t
	  treemacs-indentation 2
	  treemacs-indentation-string " "
	  treemacs-width 30)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-erros-list)

;; flymake
(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

;; term
(use-package term
  :config
  (setq term-prompt-regexp "^[^#$%>] *"))

