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
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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
   '(lsp-pyright pyvenv pyenv company eglot general all-the-icons helpful ivy-rich which-key rainbow-delimiters undo-fu evil-collection doom-modeline counsel ivy mindre-theme use-package)))
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
;; Company
(use-package company
  :ensure t)
(global-company-mode)
(setq eldoc-echo-area-use-multiline-p nil)
;; LSP-mode for python
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp)))) ; Or lsp-deferred

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
  "wh" '(split-window-below :which-key "horizontal split")
  "wl" '(evil-window-right :which-key "right window")
  "wh" '(evil-window-left :which-key "left window")
  "wk" '(evil-window-up :which-key "upper window")
  "wj" '(evil-window-down :which-key "lower window")
  "wt" '(tab-bar-switch-to-next-tab :which-key "next tab")
  "wT" '(tab-bar-switch-to-prev-tab :which-key "previous tab"))
