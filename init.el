;; -*- coding: utf-8; lexical-binding: t; -*-

;; Set the default font
(set-face-attribute 'default nil :font "Monospace")

;; Enable line numbers
(global-display-line-numbers-mode)
; (add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Enable syntax highlighting
(global-font-lock-mode t)

;; Enable automatic indentation
                                        ;(electric-indent-mode t)
(setq-default standard-indent 2)
(setq-default indent-tabs-mode nil)
;; Automatically pair parentheses
(electric-pair-mode t)

;; Enable backup files
(setq make-backup-files t)

;; Enable auto-save files
(setq auto-save-default t)

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable the tool bar
(tool-bar-mode -1)

;; Disable the scroll bars
(scroll-bar-mode -1)

;; Disable startup message
(setq inhibit-startup-message t)

;; http://yummymelon.com/devnull/surprise-and-emacs-defaults.html
(delete-selection-mode t)
(setq sentence-end-double-space nil)
(setq dired-auto-revert-buffer t)

;; Add welcome msg
(setq initial-scratch-message "archie, Emacs loves you!\n")

;; package
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(defvar bootstrap-version)
(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-repository-branch "develop")
(straight-use-package 'use-package)

;; Optional
(setq use-package-always-ensure t)

;; magit
(use-package magit
  :ensure t
  :commands (magit-status magit-get-current-branch))

;; theme
(use-package zenburn-theme
  :ensure t
  :config
  ;; Load the theme after `use-package` installs it
  (load-theme 'zenburn t))

;; Vertico for completion
(use-package vertico
  :init
  (vertico-mode t)
  :config
  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
    (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-word)
    (define-key vertico-map (kbd "M-d") #'vertico-directory-delete-char)))

;; LSP Support with Eglot
(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :init
  (defalias 'start-lsp-server #'eglot))

;; format code
(use-package format-all
  :preface
  (defun ian/format-code ()
    "Auto-format whole buffer."
    (interactive)
    (if (derived-mode-p 'prolog-mode)
        (prolog-indent-buffer)
      (format-all-buffer)))
  :config
  (global-set-key (kbd "M-F") #'ian/format-code)
  (add-hook 'prog-mode-hook #'format-all-ensure-formatter))

;; Inline static analysis with Flymake
(add-hook 'prog-mode-hook #'flymake-mode)

;; Git client with Magit
(use-package magit
  :bind ("C-c g" . magit-status))

;; Indication of local VCS changes with diff-hl
(use-package diff-hl
  :hook (prog-mode . diff-hl-mode))

;; Go Support
(use-package go-mode)

;; JSON Support
(use-package json-mode)

;; Typescript Support
(use-package typescript-mode)

;; Markdown support
(use-package markdown-mode
  :ensure t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
      ("\\.md\\'" . markdown-mode)
      ("\\.markdown\\'" . markdown-mode)))

;; EditorConfig support
(use-package editorconfig
  :config
  (editorconfig-mode t))

;; Miscellaneous options
;;; This sets the default major mode for new buffers.
;;; The lambda function provided will be used to guess the major mode based on the file name.
;;; If buffer-file-name is nil (which it will be for new, unsaved buffers), it sets buffer-file-name to the buffer name and then calls set-auto-mode, which sets the major mode based on the file name.
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
;;; Remember cursor position in files when you close them,
;;; and restores the cursor position when you reopen the files.
(save-place-mode t)
;;; Saves the minibuffer history across Emacs sessions.
(savehist-mode t)
;;; maintains a menu of recently opened files.
(recentf-mode t)
;;; yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  :init
  (add-hook 'org-mode-hook #'yas-minor-mode)); Enable yasnippet in org-mode buffers

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :bind (:map copilot-mode-map (("TAB" . copilot-accept-completion)
                                 ("C-c 0 <down>" .  copilot-next-completion)
                                 ("C-c 0 <up>" . copilot-previous-completion)
                                 ("C-c 0 DEL" . copilot-clear-overlay)
                                 ("C-c 0 TAB" . copilot-panel-complete)
                                 ("C-c 0 ESC" . copilot-mode)))
  :bind ("C-c 0 ESC" . copilot-mode)
  :custom
  ;; Copilot...never give me code comment recommendations.
  (copilot-disable-predicates '(er--point-is-in-comment-p))
  (copilot-idle-delay 1.5)
  :ensure t)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
