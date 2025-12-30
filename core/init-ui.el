;;; init-ui.el --- UI configuration -*- lexical-binding: t -*-

;;; Code:

;; --- Theme ---
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; --- Icons ---
(use-package nerd-icons
  :ensure t
  :when (display-graphic-p)
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-dired
  :ensure t
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

;; --- Modeline ---
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-modification-icon nil
        doom-modeline-project-detection 'project
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-unicode-fallback t))

;; --- Fonts & Basic UI ---
(use-package emacs
  :ensure nil
  :bind (("C--" . text-scale-decrease)
         ("C-=" . text-scale-increase)
         ("C-0" . text-scale-adjust))
  :config
  (set-face-attribute 'default nil :family "Sarasa Term SC Nerd" :height 165)
  (global-hl-line-mode +1)
  (blink-cursor-mode t)
  (setq-default cursor-type 'bar)
  (setq visible-cursor nil)
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b")))))

;; --- Visual Aids ---
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ansi-color
  :ensure t
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package display-fill-column-indicator
  :ensure nil
  :custom
  (display-fill-column-indicator-column 120)
  (display-fill-column-indicator-character ?\u2502))

;; --- Time & Battery ---
(setq display-time-default-load-average nil
      display-time-format "%H:%M")
(display-time-mode t)
(display-battery-mode 1)

;; --- Scrolling ---
(setq frame-resize-pixelwise t)
(pixel-scroll-precision-mode 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      scroll-step 1)

(provide 'init-ui)
;;; init-ui.el ends here
