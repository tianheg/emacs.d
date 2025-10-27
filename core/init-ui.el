(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package nerd-icons
  :ensure t
  :when (display-graphic-p)
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-dired
  :ensure t
  :after nerd-icons
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-modification-icon nil
        doom-modeline-project-detection 'project
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-unicode-fallback t))

(use-package emacs
  :ensure nil
  :bind (("C--" . text-scale-decrease)
         ("C-=" . text-scale-increase)
         ("C-0" . text-scale-adjust))
  :config
  (set-face-attribute 'default nil :family "Sarasa Term SC Nerd" :height 165)
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
   '(org-level-2 ((t (:inherit outline-1 :height 1.2))))
   '(org-level-3 ((t (:inherit outline-1 :height 1.0))))
   '(org-level-4 ((t (:inherit outline-1 :height 1.0)))))
  )

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(global-hl-line-mode +1)

(use-package ansi-color
  :ensure t
  :hook (compilation-filter . ansi-color-compilation-filter))

(provide 'init-ui)
