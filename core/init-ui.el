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

(use-package dashboard
  :ensure t
  :after (nerd-icons)
  :init
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  :config
  (setq dashboard-projects-backend 'project-el
        dashboard-items '((recents . 8)
                          (agenda . 8)
						  )
        dashboard-banner-logo-title "你今天快乐了吗？"
        dashboard-footer-messages '("https://github.com/tianheg/emacs.d")
        dashboard-image-banner-max-height 160
        dashboard-set-navigator t
        dashboard-set-footer nil
        dashboard-show-shortcuts nil)
  (setq dashboard-agenda-tags-format 'ignore
        dashboard-agenda-sort-strategy '(priority-down)
        dashboard-week-agenda t)
  (setq dashboard-navigator-buttons
        `((;; homepage
           (,(nerd-icons-octicon "nf-oct-home" :height 1.0 :v-adjust 0.0)
            "Homepage"
            "Go to homepage"
            (lambda (&rest _) (browse-url "https://tianheg.co/")))
           ;; Github
           (,(nerd-icons-octicon "nf-oct-mark_github" :height 1.0 :v-adjust 0.0)
            "Github"
            "Go to github"
            (lambda (&rest _) (browse-url "https://github.com/tianheg")))
           )))
  (dashboard-setup-startup-hook))

(use-package ansi-color
  :ensure t
  :hook (compilation-filter . ansi-color-compilation-filter))

(provide 'init-ui)
