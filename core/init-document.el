(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
		 ("\\.md\\'" . markdown-mode)
		 ("\\.markdown\\'" . markdown-mode)))

(use-package htmlize
  :ensure t
  :defer t)

;; https://emacs.stackexchange.com/questions/17710/use-package-with-config-to-set-variables
(use-package org
  :pin nongnu
  :mode (("\\.org\\'" . org-mode))
  :ensure org-contrib
  :bind (("M-[" . org-previous-visible-heading)
		 ("M-]" . org-next-visible-heading))
  :init
  (setq org-ellipsis " ▾"
        org-adapt-indentation nil
		org-log-done t
		org-src-tab-acts-natively nil
		org-pretty-entities t
		org-hide-emphasis-markers t
		org-startup-folded t
		org-startup-with-inline-images t
		org-image-actual-width '(1024)
        org-capture-templates nil)
  :config
  ;; GTD setting
  (require 'org-inlinetask)
  (setq org-todo-keywords
		'((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d@/!)" "DEPR(r@/!)")))
  (setq org-priority-faces
        '((?A :foreground "#ff6c6b" :weight bold)
          (?B :foreground "#98be65" :weight bold)
          (?C :foreground "#c678dd" :weight bold)))
  (setq org-todo-keyword-faces
    	'(;;("TODO" . "lime green")
          ("WAIT" . "darkgoldenrod")
          ("DONE" . "dark")
    	  ("DEPR" . "darkgrey")))
  (setq org-agenda-include-deadlines t
		org-agenda-include-diary nil
		org-agenda-compact-blocks t
		org-agenda-start-with-log-mode t
		org-agenda-start-on-weekday 1
		org-agenda-span 28
		org-deadline-warning-days 7
		org-agenda-skip-deadline-prewarning-if-scheduled t
		org-agenda-skip-scheduled-delay-if-deadline t
		org-agenda-skip-scheduled-if-done t
		org-agenda-skip-deadline-if-done t)
  (setq org-agenda-custom-commands
		'(("g" "global overview"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "* HIGH-PRIORITY:")))
			(tags "PRIORITY=\"B\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "* MEDIUM-PRIORITY:")))
			(tags "PRIORITY=\"C\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "* LOW-PRIORITY:")))))))

  (setq org-agenda-breadcrumbs-separator " -> "
    	org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
    	org-agenda-time-grid '((weekly today require-timed)
                               (800 1000 1200 1400 1600 1800 2000)
                               "---" "┈┈┈┈┈┈┈┈┈┈┈┈┈")
    	org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                   (todo . " %i %-12:c")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c")))
  (setq org-agenda-format-date (lambda (date) (concat "\n" (make-string (window-width) 9472)
                                                      "\n"
                                                      (org-agenda-format-date-aligned date))))
  (setq org-cycle-separator-lines 2)

  (require 'org-tempo)

  (require 'org-indent)

  (use-package ob-go
    :ensure t)

  ;; programming languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
	 (css . t)
	 (js . t)
	 (org . t)
	 (python . t)
	 (sed . t)
	 (sql . t)
	 (R . t)
	 (go . t)))
  )

(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-mode))

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :init
  (setq org-superstar-special-todo-items nil
		org-superstar-prettify-item-bullets t
        org-superstar-remove-leading-stars t)
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode))

(provide 'init-document)
