;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package org
  :hook ((org-mode . visual-line-mode))
  :custom
  (org-startup-truncated nil))

(use-package org-agenda
  :after org
  :bind ("C-c a" . org-agenda)
  :custom
  (org-agenda-files '("~/Documents/daily/"))  ;; Adjust the path to your org files
  (org-agenda-prefix-format '((agenda . " %i %-20:c%?-12t%-6e% s")
                              (todo . " %i %-20:c %-6e")
                              (tags . " %i %-20:c")
                              (search . " %i %-20:c")))
  (org-agenda-custom-commands '(("d" "Today's Tasks"
	((tags-todo
	  "TEST+PRIORITY=\"A\""
	  ((org-agenda-files '("~/Documents/daily/2025.org"))
	   (org-agenda-overriding-header "Primary goals this month")))
	 (agenda "" ((org-agenda-span 1)
		     (org-agenda-overriding-header "Today")))))

  )))

(use-package org-ql
  :ensure t)

(provide 'init-org)
