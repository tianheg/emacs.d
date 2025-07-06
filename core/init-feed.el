(use-package elfeed
  :bind ("<f2> e" . elfeed)
  :ensure t
  :init
  (setq elfeed-use-curl t)
  (setq elfeed-search-filter "@1-months-ago +unread")
  (setq elfeed-sort-order 'descending)
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-search-title-min-width 30)
  (setq elfeed-search-trailing-width 25)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-show-unique-buffers t)
  (setq elfeed-search-date-format '("%F %R" 16 :left))
  :config
  (setq elfeed-feeds
        '(("https://cestlaz.github.io/rss.xml" emacs)
          ("https://manateelazycat.github.io/feed.xml" emacs)
          ("https://emacstalk.codeberg.page/index.xml" emacs)
          ("https://planet.emacslife.com/atom.xml" emacs)
          ("https://sachachua.com/blog/feed/index.xml" emacs)
          ("https://bearblog.dev/discover/feed/?type=rss" life)
          ))
  )

(provide 'init-feed)
