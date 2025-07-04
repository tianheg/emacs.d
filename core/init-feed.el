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
        '(("https://2d2d.io/feed.xml" it)
          ("https://blog.codingnow.com/atom.xml" blog_cn)
          ("https://cestlaz.github.io/rss.xml" emacs)
          ("https://cprss.s3.amazonaws.com/golangweekly.com.xml" weekly)
          ("https://cprss.s3.amazonaws.com/weekly.statuscode.com.xml" weekly)
          ("https://frostming.com/feed.xml" python)
          ("https://github.blog/all.atom" github)
          ("https://jimmysong.io/blog/index.xml" cloudnative)
          ("https://manateelazycat.github.io/feed.xml" emacs)
          ("https://emacstalk.codeberg.page/index.xml" emacs)
          ("https://moelove.info/index.xml" cloudnative)
          ("https://skyao.io/index.xml" blog_cn)
          ("https://willschenk.com/feed.xml" dev)
          ("https://www.cncf.io/feed/" cloudnative)
          ("https://www.flysnow.org/index.xml" blog_cn)
          ("https://www.morling.dev/blog/index.xml" blog_en)
          ("https://www.qikqiak.com/index.xml" docker kubernetes)
          ("https://kubernetes.io/feed.xml" cloudnative kubernetes)
          ("https://planet.emacslife.com/atom.xml" emacs)
          ("https://hechao.li/feed.xml" blog_en)
          ("https://iximiuz.com/feed.rss" blog_en)
          ("https://www.mikeperham.com/index.xml" blog_en)
          ("https://mrkaran.dev/atom.xml" blog_en)
          ("https://www.brendangregg.com/blog/rss.xml" blog_en)
          ("https://www.raychase.net/feed" blog_cn)
          ("https://coolshell.cn/feed" blog_cn)
		  ("https://threedots.tech/index.xml" go)
          ))
  )

(provide 'init-feed)
