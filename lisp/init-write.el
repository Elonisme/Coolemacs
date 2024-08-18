;;; init-write -- 写作模块
;;; commentary:

;;; code:

;; 安装 markdown-mode
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" "\\.markdown\\'")
  :commands (markdown-mode gfm-mode)
  :config
  (setq markdown-command "pandoc") ;; 可选：使用 pandoc 作为Markdown 渲染工具
  ;; 配置 Markdown 快捷键
  (bind-key "C-c C-c" 'markdown-command markdown-mode-map)
  
  ;; 自定义 Markdown 编辑器行为
  (setq markdown-fontify-code-blocks-natively t) ;; 高亮代码块
  (setq markdown-enable-math t) ;; 启用数学公式支持
  (setq markdown-hide-markup t)) ;; 隐藏标记符

;; 安装 markdown-preview-mode
(use-package markdown-preview-mode
  :ensure t
  :commands markdown-preview
  :config
  (setq markdown-preview-stylesheets
        '("~/.emacs.d/css/github-markdown.css"))
  )

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-item-bullet-alist '((?- . ?•) (?* . ?•) (?+ . ?◦))))  ; 使用不同符号美化列表


(use-package org
  :ensure t
  :hook
  ((org-mode . visual-line-mode)
	 (org-mode . my/org-prettify-symbols))
  :commands (org-find-exact-headline-in-buffer org-set-tags)
  :custom-face
  ;; 设置Org mode标题以及每级标题行的大小
  (org-document-title ((t (:height 1.75 :weight bold))))
  (org-level-1 ((t (:height 1.2 :weight bold))))
  (org-level-2 ((t (:height 1.15 :weight bold))))
  (org-level-3 ((t (:height 1.1 :weight bold))))
  (org-level-4 ((t (:height 1.05 :weight bold))))
  (org-level-5 ((t (:height 1.0 :weight bold))))
  (org-level-6 ((t (:height 1.0 :weight bold))))
  (org-level-7 ((t (:height 1.0 :weight bold))))
  (org-level-8 ((t (:height 1.0 :weight bold))))
  (org-level-9 ((t (:height 1.0 :weight bold))))
  :config
  ;; set default image width
  (setq org-image-actual-width '(400))

  ;; Improve org mode looks
  (setq-default ;;org-startup-indented t
                org-pretty-entities t
                org-use-sub-superscripts "{}"
                org-hide-emphasis-markers t
                org-startup-with-inline-images t
                org-image-actual-width '(300))

  (setq org-startup-with-latex-preview t)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (setq org-hide-emphasis-markers t)
  (setq org-latex-packages-alist '(("" "ctex"))) ; Load the ctex package

  ;; use xelatex to produce Chinese PDF in org mode
  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; display chinese math funtion
  (add-to-list
   'org-preview-latex-process-alist
   '(xdvsvgm
     :progams
     ("xelatex" "dvisvgm")
     :discription "xdv > svg"
     :message
     "you need install the programs: xelatex and dvisvgm."
     :image-input-type "xdv"
     :image-output-type "svg"
     :image-size-adjust (2 . 2)
     :latex-compiler
     ("xelatex -no-pdf -shell-escape -output-directory=%o %f")
     :image-converter ("dvisvgm %f -n -b min -c %S -o %O")))

  ;; set defalut preview process
  (setq org-preview-latex-default-process 'xdvsvgm)

  ;; 文学编程
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((latex . t)  ;; 启用 Latex 支持
     (python . t)  ;; 启用 Python 支持等
     (clojure . t)  ;; 启用 Clojure 支持
     (emacs-lisp . t)  ;; 启用 Emacs Lisp 支持
     ))


  ;; 在org mode里美化字符串
  (defun my/org-prettify-symbols ()
    (interactive)
	  (setq prettify-symbols-alist
		      (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
				          '(
					          ("#+begin_src"      . 9998)         ; ✎
					          ("#+end_src"        . 9633)         ; □
					          ("#+results:"       . 9776)         ; ☰
					          ("#+attr_latex:"    . "🄛")
					          ("#+attr_html:"     . "🄗")
					          ("#+attr_org:"      . "🄞")
					          ("#+name:"          . "🄝")         ; 127261
					          ("#+caption:"       . "🄒")         ; 127250
					          ("#+date:"          . "📝")         ; 128197
					          ("#+author:"        . "💁")         ; 128100
					          ("#+setupfile:"     . 128221)       ; 📝
					          ("#+email:"         . 128231)       ; 📧
					          ("#+startup:"       . 10034)        ; ✲
					          ("#+options:"       . 9965)         ; ⛭
					          ("#+title:"         . 10162)        ; ➲
					          ("#+subtitle:"      . 11146)        ; ⮊
					          ("#+downloaded:"    . 8650)         ; ⇊
					          ("#+language:"      . "α")          ; α
                    (":PROPERTIES:"      . 9998)        ; «
                    (":ID:"  . 8943)                    ; ⋯
                    (":END:"    . 9633)                 ; ⋯
					          )))
    (setq prettify-symbols-unprettify-at-point t)
	  (prettify-symbols-mode 1))
  )


(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autoentities t)
  (setq org-appear-autokeywords t)
  (setq org-appear-inside-latex t)
  )




(use-package org-fragtog
  :ensure t
)


(use-package org-auto-tangle
  :ensure t
  )


;; 使用 visual-fill-column 让文本居中并限制文本宽度，提升可读性
(use-package visual-fill-column
  :ensure t
  :hook (org-mode . visual-fill-column-mode)
  :config
  (setq visual-fill-column-width 80
        visual-fill-column-center-text t))

;; 粘贴图片到org mode
(use-package org-download
  :ensure t
  :defer t ;; 延迟加载
  :bind
  (:map org-mode-map
        ("C-M-y" . org-download-clipboard)) ;; 绑定从剪贴版粘贴截图的快捷键
  :custom
  (org-download-heading-lvl 1) ;; 用一级标题给截图文件命名
  :config
  (setq-default org-download-image-dir "./imgs")) ;; 用同级 ./img 目录放置截图文件
(add-hook 'dired-mode-hook 'org-download-enable)

;; 显示latex公式
(use-package org-fragtog
  :ensure t
  :hook
  (org-mode . org-fragtog-mode)
  )

(use-package org-auto-tangle
  :ensure t
  :hook
  (org-mode . org-auto-tangle-mode)
  )

(use-package denote
  :ensure t
  :config
  ;; 你的 Denote 配置
  (setq denote-directory "~/Documents/OrgMode") ;; 设置笔记存储目录
  (setq denote-date-format "%Y-%m-%d") ;; 设置日期格式
  (setq denote-file-type 'org) ;; 设置笔记文件类型（可选：'md、'org）
  (setq denote-file-name-format "%Y-%m-%d-%s")) ;; 设置文件名格式



(provide 'init-write)
;;; init-write.el ends here

