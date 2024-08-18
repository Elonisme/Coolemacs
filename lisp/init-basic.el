;;; init-basic --- 基础的配置
;;; Commentary:

;;; code:



;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  )

;; 安装 all-the-icons
(use-package all-the-icons
  :ensure t)

;; 设置侧边目录
(use-package neotree
  :ensure t
  :bind
  (("C-c r" . neotree-refresh)   ;; 绑定 Ctrl+c r 来刷新 neotree
   ([f8] . neotree-toggle))      ;; 绑定 F8 键来打开/关闭 neotree
  :config
  (setq neo-smart-open t)        ;; 自动打开当前文件所在的目录
  (setq neo-theme 'icons)        ;; 使用图标主题（需要安装 'all-the-icons'）
  )

;; 快捷键提示
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5))


;; 增强的 M-x 和 minibuffer 完成
(use-package ivy
  :ensure t
  :diminish
  :init (ivy-mode)
  :config
  ;; 配置 Ivy
  (setq ivy-use-virtual-buffers t)  ;; 在 Ivy 列表中显示虚拟缓冲区
  (setq ivy-count-format "(%d/%d) ") ;; 显示匹配项计数
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))   ;; 使用更灵活的正则表达式

  ;; 启用默认键绑定
  (define-key ivy-minibuffer-map (kbd "C-r") 'ivy-previous-history-element)
  (define-key ivy-minibuffer-map (kbd "C-s") 'ivy-next-history-element)
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done))

;; 安装 amx
(use-package amx
  :ensure t)

;; 安装和配置 Counsel
(use-package counsel
  :ensure t
  :diminish
  :after ivy
  :bind (("M-x" . counsel-M-x)  ;; 替代默认的 M-x
         ("C-x C-f" . counsel-find-file)  ;; 替代默认的 find-file
         ("C-c g" . counsel-git)  ;; Git 文件搜索
         ("C-c k" . counsel-ag)  ;; 更强大的 ag 搜索
         ("C-c j" . counsel-imenu)) ;; Imenu 支持
  :config
  (setq counsel-find-file-at-point t))

;; 安装和配置 Swiper
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper) ;; 替代默认的搜索
         ("C-r" . swiper) ;; 反向搜索
         ("C-c C-r" . swiper-thing-at-point)) ;; 搜索当前位置的内容
  :diminish
  :config
  (setq swiper-action-recenter t)) ;; 确保匹配项居中显示


;; 安装并配置'eglot'
(use-package eglot
  :hook ((python-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp"))))

;; company 补全
(use-package company
  :hook ((prog-mode . company-mode)
         (org-mode . company-mode)
         )
  :config
  ;; 设置更灵活的补全前缀长度和延迟
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0
        company-tooltip-align-annotations t  ;; 对齐注释
        company-show-numbers t               ;; 显示编号
        company-tooltip-limit 10             ;; 补全菜单的最大条目数量
        company-dabbrev-downcase nil         ;; 保持补全大小写一致
        company-dabbrev-ignore-case nil)
  
  ;; 配置 backends
  (setq company-backends '((company-capf            ;; 补全 at point function，通常用于与 eglot 一起工作
                            company-files           ;; 文件路径补全
                            company-keywords        ;; 关键字补全
                            company-yasnippet)      ;; 代码段补全
                           (company-dabbrev-code    ;; 代码片段补全
                            company-dabbrev
                            company-etags            ;; 增加 etags 支持
                            company-semantic)))      ;; 增加 semantic 支持
  
  ;; company-files 配置
  (setq company-files-show-hidden t)                     ;; 显示隐藏文件
  (setq company-files-env-vars t)                        ;; 补全路径中的环境变量，如 $HOME

  ;; 配置 backend 的顺序和优先级
  (setq company-transformers '(company-sort-by-backend-importance)))

;; 增强补全排序和筛选
(use-package company-prescient
  :ensure t
  :after company
  :config
  (company-prescient-mode 1)
  (setq prescient-filter-method '(literal fuzzy)))  ;; 设置 prescient 的过滤方法


;; 提供上下文帮助
(use-package company-quickhelp
  :ensure t
  :hook (company-mode . company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay 0.5))  ;; 设置帮助显示延迟


;; AI 驱动的补全插件
(use-package company-tabnine
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-tabnine))


;; 美化 company 补全界面
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons ;; 使用 all-the-icons 图标
        company-box-max-candidates 50                     ;; 显示最多 50 个候选项
        company-box-backends-colors nil                    ;; 不同 backend 使用不同颜色
        company-box-scrollbar t                            ;; 启用滚动条
        company-box-doc-enable t                          ;; 启用文档显示
        company-box-color-icon nil                         ;; 关闭图标着色
        company-box-doc-max-width 60))                      ;; 设置文档最大宽度

 ;; 频率最高的补全选项
 (use-package company-statistics
   :ensure t
   :after company
   :config
   (company-statistics-mode 1))

;; 配置 'yasnippet'
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook
  ((prog-mode . yas-minor-mode)
   (org-mode . yas-minor-mode)
   )
  :config
  (yas-global-mode 1))

;; 配置预设 Snippets
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; 配置flycheck 的集成
(use-package flycheck
  :ensure t
  :hook
  (company-mode . flycheck-mode)
  :config
  (setq flycheck-checker 'python-flake8)
  )

;; 拼写检查配置
(use-package flyspell
  :ensure t
  :diminish flyspell-mode
  :hook ((text-mode . flyspell-mode) ;; 启用文本模式中的拼写检查
         (prog-mode . flyspell-prog-mode)) ;; 启用编程模式中的拼写检查
  :config
  ;; 配置拼写检查
  (setq ispell-program-name "aspell")  ;; 设置使用的拼写检查程序，常用的还有 "hunspell"
  (setq ispell-dictionary "english")   ;; 设置默认的字典
  (setq ispell-personal-dictionary "~/.emacs.d/.ispell"))  ;; 设置个人字典路径

;; 重启 emacs
(use-package restart-emacs
  :ensure t)

;; vterm shell
(use-package vterm
  :ensure t)

(provide 'init-basic)
;;; init-basic.el ends here

