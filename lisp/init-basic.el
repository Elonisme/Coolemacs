;;; init-basic --- 基础的配置
;;; Commentary:

;;; code:

;; 启用 electric-pair-mode
(electric-pair-mode 1)

;; 使用代理
(setq my-proxy "127.0.0.1:7890")

;; Configure network proxy
(defun show-proxy ()
  "Show http/https proxy."
  (interactive)
  (if url-proxy-services
      (message "Current proxy is \"%s\"" my-proxy)
    (message "No proxy")))

(defun set-proxy ()
  "Set http/https proxy."
  (interactive)
  (setq url-proxy-services `(("http" . ,my-proxy)
                             ("https" . ,my-proxy)))
  (show-proxy))

(defun unset-proxy ()
  "Unset http/https proxy."
  (interactive)
  (setq url-proxy-services nil)
  (show-proxy))

(defun toggle-proxy ()
  "Toggle http/https proxy."
  (interactive)
  (if url-proxy-services
      (unset-proxy)
    (set-proxy)))

(global-set-key (kbd "C-c p") 'toggle-proxy)


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


;; 自动补全
(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))
  :hook
  (
   (prog-mode . lsp-bridge-mode)
   (org-mode . lsp-bridge-mode)
   )
  :config
  (setq lsp-bridge-python-lsp-server 'pylsp)
  )


;; 配置 'yasnippet'
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-global-mode 1))

;; 配置预设 Snippets
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; 配置flycheck 的集成
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
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

(provide 'init-basic)
;;; init-basic.el ends here

