;;; init-basic --- 基础的配置
;;; Commentary:

;;; code:

;; 矩阵编辑
(use-package multiple-cursors
  :ensure t
  )

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


(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  (historian-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-historian-mode 1)
  :bind
  (("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

(use-package amx
  :ensure t
  :init (amx-mode))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . 'ace-window)))                    

;; 安装和配置 Swiper
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper) ;; 替代默认的搜索
         ("C-r" . swiper) ;; 反向搜索
         ("C-c C-r" . swiper-thing-at-point)) ;; 搜索当前位置的内容
  :diminish
  :config
  (setq swiper-action-recenter t)) ;; 确保匹配项居中显示

;; 启用 yasnippet 的全局模式
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; 重启 emacs
(use-package restart-emacs
  :ensure t)

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-preview-current nil)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind ("M-/" . completion-at-point)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)))

(unless (display-graphic-p)
  (use-package corfu-terminal
    :ensure t
    :hook (global-corfu-mode . corfu-terminal-mode)))


(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  )

(use-package vertico
  :ensure t
  :custom (vertico-count 15)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package nerd-icons-completion
  :ensure t
  :hook (vertico-mode . nerd-icons-completion-mode))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package org-side-tree
  :ensure t)

(use-package ivy-historian
  :ensure t
  )

;; 安装 EMMS
(use-package emms
  :ensure t
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-vlc))
  (setq emms-info-functions '(emms-info-native))
  )


(use-package ivy-emms
  :ensure t)

(provide 'init-tool)
;;; init-basic.el ends here
