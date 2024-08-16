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


(provide 'init-write)
;;; init-write.el ends here

