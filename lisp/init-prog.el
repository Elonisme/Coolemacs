;;; init-prog --- 编程模块
;;; commentary:
;;; Code:

;; 安装和配置 Python 模式
(use-package python-mode
  :ensure t
  :config
  (setq python-indent-offset 4))


;; 安装并配置 CIDER，这是一个用于 Clojure 的强大交互式开发环境。
(use-package cider
  :ensure t                     ;; 确保安装 cider 包，如果未安装则自动安装。
  :config
  (setq nrepl-log-messages t))  ;; 设置为记录 nREPL 消息日志，帮助调试和查看 REPL 输出。

;; 安装 clojure-mode，这是一种用于 Clojure 代码的主要编辑模式，提供语法高亮和缩进功能。
(use-package clojure-mode
  :ensure t)                    ;; 确保安装 clojure-mode 包。


;; 安装和配置 Black（代码格式化）
(use-package blacken
  :ensure t
  :hook
  (python-mode . blacken-mode))

;; 安装和配置 pytest
(use-package pytest
  :ensure t
  :commands (pytest pytest-current-buffer)
  :bind (("C-c t" . pytest-current-buffer)))

;; 运行的代码文件
(use-package quickrun
  :ensure t
  :bind
  ("C-c C-r" . quick-run)  ;; 绑定快捷键以快速运行代码
  :config
  (setq quick-run-focus-p nil) ;; 运行时不切换到结果窗口
  )

;; conda配置
(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "/opt/anaconda"))
  (setq conda-env-home-directory (expand-file-name "/opt/anaconda"))
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  (conda-env-autoactivate-mode t)
  (setq conda-env-default-location "/opt/anaconda/condabin/conda")
  )

;; 安装和配置 dap-mode
(use-package dap-mode
  :ensure t
  :config
  (require 'dap-python)
  (setq dap-python-executable "python3")
  (setq dap-python-debugger 'debugpy))

;; 启用 eglot 和 dap-mode
(defun my-python-setup ()
  "Custom Python setup for eglot and dap-mode."
  (eglot-ensure)
  (dap-mode 1)
  (dap-ui-mode 1))

(add-hook 'python-mode-hook 'my-python-setup)

(provide 'init-prog)
;;; init-prog.el ends here

