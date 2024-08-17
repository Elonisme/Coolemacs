;;; init-prog --- 编程模块
;;; commentary:
;;; Code:

;; 安装和配置 Python 模式
(use-package python-mode
  :ensure t
  :config
  (setq python-indent-offset 4))

;; 安装和配置 Black（代码格式化）
(use-package blacken
  :ensure t
  :hook
  (python-mode . blacken-mode)
  :config
  (setq blacken-allow-py36 t)) ;; 允许 Python 3.6+

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

(provide 'init-prog)
;;; init-prog.el ends here

