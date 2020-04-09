;;; package ---  My local config
;;; Commentary:


;;; Code:
(setq-default TeX-PDF-mode t)
(set-default 'preview-default-document-pt 30)
(setq-default TeX-engine 'xetex)
(set-default 'preview-scale-function 3)
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

(menu-bar-mode 1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "FantasqueSansMono Nerd Font" :foundry "PfEd" :slant normal :weight normal :height 162 :width normal)))))

;;; (load "preview-latex.el" nil t t)
(provide 'init-local)
;;; init-local.el ends here
