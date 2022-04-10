(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval ignore-errors
           (require 'whitespace)
           (whitespace-mode 1))
     (whitespace-line-column . 79)
     (whitespace-style face indentation)
     (eval progn
           (c-set-offset 'case-label '0)
           (c-set-offset 'innamespace '0)
           (c-set-offset 'inline-open '0)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-buffer-modified ((t (:foreground "orange"))))
 '(markdown-header-face-1 ((t (:height 1.25 :inherit markdown-header-face))))
 '(markdown-header-face-2 ((t (:height 1.15 :inherit markdown-header-face))))
 '(markdown-header-face-3 ((t (:height 1.08 :inherit markdown-header-face))))
 '(markdown-header-face-4 ((t (:height 1.0 :inherit markdown-header-face))))
 '(markdown-header-face-5 ((t (:height 0.9 :inherit markdown-header-face))))
 '(markdown-header-face-6 ((t (:height 0.75 :inherit markdown-header-face))))
 '(org-document-title ((t (:height 1.3))))
 '(org-quote ((t (:family "CMU Typewriter Text" :height 1.05))))
 '(outline-1 ((t (:height 1.25))))
 '(outline-2 ((t (:height 1.15))))
 '(outline-3 ((t (:height 1.12))))
 '(outline-4 ((t (:height 1.09))))
 '(outline-5 ((t (:height 1.06))))
 '(outline-6 ((t (:height 1.03))))
 '(outline-8 ((t (:height 1.01)))))
