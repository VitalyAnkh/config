(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(network-security-level 'low)
 '(org-cliplink-transport-implementation 'url-el)
 '(package-selected-packages
   '(evil-test-helpers evil elsa package-lint wgrep hydra avy dash)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; not using default, let doom handle it
 ;;'(default ((t (:family "mononoki" :foundry "nil" :slant normal :weight light :height 141 :width normal))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
 '(org-block-begin-line ((t (:extend t :background "#f7e0c3" :foreground "gray"
                             :weight semi-bold :height 151 :family "CMU Typewriter Text"))))
 ;;'(org-code ((t (:foreground "#957f5f" :family "Latin Modern Mono"))))
 '(org-document-title ((t (:foreground "midnight blue" :weight bold :height 2.0))))
 '(org-hide ((t (:foreground "#E5E9F0" :height 0.1))))
 ;;'(org-level-1 ((t (:inherit outline-1 :foreground "#076678" :weight extra-bold
 ;;                   :height 1.5 :family "CMU Typewriter Text"))))
 ;;'(org-level-2 ((t (:inherit outline-2 :foreground "#b57614" :height 1.2 :family
 ;;                   "CMU Typewriter Text"))))
 '(org-level-8 ((t (,@headline ,@variable-tuple))))
 '(org-level-7 ((t (,@headline ,@variable-tuple))))
 '(org-level-6 ((t (,@headline ,@variable-tuple))))
 '(org-level-5 ((t (,@headline ,@variable-tuple))))
 '(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
 '(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
 '(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
 '(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
 '(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))
 '(org-list-dt ((t (:foreground "#7382a0"))))
 ;;'(org-verbatim ((t (:foreground "#81895d" :family "Latin Modern Mono"))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 ;; TODO set the color following this
 ;;'(org-block ((t (:extend t :background "#f7e0c3" :foreground "#5b5143" :family "Latin Modern Mono"))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 ;;'(variable-pitch ((t (:family "Georgia"))))
 '(variable-pitch ((t (:family "Alegreya" :height 180 :weight thin))))
 '(fixed-pitch ((t (:family "mononoki" :height 170))))
 )
