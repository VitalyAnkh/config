;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "VitalyR"
      user-mail-address "vitalyankh@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "mononoki" :size 22 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Noto Serif CJK SC Light" :size 24)
      doom-unicode-font (font-spec :family "Noto Serif CJK SC Light" :size 22)
      doom-big-font (font-spec :family "Noto Serif CJK SC" :size 24))

(set-fontset-font t 'unicode "Symbola" nil 'prepend)
(push "Noto Serif CJK SC" doom-unicode-extra-fonts)

(setq-default auto-fill-function 'do-auto-fill)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/projects/learn/Notebook/org")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(after! rustic
  (setq rustic-lsp-server 'rust-analyzer)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  (setq lsp-rust-analyzer-proc-macro-enable t)
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints t)
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-rust-all-features t)
  ;; (setq lsp-rust-full-docs t)
  (setq lsp-enable-semantic-highlighting t))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


(setq +latex-viewers '(pdf-tools))
(setq pdf-view-use-scaling t
      pdf-view-use-imagemagick nil
      pdf-view-resize-factor 10)
(setq-default TeX-engine 'xetex
              TeX-PDF-mode t)

;; (use-package auctex
;;   :hook ((LaTeX-mode-hook . visual-line-mode)
;; 	 (LaTeX-mode-hook . turn-on-reftex))
;;   :custom ((TeX-master nil)
;; 	   (TeX-auto-save t)
;; 	   (TeX-parse-self t)
;; 	   (reftex-plug-into-AUCTeX t)
;; 	   (TeX-command-list '(("LaTeX"
;; 				"%`xelatex%(mode)%' %t"
;; 				TeX-run-command nil t
;; 				:help "Run XeLaTeX")
;; 			       ("Tectonic" "tectonic %t"
;; 				TeX-run-command nil t
;; 				:help "Run tectonic")))
;; 	   (TeX-view-program-selection '((output-pdf "pdf-tools")))
;; 	   (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
;; 	   (TeX-source-correlate-mode t)
;; 	   (TeX-source-correlate-start-server t)))

(use-package pdf-tools
  :config
  (setq-default pdf-view-display-size 'fit-width)
  )

(setq TeX-source-correlate-start-server t)
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)
;;(setq-default preview-scale-function 1)
;; preview-scale-function and preview-scale has no effect in the size of the
;; preview image

(setq-default preview-default-document-pt 30)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)


(add-hook 'doom-first-file-hook #'auto-image-file-mode)
(auto-image-file-mode 1)

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-show-graph)
  :init
  (setq org-roam-directory org-directory)
  ;;(setq org-roam-graph-viewer "inkscape")
  (map! :leader
        :prefix "n"
        :desc "Org-Roam-Insert" "i" #'org-roam-insert
        :desc "Org-Roam-Find" "/" #'org-roam-find-file
        :desc "Org-roam-Buffer" "r" #'org-roam
        :desc "Org-Roam-Show-Graph" "g" #'org-roam-show-graph
        )
  :config
  (org-roam-mode t)
  (require 'org-roam-protocol) ;; require org-roam-protocol here
  )
(setq org-roam-server-host "127.0.0.1"
      org-roam-server-port 8080
      org-roam-server-authenticate nil
      org-roam-server-label-truncate t
      org-roam-server-label-truncate-lenght 60
      org-roam-server-label-wrap-length 20)
;; auto start org roam server
;; (add-hook 'org-mode #'(lambda () (org-roam-server-mode 1)))


(setq default-input-method "rime")
(setq rime-user-data-dir "~/sdk/config/input_method/rime")
(setq rime-show-candidate 'posframe)
(setq rime-disable-predicates
      '(rime-predicate-evil-mode-p
        rime-predicate-after-alphabet-char-p ;; 当光标处于紧挨着字母的位置时，自动由中文切换为英文
        rime-predicate-prog-in-code-p
        ))
(setq rime-posframe-properties
      (list :font "sarasa ui sc"
            :internal-border-width 10))

(setq rime--popup 1)
(setq rime-show-preedit 1)
(setq rime-posframe-fixed-position t)

(use-package! valign
  :init
  (require 'valign)
  :hook
  ('org-mode . #'valign-mode))

(require 'kana)

(setq-hook! 'LaTeX-mode-hook +spellcheck-immediately nil)

(require 'org)

(use-package org-latex-impatient
  :defer t
  :hook (org-mode . org-latex-impatient-mode)
  :init
  (setq org-latex-impatient-tex2svg-bin "tex2svg")
  (setq org-latex-impatient-scale 2)
  (setq org-latex-impatient-delay 0.01)
  )

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1))
;;(setq org-format-latex-options (plist-put org-format-latex-options :background "default"))

(setq org-preview-latex-default-process 'dvisvgm)

(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

(require 'deft)
(setq deft-directory org-directory)

(use-package sis
  :hook
  ;; enable the /follow context/ and /inline region/ mode for specific buffers
  (((text-mode prog-mode) . sis-context-mode)
   ((text-mode prog-mode) . sis-inline-mode))


  :config
  (sis-ism-lazyman-config "1" "2" 'fcitx5)
  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /follow context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t)
  )

;; (require 'nox)
;; (dolist (hook (list
;;                'html-mode-hook
;;                'css-mode-hook
;;                'js-mode-hook
;;                'json-mode-hook
;;                ;;'rust-mode-hook
;;                'python-mode-hook
;;                'ruby-mode-hook
;;                'java-mode-hook
;;                'sh-mode-hook
;;                'tex-mode-hook
;;                'php-mode-hook
;;                'c-mode-common-hook
;;                'c-mode-hook
;;                'csharp-mode-hook
;;                'c++-mode-hook
;;                'haskell-mode-hook
;;                ))
;;  (add-hook hook '(lambda () (nox-ensure))))


;; garbage collection for org-roam
(setq org-roam-db-gc-threshold most-positive-fixnum)

(add-hook 'prog-mode-hook #'wucuo-start)
(add-hook 'text-mode-hook #'wucuo-start)

;; to speed up company
(setq company-idle-delay 0)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-opera-light t)

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
;;(transwin-toggle-transparent-frame)

(use-package company-tabnine
  :ensure t)
(require 'company-tabnine)
(add-to-list 'company-backends #'company-tabnine)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)
;; use aspell as ispell backend
(setq-default ispell-program-name "aspell")
;; use American English as ispell default dictionary
(ispell-change-dictionary "american" t)
;;
;; no stack
;; I hate stack
;; I hate virtual environment
(setq
 ghc-ghc-options '("-fno-warn-missing-signatures")
 haskell-interactive-popup-errors nil
 )

;; When time comes, I will use emacs-eaf again
;; (require 'eaf)
;; (use-package eaf
;;   :load-path "~/usr/share/emacs/site-lisp/eaf" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
;;   :custom
;;   (eaf-find-alternate-file-in-dired t)
;;   :config
;;   (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key take_photo "p" eaf-camera-keybinding))

(add-to-list 'load-path "/home/vitalyr/.opam/default/share/emacs/site-lisp")
(require 'ocp-indent)
(setq word-wrap-by-category t)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; add this after install emacs-libvterm-git from AUR
;; (require 'vterm)
(setq org-image-actual-width nil)
(setq-default org-download-image-dir (concat org-directory "/.attach/pictures"))
(use-package! org-xournal
  :hook (org-mode . org-xournal-mode)
  :config
  (setq org-xournal-note-dir "~/nutstore_files/Notebook/xournalpp"  ;; xopp 笔记存储目录
        org-xournal-template-dir "~/nutstore_files/Notebook/xournalpp/templates" ;; xournal 目标文件存储目录
        org-xournal-default-template-name "template.xopp" ;; 默认笔记模版名称，应该位于 org-xournal-template-dir
        org-xournal-bin "xournalpp" ;; xournal 执行文件
        )
  )
(use-package! org-krita
  :config
  (add-hook 'org-mode-hook 'org-krita-mode))

(require 'quickrun)
(quickrun-add-command "c++/c1z"
  '((:command . "g++")
    (:exec    . ("%c -std=c++1z %o -o %e %s"
		 "%e %a"))
    (:remove  . ("%e")))
  :default "c++")

(load "/home/vitalyr/.opam/default/share/emacs/site-lisp/tuareg-site-file")


;; 学习如何在emacs中读取$DOOMDIR这个环境变量后，重构module-dir的定义，用$DOOMDIR
;; 展开而不是硬编码
;; (defvar modules-dir (expand-file-name "modules" user-emacs-directory)
;;   "The directory contains all modules.")
(defvar modules-dir "~/sdk/config/emacs/doom.d-vitalyr/modules"
  "The directory contains all modules")
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(let* ((variable-tuple
        (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground
                             ,base-font-color))))

(custom-theme-set-faces
 ;; not using default, let doom handle it
 ;;'(default ((t (:family "mononoki" :foundry "nil" :slant normal :weight light :height 141 :width normal))))
 'user
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
 '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin))))
 '(fixed-pitch ((t (:family "mononoki" :height 170))))
 )
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(add-to-list 'load-path modules-dir)
;;(require 'quelpa-use-package)
;;(require 'prelude)
;;(require 'prelude-org)
;;(require 'prelude-ui)
