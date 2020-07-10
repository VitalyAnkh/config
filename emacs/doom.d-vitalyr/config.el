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
(setq doom-font (font-spec :family "mononoki" :size 45 :weight 'light)
                 doom-variable-pitch-font (font-spec :family "Noto Sans CJK SC Light" :size 40))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

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
  (setq lsp-rust-full-docs t)
  (setq lsp-enable-semantic-highlighting t))
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


(setq +latex-viewers '(pdf-tools))
(setq pdf-view-use-scaling t)
(setq pdf-view-resize-factor 1.5)
(setq-default TeX-engine 'xetex
              TeX-PDF-mode t)
(use-package pdf-tools
  :config
  (setq-default pdf-view-display-size 'fit-width)

  )
(setq TeX-source-correlate-start-server t)
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)
;;(setq-default preview-scale-function 2)
;; preview-scale-function and preview-scale has no effect in the size of the
;; preview image

(setq-default preview-default-document-pt 5)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)

(add-hook 'doom-first-file-hook #'auto-image-file-mode)
;;(auto-image-file-mode 1)

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
;;(setq rime-show-candidate 'posframe)
(setq rime-disable-predicates
      '(rime-predicate-evil-mode-p
        rime-predicate-after-alphabet-char-p ;; 当光标处于紧挨着字母的位置时，自动由中文切换为英文
        rime-predicate-prog-in-code-p
        ))
;; (setq rime-posframe-properties
;;  (list :font "sarasa ui sc"
;;        :internal-border-width 10))
(setq rime--popup 1)
(setq rime-show-preedit 1)
;;(setq rime-posframe-fixed-position t)

(use-package! valign
  :init
  (require 'valign)
  :hook
  ('org-mode . #'valign-mode))
(require 'kana)
(setq-hook! 'LaTeX-mode-hook +spellcheck-immediately nil)
(require 'org)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 4.0))
(setq org-preview-latex-default-process 'dvisvgm)

(use-package org-latex-instant-preview
  :defer t
  :hook (org-mode . org-latex-instant-preview-mode)
  :init
  (setq org-latex-instant-preview-tex2svg-bin
        ;; location of tex2svg executable
        "tex2svg"))

(org-roam-server-mode)

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(require 'deft)
(setq deft-directory org-directory)

;; (use-package smart-input-source
;;   :init
;;   ;; set the english input source
;;   ;;(setq smart-input-source-english
;;    ;;     "com.apple.keylayout.US")

;;   ;; set the default other language input source for all buffer
;;   ;;(setq-default smart-input-source-other
;;    ;;             "com.sogou.inputmethod.sogou.pinyin")

;;   :config
;;   ;; Input source specific cursor color
;;   (defvar original-cursor-background nil)
;;   (add-hook 'smart-input-source-set-english-hook
;;             (lambda ()
;;               (when original-cursor-background
;;                 (set-cursor-color original-cursor-background))))
;;   (add-hook 'smart-input-source-set-other-hook
;;             (lambda ()
;;               (unless original-cursor-background
;;                 (setq original-cursor-background
;;                       (or (cdr (assq 'cursor-color default-frame-alist))
;;                           (face-background 'cursor)
;;                           "Red")))
;;               (set-cursor-color "green")))

  ;; (push 'YOUR-COMMAND smart-input-source-preserve-save-triggers)

  ;; enable the /respect/ mode
;;   (smart-input-source-global-respect-mode t)

;;   ;; enable the /follow context/ and /inline english/ mode for all buffers
;;   (smart-input-source-global-follow-context-mode t)
;;   (smart-input-source-global-inline-english-mode t)

;;   ;; enable the /follow context/ and /inline english/ mode for specific buffers
;;   ;; :hook
;;   ;; (((text-mode prog-mode) . smart-input-source-follow-context-mode)
;;   ;;  ((text-mode prog-mode) . smart-input-source-inline-english-mode))
;;   )

;; (require 'subr-x)
;; (setq smart-input-source-external-ism "fcitx5-remote")
;; (setq smart-input-source-english "1")
;; (setq-default smart-input-source-other "2")
;; (setq smart-input-source-do-get
;;       (lambda()
;;         (string-trim
;;          (shell-command-to-string
;;           smart-input-source-external-ism))))
;; (setq smart-input-source-do-set
;;       (lambda(source)
;;         (pcase source
;;           ("1" (string-trim (shell-command-to-string
;;                              (concat smart-input-source-external-ism " -c"))))
;;           ("2" (string-trim (shell-command-to-string
;;                              (concat smart-input-source-external-ism " -o")))))))


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
(setq haskell-process-type 'cabal-new-repl)
(add-hook 'prog-mode-hook #'wucuo-start)
(add-hook 'text-mode-hook #'wucuo-start)
