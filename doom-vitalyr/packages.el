(unpin! yasnippet)
(unpin! citar)
(unpin! citar-org-roam)
(unpin! auto-yasnippet)
(unpin! emacsql)
(unpin! magit-gitflow)
(unpin! magit-popup)
(unpin! magit-todos)
(unpin! magit)
(unpin! popwin)
(unpin! ace-window)
(unpin! persp-mode)
(unpin! avy)
(unpin! vertico)
(unpin! closql)
(unpin! centaur-tabs)
(unpin! consult)
(unpin! compat)
(unpin! marginalia)
(unpin! straight)
(unpin! embark)
(unpin! centaur-tabs)
(unpin! lsp-metals)
(unpin! elisp-tree-sitter)
(unpin! git-gutter)
(unpin! code-review)
(unpin! haskell-mode)
(package! treemacs :pin "aa0944a29eee48302fd76b6c3a59c5aece114fa6")
(unpin! lsp-haskell)
(unpin! lsp-java)
(unpin! ox-hugo)
(unpin! lsp-mode)
(unpin! dap-mode)
(package! multiple-cursors :disable t)
(package! separate-inline :recipe
  (:host github :repo "ingtshan/separate-inline.el" :files ("*.el")))
(package! benchmark-init :recipe (:host github :repo "dholm/benchmark-init-el"))
(package! org-pretty-tags)
(package! yasnippet)
(package! org-xlatex :recipe
  (:host github :repo "ksqsf/org-xlatex" :files ("*.el" "*.html")))
(unpin! yaml)
(unpin! lsp-treemacs)
(package! magit :recipe (:branch "main"))
(package! ghub :recipe (:branch "main"))
(package! multiple-cursors :disable t)
;;(package! consult-recoll)
;; (package! emacs-nerd-icons
;;   :recipe (:type git
;;            :host github
;;            :repo "rainstormstudio/emacs-nerd-icons"
;;            :files ("*.el" "data")))

;; -*- no-byte-compile: t; -*-

(package! rotate)

;; (package! emacs-everywhere :recipe (:host github :repo "tecosaur/emacs-everywhere"))
;; (unpin! emacs-everywhere)

(package! vlf :recipe (:host github :repo "emacs-straight/vlf" :files ("*.el")))

(package! meow)

(package! copilot :recipe (:host github :repo "zerolfx/copilot.el" :files ("dist" "copilot.el" "copilot-balancer.el")))

;; (package! magit-delta :recipe (:host github :repo "dandavison/magit-delta") :pin "5fc7dbddcfacfe46d3fd876172ad02a9ab6ac616")

(package! aas :recipe (:host github :repo "ymarco/auto-activating-snippets"))

(package! screenshot :recipe (:host github :repo "tecosaur/screenshot"))

(package! etrace :recipe (:host github :repo "aspiers/etrace")
  )

(package! string-inflection)

(package! info-colors)

(package! modus-themes)

(package! spacemacs-theme)

(package! theme-magic)

;; (package! simple-comment-markup :recipe (:host github :repo "tecosaur/simple-comment-markup"))

(package! keycast)

(package! gif-screencast)

(package! page-break-lines :recipe (:host github :repo "purcell/page-break-lines"))

(package! xkcd)

(package! selectric-mode)

;; (package! wttrin :recipe (:host :local-repo "lisp/wttrin"))

(package! spray)

(package! elcord)

;; (package! systemd :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae")

(package! wakatime-mode)

(package! sis :recipe (:host github :repo "VitalyAnkh/emacs-smart-input-source"
                           :files ("*.el" ) :branch "staging"))

;;(package! rime)

(package! calibredb)

(package! nov)

(package! calctex :recipe (:host github :repo "johnbcoughlin/calctex"
                           :files ("*.el" "calctex/*.el" "calctex-contrib/*.el" "org-calctex/*.el" "vendor")))

(unpin! lsp-mode)

(package! lsp-bridge
  :recipe (:host github
           :repo "manateelazycat/lsp-bridge"
           :files ("*.el" "*.py" "acm" "core" "langserver"
                   "multiserver" "resources")))
(package! popon
  :recipe (:host nil :repo "https://codeberg.org/akib/emacs-popon.git"))
(package! acm-terminal
  :recipe (:host github :repo "twlz0ne/acm-terminal"))

(package! clang-format+
  :recipe (:host github :repo "SavchenkoValeriy/emacs-clang-format-plus"))

;;(package! lsp-grammarly)

(package! just-mode
  :recipe (:host github :repo "leon-barrett/just-mode.el"))

(package! lean4-mode :recipe
  (:host github
   :repo "leanprover/lean4-mode"
   :files ("data" "*.el")))

(package! meson-mode)

(package! llvm :recipe
  (:host github
   :repo "VitalyAnkh/llvm-tools"
   :files ("*.el")))
;; Use my own fork of the llvm-tools which located in the llvm-project monorepo

(package! sage-shell-mode)
(package! ob-sagemath)

(package! wgsl-mode :recipe
  (:host github
   :repo "acowley/wgsl-mode"
   :files ("wgsl-mode.el")))

(package! wat-mode :recipe (:host github :repo "devonsparks/wat-mode"))

nil
(unpin! org) ; there be bugs
(package! org-contrib
  :recipe (:host nil :repo "https://git.sr.ht/~bzg/org-contrib"
           :files ("lisp/*.el")))
(unpin! org-contrib)

(package! org-modern)

(package! org-appear :recipe (:host github :repo "awth13/org-appear"))

(package! org-ol-tree :recipe (:host github :repo "Townk/org-ol-tree"))

;; (package! ob-julia :recipe (:host github :repo "tecosaur/ob-julia" :files ("*.el" "julia")))

(package! ob-http)

(package! org-transclusion :recipe (:host github :repo "nobiot/org-transclusion"))

(package! org-graph-view :recipe (:host github :repo "alphapapa/org-graph-view"))

(package! org-chef)

(package! org-pandoc-import :recipe
  (:host github :repo "tecosaur/org-pandoc-import" :files ("*.el" "filters" "preprocessors")))

(package! org-glossary :recipe (:host github :repo "tecosaur/org-glossary"))

(package! orgdiff :recipe (:host github :repo "tecosaur/orgdiff"))

(package! org-music :recipe (:host github :repo "tecosaur/org-music"))

(package! org-cite-csl-activate :recipe (:host github :repo "andras-simonyi/org-cite-csl-activate"))

(package! org-super-agenda)

(package! doct
  :recipe (:host github :repo "progfolio/doct")
  )

(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(package! websocket) ; dependency of `org-roam-ui'

;; (package! org-pretty-tags :pin "5c7521651b35ae9a7d3add4a66ae8cc176ae1c76")

(package! engrave-faces :recipe (:host github :repo "tecosaur/engrave-faces"))

(package! ox-chameleon :recipe (:host github :repo "tecosaur/ox-chameleon"))

(package! ox-gfm)

(package! laas :recipe (:host github :repo "tecosaur/LaTeX-auto-activating-snippets"))

;; (package! pdftotext :recipe (:host github :repo "tecosaur/pdftotext"))

(package! conf-data-toml :recipe (:host github :repo "tecosaur/conf-data-toml"))

(package! graphviz-dot-mode)

(package! beancount :recipe (:host github :repo "beancount/beancount-mode"))
