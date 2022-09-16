;; [[file:config.org::*Workaround][Workaround:2]]
(package! clang-format+
  :recipe (:host github :repo "SavchenkoValeriy/emacs-clang-format-plus"))
(package! haskell-mode
  :recipe (:host github :repo "VitalyAnkh/haskell-mode" :branch "fix_cl_case_error"))
(unpin! haskell-mode)
(package! lsp-haskell)
(package! all-the-icons :disable t)
;; There are some problems in Emacs 29 which make doom not download these packages
(package! f)
(package! dash)
(package! pkg-info)
(package! epl)
;; Workaround:2 ends here

;; -*- no-byte-compile: t; -*-

;; [[file:config.org::*Rotate (window management)][Rotate (window management):1]]
(package! rotate)
;; Rotate (window management):1 ends here

;; [[file:config.org::*Emacs Everywhere][Emacs Everywhere:1]]
;;(package! emacs-everywhere :recipe (:local-repo "lisp/emacs-everywhere"))
;;(unpin! emacs-everywhere)
;; Emacs Everywhere:1 ends here

;; [[file:config.org::*Very large files][Very large files:1]]
;; (package! vlf)
;; Very large files:1 ends here

;; [[file:config.org::*Meow][Meow:1]]
(package! meow)
;; Meow:1 ends here

;; [[file:config.org::*Copilot][Copilot:1]]
(package! copilot :recipe (:host github :repo "zerolfx/copilot.el" :files ("dist" "copilot.el")))
;; Copilot:1 ends here

;; [[file:config.org::*Corfu][Corfu:1]]
(package! corfu)
(package! orderless)
(package! kind-icon )
(package! cape :recipe (:host github :repo "minad/cape" :branch "main"))
(package! corfu-doc :recipe (:host github :repo "galeo/corfu-doc" :branch "main"))
;; Corfu:1 ends here

;; [[file:config.org::*Annotate][Annotate:1]]
(package! annotate)
;; Annotate:1 ends here

;; [[file:config.org::*\\\[\\\]agit delta][\[\]agit delta:2]]
(package! magit-delta :recipe (:host github :repo "dandavison/magit-delta"))
;; \[\]agit delta:2 ends here

;; [[file:config.org::*Don't use ~spell-fu~!][Don't use ~spell-fu~!:1]]
(disable-packages! spell-fu)
;; Don't use ~spell-fu~!:1 ends here

;; [[file:config.org::*Auto activating snippets][Auto activating snippets:1]]
(package! aas :recipe (:host github :repo "ymarco/auto-activating-snippets"))
;; Auto activating snippets:1 ends here

;; [[file:config.org::*Screenshot][Screenshot:1]]
;;(package! screenshot :recipe (:local-repo "lisp/screenshot"))
;; Screenshot:1 ends here

;; [[file:config.org::*Etrace][Etrace:1]]
(package! etrace :recipe (:host github :repo "aspiers/etrace"))
;; Etrace:1 ends here

;; [[file:config.org::*Etrace][Etrace:2]]
(use-package! etrace
  :after elp)
;; Etrace:2 ends here

;; [[file:config.org::*String inflection][String inflection:1]]
(package! string-inflection)
;; String inflection:1 ends here

;; [[file:config.org::*Info colours][Info colours:1]]
(package! info-colors)
;; Info colours:1 ends here

;; [[file:config.org::*Modus themes][Modus themes:1]]
(package! modus-themes)
;; Modus themes:1 ends here

;; [[file:config.org::*Nano heme][Nano heme:1]]
(package! nano-theme
  :recipe (:host github :repo "rougier/nano-theme"))
;; Nano heme:1 ends here

;; [[file:config.org::*Theme magic][Theme magic:1]]
(package! theme-magic)
;; Theme magic:1 ends here

;; [[file:config.org::*Keycast][Keycast:1]]
(package! keycast)
;; Keycast:1 ends here

;; [[file:config.org::*Screencast][Screencast:1]]
(package! gif-screencast)
;; Screencast:1 ends here

;; [[file:config.org::*Prettier page breaks][Prettier page breaks:1]]
(package! page-break-lines :recipe (:host github :repo "purcell/page-break-lines"))
;; Prettier page breaks:1 ends here

;; [[file:config.org::*xkcd][xkcd:1]]
(package! xkcd)
;; xkcd:1 ends here

;; [[file:config.org::*Selectric][Selectric:1]]
(package! selectric-mode)
;; Selectric:1 ends here

;; [[file:config.org::*Wttrin][Wttrin:1]]
;;(package! wttrin :recipe (:local-repo "lisp/wttrin"))
;; Wttrin:1 ends here

;; [[file:config.org::*Spray][Spray:1]]
(package! spray)
;; Spray:1 ends here

;; [[file:config.org::*Elcord][Elcord:1]]
(package! elcord)
;; Elcord:1 ends here

;; [[file:config.org::*Systemd][Systemd:1]]
(package! systemd)
;; Systemd:1 ends here

;; [[file:config.org::*LSP][LSP:1]]
(unpin! lsp-mode)
;; LSP:1 ends here

;; [[file:config.org::*Lean][Lean:1]]
(package! lean4-mode :recipe
  (:host github
   :repo "leanprover/lean4"
   :files ("lean4-mode/*.el")))
;; Lean:1 ends here

;; [[file:config.org::*Meson][Meson:1]]
(package! meson-mode)
;; Meson:1 ends here

;; [[file:config.org::*LLVM Tools][LLVM Tools:1]]
;; Use my own fork of the llvm-tools which located in the llvm-project monorepo
(package! llvm :recipe
  (:host github
   :repo "VitalyAnkh/llvm-tools"
   :files ("*.el")))
;; LLVM Tools:1 ends here

;; [[file:config.org::*Sage Math][Sage Math:1]]
(package! sage-shell-mode)
(package! ob-sagemath)
;; Sage Math:1 ends here

(package! org-mode :recipe (:host github :repo "emacs-straight/org-mode" :files ("*.el" "lisp/*.el" "etc") :pre-build (with-temp-file "org-version.el" (insert "(fset 'org-release (lambda () \"9.5\"))
" (format "(fset 'org-git-version (lambda () \"%s\"))
" (substring (shell-command-to-string "git rev-parse --short HEAD") 0 -1)) "(provide 'org-version)
")) :includes org) :pin nil)
(unpin! org-mode) ; there be bugs
(package! org-contrib
  :recipe (:host nil :repo "https://git.sr.ht/~bzg/org-contrib"
           :files ("lisp/*.el")))
(unpin! org-contrib)

(package! org-modern)

(package! org-appear :recipe (:host github :repo "awth13/org-appear"))

(package! valign :recipe (:host github :repo "casouri/valign"))

(package! org-ol-tree :recipe (:host github :repo "Townk/org-ol-tree"))

(package! ob-http)

(package! org-transclusion :recipe (:host github :repo "nobiot/org-transclusion"))

(package! org-graph-view :recipe (:host github :repo "alphapapa/org-graph-view"))

(package! org-chef)

;;(package! org-pandoc-import :recipe
;;  (:local-repo "lisp/org-pandoc-import" :files ("*.el" "filters" "preprocessors")))

(package! org-glossary
  :recipe (:host github :repo "tecosaur/org-glossary"))

;;(package! orgdiff :recipe (:local-repo "lisp/orgdiff"))

;;(package! org-music :recipe (:local-repo "lisp/org-music"))

(unpin! citar)
(package! citeproc)
(package! org-cite-csl-activate :recipe (:host github :repo "andras-simonyi/org-cite-csl-activate"))

(package! org-super-agenda)

(package! doct
  :recipe (:host github :repo "progfolio/doct"))

(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(unpin! org-roam)
(package! websocket) ; dependency of `org-roam-ui'

(package! seperate-inline :recipe
 (:host github :repo "ingtshan/separate-inline.el" :files ("*.el")))

(package! org-pretty-tags)

(package! xenops)

(package! org-fragtog)

(package! engrave-faces)

;;(package! ox-chameleon :recipe (:local-repo "lisp/ox-chameleon"))

(package! ox-gfm)

;; [[file:config.org::*LAAS][LAAS:1]]
(package! laas)
;; LAAS:1 ends here

;; [[file:config.org::*Graphviz][Graphviz:1]]
(package! graphviz-dot-mode)
;; Graphviz:1 ends here

;; [[file:config.org::*Beancount][Beancount:1]]
(package! beancount :recipe (:host github :repo "beancount/beancount-mode"))
;; Beancount:1 ends here

;; [[file:config.org::*wakatime][wakatime:1]]
(package! wakatime-mode)
;; wakatime:1 ends here

;; [[file:config.org::*Input Method][Input Method:1]]
(package! sis :recipe (:host github :repo "VitalyAnkh/emacs-smart-input-source"
                           :files ("*.el" ) :branch "staging"))
;; Input Method:1 ends here

;; [[file:config.org::*Use =emacs-rime=][Use =emacs-rime=:1]]
;;(package! rime)
;; Use =emacs-rime=:1 ends here

;; [[file:config.org::*Ebooks][Ebooks:1]]
;;(package! calibredb)
;; Ebooks:1 ends here

;; [[file:config.org::*Ebooks][Ebooks:2]]
;;(package! nov)
;; Ebooks:2 ends here

;; [[file:config.org::*CalcTeX][CalcTeX:1]]
(package! calctex :recipe (:host github :repo "johnbcoughlin/calctex"
                           :files ("*.el" "calctex/*.el" "calctex-contrib/*.el" "org-calctex/*.el" "vendor")))
;; CalcTeX:1 ends here
