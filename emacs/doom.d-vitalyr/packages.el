;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))
;
(package! kana :recipe (:host github :repo "chenyanming/kana"))
(package! org-roam-server :recipe (:host github :repo "org-roam/org-roam-server"))
(package! rime :recipe (:host github :repo "DogLooksGood/emacs-rime"))
(package! valign :recipe (:host github :repo "casouri/valign"))
(package! org-latex-impatient :recipe (:host github :repo "yangsheng6810/org-latex-impatient"))
(package! smart-input-source :recipe (:host github :repo "laishulu/emacs-smart-input-source"))
(package! exec-path-from-shell :recipe (:host github :repo "purcell/exec-path-from-shell"))
(package! quickrun :recipe (:host github :repo "emacsorphanage/quickrun"))
(package! wucuo :recipe (:host github :repo "redguardtoo/wucuo"))
(package! maple-run :recipe (:host github :repo "VitalyAnkh/emacs-maple-run"))
(package! doom-themes :recipe (:host github :repo "hlissner/emacs-doom-themes"))
(package! transwin :recipe (:host github :repo "jcs-elpa/transwin"))
(package! company-tabnine :recipe (:host github :repo "TommyX12/company-tabnine"))
(package! org-roam-bibtex :recipe (:host github :repo "org-roam/org-roam-bibtex"))
;; no nox any more
;;(package! nox :recipe(:host github :repo "manateelazycat/nox"))
(unpin! org-roam)
(unpin! org-roam-server)
(unpin! rime)
