# Thing to do when setting up a new desktop computer

## config system source mirror site
* for archlinux/manjaro: tuna
* for ubuntu:

## download the necessary softwares, packages, SDKs
1. just run setup-new-desktop.sh
2. check the uninstalled things and install them mannually

## config some uneful thing for the packages
* vscode: install config sync extesnsion and download packages from it
* emacs: sync emacs.d/init.el from git repo
* haskell: add stackage and cabal mirror site from tuna
* agda: install agda from agda git repo, cd to the path and
run `stack --stack-yaml=sttack-version-yaml install`, the `version` in the command is the version of your ghc version
* agda-stdlib: just run set-up-agda-stdlib.sh


## some tips
* cabal-1.4: This version of 'cabal-install' does not support the 'uninstall'
operation. It will likely be implemented at some point in the future; in the
meantime you're advised to use either 'ghc-pkg unregister PACKAGE_NAME' or
'cabal sandbox hc-pkg -- unregister PACKAGE_NAME'.