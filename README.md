# Installed Packages as of Feb 23, 2017

```
ls | grep -o "^[a-z\-]*[a-z]"
```

ace-jump-mode
alert
archives
async
autopair
avy
avy-readme
bind-key
buffer-move
cider
cider-decompile-readme
cider-readme
cl-generic
cl-generic
clj-refactor
clj-refactor-readme
clojure-mode
coffee-mode
company
company-ghc
company-restclient
cython-mode
dash
diminish
edn
ein
epl
exec-path-from-shell
exec-path-from-shell-readme
expand-region
flycheck
flycheck-pyflakes-readme
flycheck-readme
ghc
git-commit
gntp
gnupg
haskell-mode
haskell-mode-readme
hayoo
helm
helm-ag
helm-core
helm-projectile
highlight-parentheses-readme
hindent
hydra
inflections
intero
intero-readme
jade-mode
js
json-mode
json-reformat
json-snatcher
know-your-http-well
log
magit
magit-popup
magit-readme
markdown-mode
markdown-mode-readme
monokai-theme
move-text
multiple-cursors
multi-web-mode
neotree
ob-restclient
org-pomodoro
paredit
peg
pkg-info
popup
projectile
puppet-mode
puppet-mode-readme
px
py-autopep
queue
rainbow-delimiters
request
restclient
restclient-helm
rich-minority
s
seq
seq
shakespeare-mode
shm-readme
smart-mode-line
smartscan
spinner
spinner
stylus-mode
sws-mode
undo-tree
use-package
web-mode
websocket
with-editor
yaml-mode
yaml-mode-readme
yasnippet
yasnippet-readme



emacs.d
=======

My Emacs

* LaTeX
as of <2015-01-05 Mon> this is to allow support for LaTeX in org-mode using

** =sudo apt-get install texlive texlive-latex-extra texlive-fonts-recommended=
*** you may also need imagemagick (or dvipng, but imagemagick is apparently better), and ghostscript
** C-c C-x C-l : preview latex
** C-c C-c : remove preview

* Multiple Cursors
** https://github.com/magnars/multiple-cursors.el
** C-S-<click> : place multiple cursors
** <enter> : escape multiple cursors mode
** C-j : carriage return while in m.c. mode
