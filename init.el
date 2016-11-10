;;; init.el --- Summary
;;; Commentary:
;;; -the above 2 lines are to appease flycheck, linter
;;;  someday I'll figure out what to do with them

;;;; Organization Tips
;; (require 'my-appearance)
;; (require 'my-settings)
;; (require 'my-defuns)
;; (require 'my-keys)
;; (require 'my-eshell)
;; (require 'my-emms)
;; (require 'my-org)


;; Add MELPA package archives
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )


;; THEME
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" default)))
 '(safe-local-variable-values
   (quote
    ((cider-cljs-lein-repl . "(do (dev) (go) (cljs-repl))")
     (cider-refresh-after-fn . "reloaded.repl/resume")
     (cider-refresh-before-fn . "reloaded.repl/suspend")
     (hamlet/basic-offset . 4)
     (haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'monokai t)


;; highlight the buffer details if focused
;; allows for finding the focused buffer quicker
(set-face-attribute  'mode-line
                 nil 
                 :foreground "gray90"
                 :background "gray35" 
                 :box '(:line-width 1 :style released-button))
(set-face-attribute  'mode-line-inactive
                 nil 
                 :foreground "gray30"
                 :background "gray10"
                 :box '(:line-width 1 :style released-button))

;; TODO: this is incomplete
(defvar my-packages '(clojure-mode
                      ;; cider-mode
                      ;; smart-mode-line
                      ;; helm
                      ;; helm-ag
                      ;; avy
                      cider
                      paredit
                      rainbow-delimiters
                      linum
                      yasnippet
                      smartscan))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


(require 'clojure-mode)
;(require 'cider-mode)
(require 'cider)

;;;; Helm-AG
;; I think persistent means that helm stays open after you take an action
;; (custom-set-variables '(helm-follow-mode-persistent nil)) 

;; Open Helm AG Previews in Temp Buffer (so they don't pollute my
;; buffer list
;; Solution found on: https://github.com/syohex/emacs-helm-ag/issues/152
(custom-set-variables '(helm-ag-use-temp-buffer t))

;; Toggle Line Wrapping
(global-set-key (kbd "C-c w") 'toggle-truncate-lines)

;; Magit Shorcut
(global-set-key (kbd "C-c g") 'magit-status)

;; smart-scan
(global-smartscan-mode t)
(global-set-key (kbd "M-N") 'smartscan-symbol-go-forward)
(global-set-key (kbd "M-P") 'smartscan-symbol-go-backward)

;; Yasnippets!
(when (require 'yasnippet nil 'noerror)
  (progn
    (yas/load-directory "~/.emacs.d/snippets")))
(yas-global-mode 1)

;; HideShow
(add-hook 'hs-minor-mode-hook
          (lambda ()
            (local-set-key (kbd "C-=") 'hs-toggle-hiding)
            (local-set-key (kbd "C-+") 'hs-show-all)
            (local-set-key (kbd "C-_") 'hs-hide-all)))

;; Scroll screen, don't move point
;; adapted from: http://stackoverflow.com/questions/8993183/emacs-scroll-buffer-not-point-
(define-key smartscan-map (kbd "M-n") nil) ; remove conflicting bindings
(define-key smartscan-map (kbd "M-p") nil)
(global-set-key "\M-n" (lambda () (interactive) (scroll-up 4)))
(global-set-key "\M-p" (lambda () (interactive) (scroll-down 4)))


;; Splash Screen
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; Switch to Mini Buffer
;; useful, ex, when you start to find a file,
;; you navigate to a different window, and then when you
;; navigate back to emacs, you need the mouse to get to
;; the mini buffer.
;; http://superuser.com/questions/132225/how-to-get-back-to-an-active-minibuffer-prompt-in-emacs-without-the-mouse
(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))
(global-set-key (kbd "C-c m") 'switch-to-minibuffer-window)



;; turn off: scroll bar, tool bar, menu bar
(scroll-bar-mode -1)
(tool-bar-mode -1)
;(menu-bar-mode -1)

;; Display Settings - show file name
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; navigate windows with S-<arrow>
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(global-set-key (kbd "C-S-<left>") 'buf-move-left)
(global-set-key (kbd "C-S-<right>") 'buf-move-right)
(global-set-key (kbd "C-S-<up>") 'buf-move-up)
(global-set-key (kbd "C-S-<down>") 'buf-move-down)



;;display line numbers (slows down if 10k+ lines
(global-linum-mode 1)

;; replace selection
(delete-selection-mode 1)

;; white space
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Backup Files - disable
(setq make-backup-files nil) 

;; Yes and No - 'y' counts as 'yes', 'n' counts as 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Key Bindings - that don't really belong anywhere else
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
;(global-set-key (kbd "C-+") 'text-scale-increase)
;(global-set-key (kbd "C--") 'text-scale-decrease)

;; select-click region
(define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill)

;; Ido - easier buffer nav
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; multiple cursors
(require 'cl) ; required for Multiple-Cursors, otherwise `equalp not found` error
(require 'multiple-cursors)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; Smex - easier M-x nav
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Unset C-z, for safety (in a terminal, this closes out of everything)
(global-set-key "\C-z" nil)
(global-set-key "\C-x\C-z" nil)

;; move-text
;; M-S-<up> move text/region up

;; expand-region semantically
(global-set-key (kbd "C-@") 'er/expand-region)


;; undo tree - STRUGGLES WITH MULTIPLE CURSORS
;; (require 'undo-tree)
;; (global-undo-tree-mode 1)
;; (defalias 'redo 'undo-tree-redo)
;; (defalias 'redo 'undo-tree-undo)
;; (global-set-key (kbd "C-z") 'undo)
;; (global-set-key (kbd "C-S-z") 'redo)
;; C-x u  (`undo-tree-visualize')
;;   Visualize the undo tree.

;; neotree - for file navigation


;; Avy (I switched from Ace-Jump mode)
;;   notice the non-standard key bindings adopted from Ace-Jump
(global-set-key (kbd "C-c SPC") 'avy-goto-word-1)
(global-set-key (kbd "C-c C-c SPC") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-line)

(setq avy-background t)
(setq avy-keys (number-sequence ?a ?z)) ; `(?a ?s ?d ?f ?j ?k ?l ?q ?w ?e ?r ?n ?m ?u ?i)

;; TODO: replace with Avy
;; ;; ace-jump : navigate quickly inside a buffer's text by jumping the cursor to desired locations
;; (require 'ace-jump-mode)
;; (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; C-c C-c is compile, can't use that
;;(define-key global-map (kbd "C-c C-c SPC") 'ace-jump-line-mode)
;;(define-key global-map (kbd "C-c C-c C-c SPC") 'ace-jump-char-mode)

;; Transparency
;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
(set-frame-parameter (selected-frame) 'alpha '(98 90))
;;(add-to-list 'default-frame-alist '(alpha 85 50))

;; set up for iPython (instead of python)
(require 'python)
(setq
  python-shell-interpreter "ipython"
  python-shell-interpreter-args "--pylab"
  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
  python-shell-completion-setup-code
    "from IPython.core.completerlib import module_completion"
  python-shell-completion-module-string-code
    "';'.join(module_completion('''%s'''))\n"
  python-shell-completion-string-code
    "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")


;; Underscores are part of words
(modify-syntax-entry ?_ "w")

;; scroll one line at a time (less "jumpy" than defaults)
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
;; (setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
;; (setq-default smooth-scroll-margin 0)
;; (setq scroll-step 1
;;       scroll-margin 1
;;       scroll-conservatively 100000)

;; multiple cursors stuff
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Avy
(avy-setup-default) ; allows a C-' in the middle of a C-s

;; TODO
;; config files
;; certs for laptop (github)

;; TO ADD
;; C-d : multicursor select the next like this
;; move lines/regions up and down by keyboard
;; save buffer state on close
;; multi-cursors buffer
;; more intuitive 'find'
;; minimap / sublimity
;; C-<backspace> is kinda wierd with white space
;; comments spread out wierd



;; Open Octave files (*.m) in octave-mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))


;; multi-web-mode : for editing html/js/web stuff
;; (require 'multi-web-mode)
;; (setq mweb-default-major-mode 'html-mode)
;; (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;;                   (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
;;                   (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
;; (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
;; (multi-web-global-mode 1)


(require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(setq-default indent-tabs-mode nil)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq-default tab-width 4)
(setq jade-tab-width 4)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; Markdown - disabled because I think emacs knows to load markdown-mode anyways
;; (autoload 'markdown-mode "markdown-mode"
;;   "Major mode for editing Markdown files" t)
;; (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))



;; React Stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; taken from: http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(setq web-mode-enable-auto-quoting nil);; nil means false

(setq web-mode-content-types-alist
  '(("jsx" . "\\.js[x]?\\'")));;set content-type to "jsx" instead of "js"

(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-code-indent-offset 4)
            (setq web-mode-markup-indent-offset 4)
            (setq web-mode-attr-indent-offset 4)))

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)



;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; ERC (IRC Chat Client) ------------------------------
(defun irc ()
    "Connect to the freenode"
    (interactive)
    (erc :server "irc.freenode.net"
         :port 6667
         :nick "joshfreck"
         :password nil))
(global-set-key "\C-ci"  'irc)
;;(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))
(setq erc-hide-list '("JOIN" "PART" "QUIT"))



;; clojure ------------------------------

(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(setq exec-path (append exec-path '("~/bin")))
(setenv "PATH" (concat (getenv "PATH") ":~/bin"))
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

;; when cursor sits on a paren, highlight it's pair
(show-paren-mode 1)
(setq show-paren-delay 0)

;; disable paredit's default kbd's for slurping/barfing
;; use insteadone of: C-S-[ (, ), {, } ]
;; this may not be necessary if that binding already is globally bound elsewhere
;; (eval-after-load "paredit-mode"
;;   '(define-key paredit-mode-map (kbd "C-S-<right>") nil))
;; (eval-after-load "paredit-mode"
;;   '(define-key paredit-mode-map (kbd "C-S-<left>") nil))


;; prevent hippie-expand from expanding lines
;; this is especially problematic when trying to
;; expand a line in paredit-mode, and h.e. adds
;; mismatched parens
(dolist (f '(try-expand-line try-expand-list))
  (setq hippie-expand-try-functions-list
        (remq f hippie-expand-try-functions-list)))



(setq clojure-align-forms-automatically t)

;; clojure-mode: indentation + alignment
(define-clojure-indent ; for `core.match`
  (match 1))
(add-to-list 'clojure-align-cond-forms "match") ; for `core.match`

;; clojure-mode: ace-jump compatibility
(define-key clojure-mode-map (kbd "C-c SPC") nil) ; I like this key combo for `ace-jump`
(define-key clojure-mode-map (kbd "M-Q") #'clojure-align)

;; clojure-mode: cider-eval-next-sexp
(defun cider-eval-next-sexp ()
  ;; useful where I'm working in `(defn f ...)` and want to evan the following `(f ...)`
  (interactive)
  (save-excursion
    (setq go-up t)
    (while go-up
      (condition-case nil ; go forward-up as far as you can
          (paredit-forward-up)
        (error (setq go-up nil))))
    (paredit-forward) ; go to the next sexp
    (cider-eval-last-sexp) ;eval it
    ))
(define-key cider-mode-map (kbd "C-c C-v") nil) ; overwrite cider's use of C-c C-v
(global-set-key (kbd "C-c C-v") 'cider-eval-next-sexp)
;;123456789ABCDEFGH

;; Transpose adjacent sexps (esp. for paredit),
;;   eg (a | b) -> (b | a)
(defun reverse-transpose-sexps (arg)
  ;; http://emacs.stackexchange.com/a/12800
  (interactive "*p")
  (transpose-sexps (- arg)))
(global-set-key (kbd "C-M-(") 'reverse-transpose-sexps)
(global-set-key (kbd "C-M-)") 'transpose-sexps)


;;;; Clojure Figwheel-Cider nREPL Config stuff
;;     https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl

;; For (reset)ing a Component-based "Reloaded Workflow"
;; adapted from:
;; https://github.com/stuartsierra/dotfiles/blob/d0d1c46ccc4fdd8d2add363615e625cc29d035b0/.emacs#L307-L312

;; find all buffers names which match `reg`, regex
(defun find-buffer-regex (reg)
  (interactive)
  (remove-if-not #'(lambda (x) (string-match reg x))
                 (mapcar #'buffer-name (buffer-list))))

(defun cider-execute (command)
  (interactive)
  (set-buffer (car (find-buffer-regex "cider-repl.*")))
  (goto-char (point-max))
  (insert command)
  (cider-repl-return))

(defun nrepl-reset ()
  (interactive)
  (cider-execute "(reset)"))
(define-key cider-mode-map (kbd "C-c r") 'nrepl-reset)

(defun nrepl-start-dev ()
  (interactive)
  (cider-execute "(dev) (go)"))
(define-key cider-mode-map (kbd "C-c s") 'nrepl-start-dev)


;; I commented this because `cider`'s tests are better than `duct`'s
;; (defun nrepl-test ()
;;   (interactive)
;;   (cider-execute "(test)"))
;; (define-key cider-mode-map (kbd "C-c t") 'nrepl-test)


;; CLJ-Refactor, overwrite Cider's C-c C-m
(require 'clj-refactor)
(defun my-clj-refactor-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  (cljr-add-keybindings-with-prefix "C-c C-m"))
(add-hook 'clojure-mode-hook #'my-clj-refactor-hook)

;; for coordination between figwheel and CIDER
(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

;;;; CIDER stuff ------------------------------
;; http://cider.readthedocs.io/en/latest/using_the_repl/

;; eldoc-mode : display function sigs in minibuffer
(add-hook 'cider-repl-mode-hook #'eldoc-mode)

;; enlighten-mode : display values in repl during evaluation
;;   NOTE: this is noisy, instead, preface a form which you want
;;   "enlightened" with `#light`
;; (add-hook 'cider-repl-mode-hook #'cider-enlighten-mode)

;; don't switch to repl buffers when CIDER boots
(setq cider-repl-pop-to-buffer-on-connect nil)

;; when using `C-c C-z` to switch to REPL buffer,
;; do it in the same window
(setq cider-repl-display-in-current-window t)

;; wrap CIDER stack traces
(setq cider-stacktrace-fill-column 80)

;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)


;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; save system clipboard strings to the kill ring
;; so that if I copy from, say, Chrome, and then
;; kill in emacs before pasting, I can still
;; access Chrome's copied text
(setq save-interprogram-paste-before-kill t)




;;; init.el ends here
