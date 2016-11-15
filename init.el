;;; init.el --- Summary
;;; Commentary:
;;; -the above 2 lines are to appease flycheck, linter
;;;  someday I'll figure out what to do with them

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MELPA

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEME

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blerg t)
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" default)))
 '(helm-ag-use-temp-buffer t)
 '(helm-source-names-using-follow nil)
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
(set-face-attribute 'mode-line
                 nil 
                 :foreground "gray90"
                 :background "gray35" 
                 :box '(:line-width 1 :style released-button))
(set-face-attribute 'mode-line-inactive
                 nil 
                 :foreground "gray30"
                 :background "gray10"
                 :box '(:line-width 1 :style released-button))

;; Smart Mode Line (changes appearance of bottom of frame)
(setq sml/theme 'respectful)
(rich-minority-mode 1) ; comes with Smart Mode Line
(add-to-list 'rm-whitelist "") ; don't show any minor modes
(sml/setup)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install required Packages if missing

;; ;; TODO: this is incomplete
;; (defvar my-packages '(
;;                       clojure-mode
;;                       ;; cider-mode
;;                       ;; smart-mode-line
;;                       ;; helm
;;                       ;; helm-ag
;;                       ;; projectile
;;                       ;; helm-projectile
;;                       ;; avy
;;                       cider
;;                       paredit
;;                       rainbow-delimiters
;;                       linum
;;                       yasnippet
;;                       smartscan))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm Mode

;; I think persistent means that helm stays open after you take an action
;; (custom-set-variables '(helm-follow-mode-persistent nil)) 

(require 'helm)
(require 'helm-ag)
(require 'projectile)
(require 'helm-projectile)

(helm-mode t)

;; Open Helm AG Previews in Temp Buffer (so they don't pollute my
;; buffer list)
;; Solution found on: https://github.com/syohex/emacs-helm-ag/issues/152

(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-S") 'helm-occur) ; TODO: helm-swoop instead?
(setq helm-follow-mode-persistent t)

;; Helm projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart-Scan
(global-smartscan-mode t)
(global-set-key (kbd "M-N") 'smartscan-symbol-go-forward)
(global-set-key (kbd "M-P") 'smartscan-symbol-go-backward)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

;; Toggle Line Wrapping
(global-set-key (kbd "C-c w") 'toggle-truncate-lines)

;; Scroll screen, don't move point
;; adapted from: http://stackoverflow.com/questions/8993183/emacs-scroll-buffer-not-point-
(define-key smartscan-map (kbd "M-n") nil) ; remove conflicting bindings
(define-key smartscan-map (kbd "M-p") nil)
(global-set-key "\M-n" (lambda () (interactive) (scroll-up 8)))
(global-set-key "\M-p" (lambda () (interactive) (scroll-down 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-Mode

(add-hook 'org-mode-hook
          (lambda ()
            ;; for Avy
            (local-unset-key (kbd "C-c SPC"))

            ;; for WindMove
            (local-unset-key (kbd "S-<up>"))
            (local-unset-key (kbd "S-<down>"))
            (local-unset-key (kbd "S-<left>"))
            (local-unset-key (kbd "S-<right>"))

            ;; for Buf Move
            (local-unset-key (kbd "C-S-<up>"))
            (local-unset-key (kbd "C-S-<down>"))
            (local-unset-key (kbd "C-S-<left>"))
            (local-unset-key (kbd "C-S-<right>"))

            ;; Org-Pomodoro
            (local-set-key (kbd "<f12>") 'org-pomodoro)))

;; Pomodoro Mode - (org-pomodoro)
;;   libnotify binding, adapted from: https://www.reddit.com/r/emacs/comments/5ayjjl/pomodoro_in_emacs/
;; (use-package org-pomodoro
;;   :ensure t
;;   :commands (org-pomodoro)
;;   :config
;;     (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))))

(setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit Shorcut
(global-set-key (kbd "C-c g") 'magit-status)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippets

 (when (require 'yasnippet nil 'noerror)
   (progn
     (yas/load-directory "~/.emacs.d/snippets")))
 (yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HideShow
(add-hook 'hs-minor-mode-hook
          (lambda ()
            (local-set-key (kbd "C-=") 'hs-toggle-hiding)
            (local-set-key (kbd "C-+") 'hs-show-all)
            (local-set-key (kbd "C-_") 'hs-hide-all)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Specific

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
;; (global-set-key (kbd "C-;") 'comment-or-uncomment-region) ; TODO Remove this if nothing broke, I'm using `M-;` instead

;; Unset C-z, for safety (in a terminal, this closes out of everything)
(global-set-key "\C-z" nil)
(global-set-key "\C-x\C-z" nil)

;; Window transparency
(set-frame-parameter (selected-frame) 'alpha '(98 90))

;; Underscores are part of words
(modify-syntax-entry ?_ "w")

;; save system clipboard strings to the kill ring
;; so that if I copy from, say, Chrome, and then
;; kill in emacs before pasting, I can still
;; access Chrome's copied text
(setq save-interprogram-paste-before-kill t)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
;;   NOTE: helm does better
;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WindMove, BufMove

;; TODO: get it to work with Org-mode and Markdown mode

;; navigate windows with S-<arrow>
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(global-set-key (kbd "C-S-<left>") 'buf-move-left)
(global-set-key (kbd "C-S-<right>") 'buf-move-right)
(global-set-key (kbd "C-S-<up>") 'buf-move-up)
(global-set-key (kbd "C-S-<down>") 'buf-move-down)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippie Expand

(global-set-key (kbd "M-/") 'hippie-expand)

;; prevent hippie-expand from expanding lines
;; this is especially problematic when trying to
;; expand a line in paredit-mode, and h.e. adds
;; mismatched parens
(dolist (f '(try-expand-line try-expand-list))
  (setq hippie-expand-try-functions-list
        (remq f hippie-expand-try-functions-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expand Region

(global-set-key (kbd "C-@") 'er/expand-region)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple-Cursors

(require 'cl) ; required for Multiple-Cursors, otherwise `equalp not found` error
(require 'multiple-cursors)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Avy (I switched from Ace-Jump mode)

(global-set-key (kbd "C-c SPC") 'avy-goto-word-1)
(global-set-key (kbd "C-c C-c SPC") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-line)

;; config
(setq avy-background t)
(setq avy-keys (number-sequence ?a ?z)) ; `(?a ?s ?d ?f ?j ?k ?l ?q ?w ?e ?r ?n ?m ?u ?i)

;(avy-setup-default) ; allows a C-' in the middle of a C-s

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python

;; set up for iPython (instead of python)
(require 'python)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Octave

;; Open Octave files (*.m) in octave-mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web Mode

(require 'web-mode)
(setq-default indent-tabs-mode nil)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq-default tab-width 4)
(setq jade-tab-width 4)


;; React Stuff
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FlyCheck

;; TODO: remove this, I turned it off cuz it's annoying

;; (require 'flycheck)

;; ;; turn on flychecking globally
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; ;; disable jshint since we prefer eslint checking
;; (setq-default flycheck-disabled-checkers
;;   (append flycheck-disabled-checkers
;;     '(javascript-jshint)))

;; ;; use eslint with web-mode for jsx files
;; (flycheck-add-mode 'javascript-eslint 'web-mode)

;; ;; disable json-jsonlist checking for json files
;; (setq-default flycheck-disabled-checkers
;;   (append flycheck-disabled-checkers
;;     '(json-jsonlist)))


;; TODO: delete me?
;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERC (IRC Chat Client)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure

;; Company - autocompletion for CIDER
;;   use `C-M-i`
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(setq company-idle-delay nil) ; never start completions automatically


;; TODO : I'm using CIDER's `compnay` instead
;; ;; AC-Cider, autocomplete
;; (require 'ac-cider)
;; (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
;; (add-hook 'cider-mode-hook 'ac-cider-setup)
;; (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
;; (eval-after-load "auto-complete"
;;   '(progn
;;      (add-to-list 'ac-modes 'cider-mode)
;;      (add-to-list 'ac-modes 'cider-repl-mode)))

;; (defun set-auto-complete-as-completion-at-point-function ()
;;   (setq completion-at-point-functions '(auto-complete)))

;; (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
;; (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

(require 'clojure-mode)
(require 'cider-mode) ;; TODO: what's the diff with `cider` and `cider-mode`?
(require 'cider)

;; CIDER functions to remember
;;   M-.         : jump to symbol's definition
;;   M-,         : jump to pre-jump location, leaves old buffer open
;;   C-c M-t v   : trace a function
;;   C-c C-d C-d : doc for symbol at point
;;   C-C C-b     : interrupt
;;   C-M-i       : auto complete
;;   C-c M-i     : inspect symbol
;;   M-x cider-browse-ns
;;   M-x cider-classpath
;;   <the stack trace ones> - https://cider.readthedocs.io/en/latest/navigating_stacktraces/
;;   <debugger>             - https://cider.readthedocs.io/en/latest/debugging/

(setq exec-path (append exec-path '("~/bin")))
(setenv "PATH" (concat (getenv "PATH") ":~/bin")) ; TODO : why do I have this line?

(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'hs-minor-mode)

(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

;; when cursor sits on a paren, highlight it's pair
(show-paren-mode 1)
(setq show-paren-delay 0)

(setq clojure-align-forms-automatically t)

;; clojure-mode: indentation + alignment
(define-clojure-indent ; for `core.match` alignment
  (match 1))
(add-to-list 'clojure-align-cond-forms "match")

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

;;;; CIDER stuff
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







;;; init.el ends here
