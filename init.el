;;;

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
 '(custom-safe-themes (quote ("64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" default)))
 '(safe-local-variable-values (quote ((hamlet/basic-offset . 4) (haskell-process-use-ghci . t) (haskell-indent-spaces . 4)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'monokai t)

;; Splash Screen
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; turn off: scroll bar, tool bar, menu bar
;; (scroll-bar-mode -1)
(tool-bar-mode -1)
;;(menu-bar-mode -1)

;; Display Settings - show file name
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; navigate windows with S-<arrow>
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

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
(require 'multiple-cursors)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; Smex - easier M-x nav
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Unset C-z, for safety (in a terminal, this closes out of everything)
;; note, further down I use C-z for "undo"
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

;; ace-jump : navigate quickly inside a buffer's text by jumping the cursor to desired locations
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; C-c C-c is compile, can't use that
;;(define-key global-map (kbd "C-c C-c SPC") 'ace-jump-line-mode)
;;(define-key global-map (kbd "C-c C-c C-c SPC") 'ace-jump-char-mode)

;; Transparency
;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
(set-frame-parameter (selected-frame) 'alpha '(98 94))
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


;; clojure
(require 'clojure-mode)
(require 'cider-mode)

(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(setq exec-path (append exec-path '("~/bin")))
(setenv "PATH" (concat (getenv "PATH") ":~/bin"))
(add-hook 'clojure-mode-hook 'paredit-mode)

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
;(define-key cider-mode-map (kbd "C-c C-v") nil) ; overwrite cider's use of C-c C-v
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

;; Clojure Figwheel-Cider nREPL Config stuff
;;   https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl

(require 'cider)
(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")


;;; init.el ends here
