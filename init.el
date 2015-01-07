;; Add MELPA package archives
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

;; THEME
(custom-set-variables
 ;; monokai : autogenerated stuff, don't mess with it
 '(custom-safe-themes (quote ("64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" default))))
(custom-set-faces)
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
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

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

;; undo tree
(require 'undo-tree)
(global-undo-tree-mode 1)
(defalias 'redo 'undo-tree-redo)
(defalias 'redo 'undo-tree-undo)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)
;; C-x u  (`undo-tree-visualize')
;;   Visualize the undo tree.

;; neotree - for file navigation

;; ace-jump : navigate quickly inside a buffer's text by jumping the cursor to desired locations
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; C-c C-c is compile, can't use that
;;(define-key global-map (kbd "C-c C-c SPC") 'ace-jump-line-mode)
;;(define-key global-map (kbd "C-c C-c C-c SPC") 'ace-jump-char-mode)


;; TO ADD
;; C-d : multicursor select the next like this
;; move lines/regions up and down by keyboard
;; save buffer state on close
;; multi-cursors buffer
;; more intuitive 'find'
;; minimap / sublimity
;; C-<backspace> is kinda wierd with white space








