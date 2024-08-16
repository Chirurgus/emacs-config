;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;;; Declare packages
(setq my-packages
    '(
    evil
    undo-tree
    magit
    evil-collection
    vertico
    corfu ; similar to company but better integration with vertico
    marginalia
    lsp-ltex
    ))
;;; Iterate on packages and install missing ones
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Evil
;;; I am told to do this but I don't understand what it does
(setq evil-want-integration t) ;; This is optional since it's already set to t by default.
(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode 1)
(evil-select-search-module 'evil-search-module 'evil-search)
(require 'evil-collection)
(evil-collection-init)

;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(require 'undo-tree)
(evil-set-undo-system 'undo-tree)
(global-undo-tree-mode)


;; Start server 
(load "server")
(unless (server-running-p) (server-start))

;; Dired
;;; By default don't show information, toggle with `(`
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;;(setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )
(setq completion-styles '(flex basic))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 1)
  (corfu-auto-prefix 3)
  (completion-styles '(flex basic))
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init
  (global-corfu-mode))

;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
;; mode.  Vertico commands are hidden in normal buffers. This setting is
;; useful beyond Vertico.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Don't use consult package 
; (use-package consult)
; (setq completion-in-region-function
      ; (lambda (&rest args)
        ; (apply (if vertico-mode
                   ; #'consult-completion-in-region
                 ; #'completion--in-region)
               ; args)))
; (setq completion-in-region-function 'consult-completion-in-region)
; (setq enable-recursive-minibuffers t)

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;;; LanguageTool
;;; via langtool
;(setq langtool-language-tool-jar "C:/Users/oleksandr.sorochynsk/Downloads/LanguageTool-6.4/languagetool-commandline.jar")
;(require 'langtool)
;;; Via Ltex
(use-package lsp-ltex
  :ensure t
  :hook (text-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp)))  ; or lsp-deferred
  :init
  (setq lsp-ltex-version "16.0.0"))  
(define-key evil-normal-state-map (kbd "C-n") #'lsp-execute-code-action)

;; General editor settings
(set-language-environment "UTF-8")
;;; Don't wrap lines
(set-default 'truncate-lines t)
;;; Show line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;;; Enable line column
(global-display-fill-column-indicator-mode 1)
(setq display-fill-column-indicator-column 80)
;;; Highlight line
(global-hl-line-mode)
;;; Disable
(setq inhibit-startup-screen t)

;; Appearance
;;; Load color theme
(load-theme 'solarized-light t)
;;; Remove toolbar and menubar
(menu-bar-mode -1)
(tool-bar-mode -1)
;; set the fond size
(defun set-font-size (size)
    "Set the font size for all buffers"
    (interactive)
    (set-face-attribute 'default nil :height (* size 10)))
;; Set default font size
(set-font-size 14)

;; Keybinds
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
(define-key evil-normal-state-map (kbd "M-o") #'project-find-file)


;; Org mode
;;; Enter follows link (but it doesn't seem to work, `gf` works fine though)
(setq org-return-follows-link  t)
(setq org-log-done 'time)
;;; Always use org-modes' indent mode
(add-hook 'org-mode-hook 'org-indent-mode)
;; org-capture
(setq org-capture-templates
    '(
	("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks") "* TODO %?\n  %i\n  %a")
	("j" "Journal" entry (file+datetree "~/org/journal.org") "* %?\nEntered on %U\n  %i\n  %a")
    )
)
;; Don't match < and > signs as parentheses
(defun org-syntax-table-modify ()
  "Modify `org-mode-syntax-table' for the current org buffer."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))
(add-hook 'org-mode-hook #'org-syntax-table-modify)

;; Backups 
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq make-backup-files t               ; backup of a file the first time it is saved.
    backup-by-copying t               ; don't clobber symlinks
    version-control t                 ; version numbers for backup files
    delete-old-versions t             ; delete excess backup files silently
    delete-by-moving-to-trash t
    kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
    kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
    auto-save-default t               ; auto-save every buffer that visits a file
    auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
    auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
    )

;; Undo history
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; Load custom scrips
(add-to-list 'load-path "~/.emacs.d/lisp")
(load "note-id")

;; Load settings generated with `customize` interface
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

