;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Evil
(require 'evil)
(evil-mode 1)
(evil-select-search-module 'evil-search-module 'evil-search)

(require 'undo-tree)
(evil-set-undo-system 'undo-tree)
(global-undo-tree-mode)


;; Dired
;;; By default don't show information, toggle with `(`
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

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


;; Org mode
;;; 
(setq org-return-follows-link  t)
;;;
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



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "524fa911b70d6b94d71585c9f0c5966fe85fb3a9ddd635362bfabd1a7981a307" default))
 '(package-selected-packages '(undo-tree solarized-theme evil)))
    
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
