;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
(setq doom-font (font-spec :size 16))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;;
;; Stup agenda view how I like it
(setq org-agenda-prefix-format
        '((agenda . " %i%c %b %?-12t% s")
        (todo . " %i %c %b")
        (tags . " %i %c %b")
        (search . " %i %c %b")))

;; Don't match < and > signs as parentheses
(defun org-syntax-table-modify ()
  "Modify `org-mode-syntax-table' for the current org buffer."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))
(add-hook 'org-mode-hook #'org-syntax-table-modify)
(setq org-startup-folded t)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; MY confiig
;;
;;
;;; Don't wrap lines
(set-default 'truncate-lines t)
(setq truncate-lines t)
(setq org-startup-truncated t)
(setq truncate-partial-width-windows 1000)
;(setq word-wrap nil)
(add-hook 'org-mode-hook (lambda () (setq truncate-lines t)))

; Insert org links as relative
(setq org-link-file-path-type 'relative)

(global-display-fill-column-indicator-mode 1)
(setq display-fill-column-indicator-column 80)

(add-to-list 'load-path "~/.config/doom/lisp")
(load "note-id")

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; ltex
(use-package lsp-ltex
  :ensure t
  :hook (text-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp)))  ; or lsp-deferred
  :init
  (setq lsp-ltex-check-frequency "save"
        lsp-ltex-language "auto"
        lsp-ltex-disabled-rules
        '(:fr ["DEUX_POINTS_ESPACE"]
          :en []))
  ; put in folder downlaod and rename lsp-ltex-server-store-path/ltex-ls-16.0.0 to ltex-ls/latest
  ; By default, lsp-ltex-server-store-path = ~/.config/emacs/.local/etc/lsp/ltex-ls
  ; I tried setting it to something nicer, but it dint' work
)
(define-key evil-normal-state-map (kbd "C-n") #'lsp-execute-code-action)
