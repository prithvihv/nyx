;;; init.el --- Prithvi's Emacs config -*- lexical-binding: t; -*-

;; This file is a LIVE symlink managed by home-manager (see
;; darwin/common/emacs.nix). Nix installs the Emacs *binary*; everything below
;; is plain Emacs Lisp you can change WITHOUT a darwin-rebuild.
;;
;; Reload after editing (pick one):
;;   C-x C-e     eval the expression before the cursor
;;   M-x eval-buffer                 apply this whole file
;;   M-x restart-emacs   (or restart the daemon: see notes at bottom)
;;
;; Packages are installed at runtime by Emacs itself, so adding one is just a
;; new `use-package' block + reload. No Nix rebuild.

;;; Package management ---------------------------------------------------------

(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))
(package-initialize)

;; Fetch the package index the first time (nothing downloaded yet).
(unless package-archive-contents
  (package-refresh-contents))

;; use-package ships with Emacs 29+. Make every `use-package' auto-install the
;; package if it's missing, so a fresh machine bootstraps itself.
(require 'use-package)
(setq use-package-always-ensure t)

;;; Sane defaults --------------------------------------------------------------

(setq inhibit-startup-screen t)          ; skip the splash screen
(setq ring-bell-function 'ignore)        ; no audible bell
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(setq-default indent-tabs-mode nil)      ; indent with spaces

;; Keep the auto-generated `customize' settings in their own file, out of here.
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

;;; Packages -------------------------------------------------------------------
;; Example: pop-up that shows available keybindings as you type. This is the
;; pattern for everything — add a block, reload, done.

(use-package which-key
  :config (which-key-mode 1))

;;; init.el ends here
