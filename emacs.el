;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user interface settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-face-attribute 'default nil :height 80)

;; only start the server if it is not yet running
(if (and (fboundp 'server-running-p)
	 (not (server-running-p)))
    (server-start))

(unless window-system (menu-bar-mode -1))
(when window-system (tool-bar-mode -1))

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)
(show-paren-mode t)
(hl-line-mode t)
(ido-mode)
(subword-mode)

;;; use versioned backups, don't clobber symlinks, don't litter fs tree
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.d/saves"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-version 2
 version-control 5)
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup package repositories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
	     '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package-install 'atom-dark-theme)
(load-theme 'atom-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python ide stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this requires jedi, flake8, and pyflakes to be availables.
;; The easiest path is to install them globally

;; elpy: ide
;; flymake: on-the-fly checks
;; sphinx-doc: autoinsert sphinx-doc docstrings
;;             (C-c M-d at function def)
(setq python-ide-package-list '(elpy flymake sphinx-doc))
(dolist (package python-ide-package-list)
  (package-install package))

(elpy-enable)
(add-hook 'python-mode-hook (lambda ()
			      (require 'sphinx-doc)
			      (sphinx-doc-mode t)))

