;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; additional useful functions and variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun badi/package-install (package)
  "Install a package if it is not present already"
  (unless (package-installed-p package)
    (package-install package)))

(defun badi/package-install-list (package-list)
  "Install a list of packages if necessary"
  (dolist (package package-list)
    (badi/package-install package)))

(defun badi/package/refresh-contents ()
  "Refresh the package contents if necessary"
  (unless package-archive-contents
    (package-refresh-contents)))

(defun badi/package/emacs-compat-fix ()
  "Add gnu packages when emacs is v23 or less for libs like cl-lib"
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup package repositories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
	     '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(badi/package/emacs-compat-fix)
(package-initialize)
(badi/package/refresh-contents)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; install some useful packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(badi/package-install-list
 '(
   ;; auto-complete mode
   ;; http://emacswiki.org/emacs/AutoComplete
   auto-complete

   ;; Color variables differently
   ;; https://github.com/ankurdave/color-identifiers-mode
   color-identifiers-mode

   ;; flexible string matching
   flx
   flx-ido

   ;; on-the-fly checking
   flycheck
   ;; colors the mode line according to the Flycheck state
   ;; https://github.com/flycheck/flycheck-color-mode-line
   flycheck-color-mode-line

   ;; hungry delete
   ;; delete all whitespace in the direction you are deleting
   ;; https://github.com/nflath/hungry-delete
   hungry-delete

   ;; vertial ido matches
   ido-vertical-mode

   ;; make line numbers relative to cursor
   linum-relative

   ;; git
   ;; http://www.emacswiki.org/emacs/Magit
   ;; http://www.masteringemacs.org/article/introduction-magit-emacs-mode-git
   magit

   ;; markdown
   ;; http://jblevins.org/projects/markdown-mode/
   markdown-mode

   ;; multiple cursors
   ;;https://github.com/magnars/multiple-cursors.el
   multiple-cursors

   ;; enhance M-x with IDO
   ;; https://github.com/nonsequitur/smex
   smex

   ;; better undo
   ;; http://www.emacswiki.org/emacs/UndoTree
   undo-tree

   ;; visual regexp (actual regex, not emacs-style)
   ;; note: this requires python
   ;; https://github.com/benma/visual-regexp-steroids.el/
   visual-regexp-steroids

   ;; snippets
   ;; https://github.com/capitaomorte/yasnippet
   yasnippet
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto complete
;; globally enable auto-complete
(global-auto-complete-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; color identifiers
;; globally enable color-identifiers-mode
(add-hook 'after-init-hook 'global-color-identifiers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ido
;; Interactively do things
(ido-mode 1)
(require 'flx-ido) ; flexible string matching
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(flx-ido-mode 1)

					; use ido vertically (easier to read)
(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down) ; for arrow keys

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; flx-ido

(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)
;; disable ido faces to see flx highlighting
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; flycheck
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hungry delte
(global-hungry-delete-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; linum relative
(require 'linum-relative)
(global-set-key (kbd "C-x C-m C-l") 'linum-relative-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; markdown
(autoload 'markdown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; multiple cursors
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; smex
(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; the old M-x
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; undo tree
(require 'undo-tree)
(global-undo-tree-mode)

;; ;; some useful key bindings
;;   key             binding
;; ---             -------

;; C-x          Prefix Command
;; ESC          Prefix Command
;; C-_          undo-tree-undo
;; C-/          undo-tree-undo
;; C-?          undo-tree-redo
;; C-x u        undo-tree-visualize

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; visual regexp
(require 'visual-regexp-steroids)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

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
;; themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(badi/package-install 'atom-dark-theme)
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
(badi/package-install-list '(elpy
			     flymake
			     sphinx-doc))

(elpy-enable)
(add-hook 'python-mode-hook (lambda ()
			      (require 'sphinx-doc)
			      (sphinx-doc-mode t)))

