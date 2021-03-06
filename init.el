;;; package --- Configuration entry point

;; Copyright (c) 2016-.. #John
;;
;; Author: #John <pocolab.com@gmail.com>
;; URL: http://www.pocolab.com
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply set up the default load path
;; and define basic Emacs settings.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;;
;; Emacs settings
;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq column-number-mode t)

;;
;; Theme settings
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(custom-safe-themes
   (quote
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;
;; Enviroment settings
;;

(defun setenvvariable(key value)
  (setenv key (concat (getenv key) ":" value))
  (setq exec-path (append exec-path '(value))))

(setenvvariable "PATH" "c:/cygwin/bin")
(setenvvariable "PATH" "c:/cygwin64/bin")

(defvar custom-scripts-path "~/.emacs.d/custom_scripts/")

;;
;; Packages initialization
;;

;; packages to install
(defvar package-list      '(
			    evil
			    flycheck
			    undo-tree
			    auto-complete
			    smex ; most recently executed commands M-x 
			    anzu ; hilight words
			    golden-ratio
			    expand-region
			    yasnippet
			    sql-indent
			    markdown-mode
			    clojure-mode
			    ))

;; repositories with packages
(defvar package-archives  '(("melpa" . "http://melpa.org/packages/")
			    ("gnu" . "http://elpa.gnu.org/packages/")))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;;
;; Packages settings
;;

;; evil
(require 'evil)
(evil-mode 1)

;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; auto-complete
(require 'auto-complete)
(global-auto-complete-mode 1)
(setq auto-install-save-confirm nil)

;; anzu
(require 'anzu)
(global-anzu-mode 1)

;; ido
(require 'ido)
(ido-mode t)

;; golden-ratio
(require 'golden-ratio)
(golden-ratio-mode 1)
(setq golden-ratio-exclude-modes '("ediff-mode"
				   "eshell-mode"
				   "dired-mode"))

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; sql-indent
(eval-after-load "sql"
  '(load-library "sql-indent"))

;;
;; Other settings 
;;

;; winner-mode -> open latly closed window 'C-c left' 'C-c rigth'
(when (fboundp 'winner-mode)
      (winner-mode 1))

;; windmove -> change window 'M left' 'M rigth' 'M up' 'M down'
(windmove-default-keybindings 'meta)


;; hs-minor-mode -> hide-show blocks of text
(defun hs-minor-mode-hook(hook)
  (add-hook hook   'hs-minor-mode)
  (add-hook hook
	    (lambda ()
	      (local-set-key (kbd "C-c H") 
			     'hs-hide-all)
	      (local-set-key (kbd "C-c S")
			     'hs-show-all)
	      (local-set-key (kbd "C-c +")
			     'hs-toggle-hiding))))

(hs-minor-mode-hook 'c-mode-common-hook)
(hs-minor-mode-hook 'emacs-lisp-mode-hook)
(hs-minor-mode-hook 'lisp-mode-hook)
(hs-minor-mode-hook 'sh-mode-hook)

;;etags
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
    (visit-tags-table default-directory nil)))

(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
	ad-do-it
      (error (and (buffer-modified-p)
		  (not (ding))
		  (y-or-n-p "Buffer is modified, save it? ")
		  (save-buffer))
	     (er-refresh-etags extension)
	     ad-do-it))))

(global-set-key (kbd "C-c g") 'find-tag)         ;got to tag 


;; eldoc
(setq c-eldoc-includes "`pkg-config gtk+-3.0 --cflags` -I./ -I../ ")
(let((path (concat custom-scripts-path "eldoc/")))
(load (concat  path "c-eldoc.el"))
(load (concat  path "eldoc-higlight-arguments.el"))
)

(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; c language
(setq-default c-basic-offset 8
	      tab-width 8
	      indent-tabs-mode t)

(setq c-default-style '((other . "linux")))


;;; init.el ends here

