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

(setq x-super-keysym 'meta)

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
 '(custom-enabled-themes (quote (wheatgrass)))
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

(defun set-exec-path-from-shell-PATH ()
  (let 
      ((path-from-shell
	 (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))




;;
;; Packages initialization
;;

;; packages to install
(defvar package-list      '(evil flycheck undo-tree auto-complete
				 anzu ; hilight words
				 golden-ratio expand-region
				 yasnippet
				 sql-indent
				 perl-completion
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
;; Packeges settings
;;

(require 'evil)
(evil-mode 1)

(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'auto-complete)
(global-auto-complete-mode 1)

(require 'anzu)
(global-anzu-mode 1)

(require 'ido)
(ido-mode t)

(require 'golden-ratio)
(golden-ratio-mode 1)
(setq golden-ratio-exclude-modes '("ediff-mode"
				   "eshell-mode"
				   "dired-mode"))

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'yasnippet)
(yas-global-mode 1)

(eval-after-load "sql"
  '(load-library "sql-indent"))

(add-hook 'cperl-mode-hook
	  (lambda()
	    (require 'perl-completion)
	    (perl-completion-mode t)))
(add-hook  'cperl-mode-hook
	   (lambda ()
	     (when (require 'auto-complete nil t) ; no error whatever auto-complete.el is not installed.
	       (auto-complete-mode t)
	       (make-variable-buffer-local 'ac-sources)
	       (setq ac-sources
		     '(ac-source-perl-completion)))))

(setq-default c-basic-offset 8
	      tab-width 8
	      indent-tabs-mode t)

(setq c-default-style '((other . "linux")))


;reindent the new line
(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))

(add-hook 'c-initialization-hook 'my-make-CR-do-indent)

;;; init.el ends here

