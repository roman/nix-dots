;;; zoo-core.el --- Zoo core -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Core configuration.

;;; Code:
;;; Zoo group and customizations

(defgroup zoo nil
  "Zoo settings and configurations."
  :group 'convenience
  :prefix "zoo")

;;;; Directories
(defcustom zoo-emacs-dir (eval-when-compile (file-truename user-emacs-directory))
  "Path to the current Emacs directory."
  :type 'directory
  :group 'zoo)

(defcustom zoo-core-dir (expand-file-name "core/" zoo-emacs-dir)
  "Core packages and configuration for Zoo."
  :type 'directory
  :group 'zoo)

(defcustom zoo-module-dir (expand-file-name "modules/" zoo-emacs-dir)
  "Modules directory for Zoo."
  :type 'directory
  :group 'zoo)

(defcustom zoo-util-dir (expand-file-name "utils/" zoo-emacs-dir)
  "Utility directory for Zoo."
  :type 'directory
  :group 'zoo)

(defcustom zoo-org-dir (expand-file-name "org/" zoo-emacs-dir)
  "Org directory for Zoo."
  :type 'directory
  :group 'zoo)

(defcustom zoo-dotfiles-dir (expand-file-name ".dotfiles" (getenv "HOME"))
  "Location of dotfiles for Zoo."
  :type 'directory
  :group 'zoo)

;;;; Keybindings
(defcustom zoo-leader-key "SPC"
  "The default leader key for Zoo."
  :type 'string
  :group 'zoo)

(defcustom zoo-leader-secondary-key "C-SPC"
  "The secondary leader key for Zoo."
  :type 'string
  :group 'zoo)

(defcustom zoo-major-leader-key ","
  "The default major mode leader key for Zoo."
  :type 'string
  :group 'zoo)

(defcustom zoo-major-leader-secondary-key "M-,"
  "The secondary major mode leader key for Zoo."
  :type 'string
  :group 'zoo)

;;;; Assorted
(defcustom zoo-mono-font "PragmataPro Mono Liga"
  "The default monospaced font that Zoo uses."
  :type 'string
  :group 'zoo)

(defcustom zoo-serif-font "PragmataPro Liga"
  "The default sans serif font that Zoo uses."
  :type 'string
  :group 'zoo)

(defcustom zoo-line-spacing 0.15
  "The default line spacing width that Zoo uses."
  :type 'number
  :group 'zoo)

(defcustom zoo-font-size 140
  "The default font size for Zoo."
  :type 'number
  :group 'zoo)

;;; Core utilities

(defun zoo--byte-compile-zoo ()
  "Byte compile all files and directories used in Zoo."
  (interactive)
  (dolist (file (list (expand-file-name "early-init.el" zoo-emacs-dir)
                      (expand-file-name "init.el" zoo-emacs-dir)))
    (byte-compile-file file))
  (dolist (dir (list zoo-core-dir zoo-module-dir zoo-util-dir zoo-org-dir))
    (byte-recompile-directory dir 0 t)))

(defmacro csetq (&rest body)
  "A `setq' macro that works with `custom-set' properties. The
BODY is a list of the variables to be set."
  `(progn
     ,@(cl-loop for (var val) on body by 'cddr
                collect `(funcall (or (get ',var 'custom-set) #'set)
                                  ',var ,val))))

;;; Core settings
;; Emacs actually predates UTF8, which to my mind is kinda nuts. So we'll force
;; Emacs to always use unicode characters and UTF8 everywhere.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system                   'utf-8)
(set-terminal-coding-system             'utf-8)
(set-keyboard-coding-system             'utf-8)
(set-selection-coding-system            'utf-8)
(setq locale-coding-system              'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;; Then we can set some defaults that are core to Emacs. Beginning with changing
;; the initial screen for Emacs.
(setq-default inhibit-startup-message t
              inhibit-startup-buffer-menu t
              inhibit-startup-screen t
              inhibit-startup-echo-area-message t
              initial-buffer-choice t
              initial-major-mode 'fundamental-mode
              ;; Recusion and lisp call limit
              max-lisp-eval-depth 50000
              max-specpdl-size 10000
              ;; Set some common sense settings
              custom-file (expand-file-name "custom.el" zoo-emacs-dir)
              byte-compile--use-old-handlers nil     ;; Use the most recent byte code ops
              sentence-end-double-space nil          ;; Sentences end with a single space
              vc-follow-symlinks t                   ;; Always follow symbolic links
              save-interprogram-paste-before-kill t) ;; Save paste history when killing Emacs)

;; Hide the backup files inside `no-littering' directories.
(csetq backup-by-copying t            ;; Don't clobber symlinks
       delete-old-versions t          ;; Silently delete old files
       delete-by-moving-to-trash t    ;; And move them into the trash can
       kept-new-versions 6            ;; Keep some new versions around
       kept-old-versions 2            ;; But just a few old ones
       version-control t              ;; use versioned backups
       auto-save-file-name-transforms
       `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)) ;; Store auto save files in `no-littering'
       backup-directory-alist
       `((".*" . ,(no-littering-expand-var-file-name "backup/")))    ;; Store backups in `no-littering'
       ad-redefinition-action 'accept) ;; Don't give warnings for things out of my control

;; Fully inhibit the initial screen
(fset #'display-startup-echo-area-message #'ignore)

;; Mostly to save at most two strokes and at a minimum one. Efficiency baby.
(fset #'yes-or-no-p #'y-or-n-p)

;; Only load `custom.el' if it exists
(when (file-exists-p custom-file)
  (load custom-file t t))

;;; Load core configuration
(add-to-list 'load-path zoo-core-dir t)

(require 'zoo-packages)
(require 'zoo-keybindings)
(require 'zoo-system)
(require 'zoo-evil)
(require 'zoo-editor)
(require 'zoo-completion)
(require 'zoo-projects)
(require 'zoo-filechecking)
;; (require 'zoo-git)
;; (require 'zoo-ui)

;;; Load the rest of the configuration
;;;; Load all the modules
(require 'zoo-modules (expand-file-name "zoo-modules" zoo-module-dir))

;;;; Load the org configuration
(require 'zoo-org (expand-file-name "zoo-org" zoo-org-dir))

;;;; Load the utilities
(require 'zoo-utils (expand-file-name "zoo-utils" zoo-util-dir))


(provide 'zoo-core)

;;; zoo-core.el ends here

