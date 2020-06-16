;;; nix-shell-env.el --- Get environment variables such as $PATH from the a nix-shell

;; Copyright (C) 2019 Roman Gonzalez

;; Author: Roman Gonzalez <open-source@roman-gonzalez.info>
;; Keywords: environment
;; URL: https://github.com/roman/nix-shell-env
;; Package-Version: 0

;; Inspired from Steve Purcell's work on copy-shell-env

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; On OS X (and perhaps elsewhere) the $PATH environment variable and
;; `exec-path' used by a windowed Emacs instance will usually be the system-wide
;; default path, rather than that seen in a terminal window.

;; This library allows the user to set Emacs' `exec-path' and $PATH from the
;; shell path, so that `shell-command', `compile' and the like work as expected.

;; It also allows other environment variables to be retrieved from a nix-shell
;; created from a shell.nix file inside a project, so that Emacs will see the
;; same values you get in a nix-shell terminal.

;; Note that shell variables which have not been exported as environment
;; variables (e.g. using the "export" keyword) may not be visible to
;; `nix-shell-env'.

;; Installation:

;; ELPA packages are available on Marmalade and MELPA. Alternatively, place this
;; file on a directory in your `load-path', and explicitly require it.

;; Usage:
;;
;;     (require 'nix-shell-env) ;; if not using the ELPA package
;;     (nix-shell-env-initialize)
;;
;; Customize `nix-shell-env-variables' to modify the list of variables imported.
;;
;; If you use your Emacs config on other platforms, you can instead make
;; initialization conditional as follows:
;;
;;     (when (memq window-system '(mac ns))
;;       (nix-shell-env-initialize))
;;
;; Alternatively, you can use `nix-shell-env-copy-envs' or
;; `nix-shell-env-copy-env' directly, e.g.
;;
;;     (nix-shell-env-copy-env "PYTHONPATH")

;;; Code:

(require 's)

(defgroup nix-shell-env nil
  "Make Emacs use nix-shell defined values for $PATH etc."
  :prefix "nix-shell-env-"
  :group 'environment)

(defcustom nix-shell-env-variables
  '("PATH" "MANPATH")
  "List of environment variables which are copied from the nix-shell."
  :type '(repeat (string :tag "Environment variable"))
  :group 'nix-shell-env)

(defvar nix-shell-env-debug nil
  "Display debug info when non-nil.")

(defun nix-shell-env--double-quote (s)
  "Double-quote S, escaping any double-quotes already contained in it."
  (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\""))

(defun nix-shell-env--debug (msg &rest args)
  "Print MSG and ARGS like `message', but only if debug output is enabled."
  (when nix-shell-env-debug
    (apply 'message msg args)))

(defun nix-shell-env--standard-shell-p (shell)
  "Return non-nil iff SHELL supports the standard ${VAR-default} syntax."
  (not (string-match "\\(fish\\|t?csh\\)$" shell)))

(defun nix-shell-env-printf (str &optional args)
  "Return the result of printing STR in the user's shell.

Executes $SHELL as interactive login shell.

STR is inserted literally in a single-quoted argument to printf,
and may therefore contain backslashed escape sequences understood
by printf.

ARGS is an optional list of args which will be inserted by printf
in place of any % placeholders in STR.  ARGS are not automatically
shell-escaped, so they may contain $ etc."
  (let* ((printf-bin (or (executable-find "printf") "printf"))
         (printf-command
          (concat printf-bin
                  " '__RESULT\\000" str "' "
                  (mapconcat #'nix-shell-env--double-quote args " ")))
         (shell-args (list "--command" printf-command))
         (shell "nix-shell"))
    (with-temp-buffer
      (nix-shell-env--debug "Invoking shell %s with args %S" shell shell-args)
      (let ((exit-code (apply #'call-process shell nil t nil shell-args)))
        (nix-shell-env--debug "Shell printed: %S" (buffer-string))
        (unless (zerop exit-code)
          (error "Non-zero exit code from shell %s invoked with args %S.  Output was:\n%S"
                 shell shell-args (buffer-string))))
      (goto-char (point-min))
      (if (re-search-forward "__RESULT\0\\(.*\\)" nil t)
          (match-string 1)
        (error "Expected printf output from shell, but got: %S" (buffer-string))))))

(defun nix-shell-env-getenvs (names)
  "Get the environment variables with NAMES from the user's shell.

Execute $SHELL according to `nix-shell-env-arguments'.
The result is a list of (NAME . VALUE) pairs."
  ;; find parent folder that contains a shell.nix
  (if-let ((shell-file-dir (locate-dominating-file "." "shell.nix")))
      (let* ((default-directory shell-file-dir)
             (random-default (md5 (format "%s%s%s" (emacs-pid) (random) (current-time))))
             (dollar-names (mapcar (lambda (n) (format "${%s-%s}" n random-default)) names))
             (values (split-string (nix-shell-env-printf
                                    (mapconcat #'identity (make-list (length names) "%s") "\\000")
                                    dollar-names) "\0")))
        (let (result)
          (while names
            (prog1
                (let ((value (car values)))
                  (push (cons (car names)
                              (unless (string-equal random-default value)
                                value))
                        result))
              (setq values (cdr values)
                    names (cdr names))))
          result))
    ;; else
    (message "Didn't find any parent folder that contains a shell.nix")))

(defun nix-shell-env-getenv (name)
  "Get the environment variable NAME from the user's shell.

Execute $SHELL as interactive login shell, have it output the
variable of NAME and return this output as string."
  (cdr (assoc name (nix-shell-env-getenvs (list name)))))

(defun nix-shell-env-setenv (name value)
  "Set the value of environment var NAME to VALUE.
Additionally, if NAME is \"PATH\" then also set corresponding
variables such as `exec-path'."
  (setenv name value)
  (when (string-equal "PATH" name)
    (setq eshell-path-env value
          exec-path (append (parse-colon-path value) (list exec-directory)))))

;;;###autoload
(defun nix-shell-env-copy-envs (names0)
  "Set the environment variables with NAMES from the user's shell.

As a special case, if the variable is $PATH, then `exec-path' and
`eshell-path-env' are also set appropriately.  The result is an alist,
as described by `nix-shell-env-getenvs'."
  (interactive "sEnvironment variables to copy (space separated): ")
  (let* ((names (s-split-words names0))
         (pairs (nix-shell-env-getenvs names)))
    ;; (when nix-shell-env-check-startup-files
    ;;   (nix-shell-env--maybe-warn-about-startup-files pairs))
    (mapc (lambda (pair)
            (nix-shell-env-setenv (car pair) (cdr pair)))
          pairs)))

;;;###autoload
(defun nix-shell-env-copy-env (name)
  "Set the environment variable $NAME from the user's shell.

As a special case, if the variable is $PATH, then `exec-path' and
`eshell-path-env' are also set appropriately.  Return the value
of the environment variable."
  (interactive "sEnvironment variable to copy: ")
  (cdar (nix-shell-env-copy-envs (list name))))

;;;###autoload
(defun nix-shell-env-initialize ()
  "Initialize environment from the user's shell.

The values of all the environment variables named in
`nix-shell-env-variables' are set from the corresponding
values used in the user's shell."
  (interactive)
  (nix-shell-env-copy-envs nix-shell-env-variables))


(provide 'nix-shell-env)

;;; nix-shell-env.el ends here
