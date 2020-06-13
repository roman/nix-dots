;;; zoo-modules.el --- Zoo module configuration -*- lexical-binding: t -*-

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
;; Load all our precious modules.

;;; Code:

;;; Functions
(defun zoo--byte-compile-module-dir ()
  "Byte compile all modules that Zoo bundles."
  (interactive)
  (byte-recompile-directory zoo-module-dir 0 t))

;;; Add all the languages to the load path
(eval-and-compile
  (add-to-list 'load-path zoo-module-dir t))

;;; Load modules
;; (require 'zoo-elisp)
;; (require 'zoo-latex)
;; (require 'zoo-markdown)

(provide 'zoo-modules)

;;; zoo-modules.el ends here

