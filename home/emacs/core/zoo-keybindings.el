;;; zoo-keybindings.el --- Zoo keybinding configuration -*- lexical-binding: t -*-

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
;; This is probably the hardest thing by far to configure and properly do in
;; Emacs, at least in my opinion. I could use something like Spacemacs or Doom
;; which has a proper consistent theme for keybindings, but that's no fun.
;; Instead we'll roll our own built around `Evil', `General.el' and `which-key'.
;; Lastly, we'll mimick how I used to do things in Vim (and how Spacemacs and
;; others does things) by letting `SPC' be our leader key and `,' be our major
;; mode leader key. If you are in the `insert' state, you can use `C-SPC' for
;; the leader key and `M-,' for the major mode leader key.

;;; Code:
;;; `which-key':
;; This is a really cool package, I initially discovered this from Spacemacs (as
;; I have done with a great many things). What it does is show you any and all
;; keybindings you can complete from the binding you just executed. For example,
;; if you are in Org-mode and run `C-c', `which-key' will show on the bottom of
;; the screen and show all the keybindings you can complete from there. It's
;; really great for discoverability.
(use-package which-key
  :demand t
  :delight
  :commands which-key-mode
  :init (which-key-mode)
  :config
  (progn
    (csetq which-key-idle-delay 0.2                           ;; Reduce the time before which-key pops up
           which-key-sort-order 'which-key-key-order-alpha))) ;; Sort things properly alphabetical

;;; `General':
;; This is a whole framework for binding keys in a really nice and consistent
;; manner.
(use-package general
  :demand t
  :commands general-evil-setup
  :config
  (progn
    (general-override-mode)
    (general-evil-setup)
    (general-create-definer zoo-leader
      :states '(normal insert emacs)
      :prefix zoo-leader-key
      :non-normal-prefix zoo-leader-secondary-key)
    (general-create-definer zoo-major-leader
      :states '(normal insert emacs)
      :prefix zoo-major-leader-key
      :non-normal-prefix zoo-major-leader-secondary-key)
    (general-nmap "SPC m" (general-simulate-key "," :which-key "major mode"))))

;;; Global bindings
;; Default `which-key' prefixes
;; This keeps all the main menus in one place instead of spread throughout the
;; whole project.
(zoo-leader
  "SPC" '(counsel-M-x :wk "M-x")
  "a" '(:ignore t :wk "applications")
  "b" '(:ignore t :wk "buffers")
  "f" '(:ignore t :wk "files")
  "g" '(:ignore t :wk "git")
  "h" '(:ignore t :wk "help")
  "S" '(:ignore t :wk "spelling")
  "w" '(:ignore t :wk "windows")) 

(provide 'zoo-keybindings)

;;; zoo-keybindings.el ends here

