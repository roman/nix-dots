(spacemacs/declare-prefix "ah" "hooks")

(spacemacs/set-leader-keys
  "aha" 'zoo/add-after-save-hook
  "ahr" 'zoo/remove-after-save-hook
  "asc" 'zoo/toggle-compile-on-save
  "." 'evil-avy-goto-char)
