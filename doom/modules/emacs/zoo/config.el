;;; emacs/zoo/config.el -*- lexical-binding: t; -*-


(map! :leader
      :desc "Add after-save hook" "k a" 'zoo/add-after-save-hook-kbd
      :desc "Remove after-save hook" "k r" 'zoo/remove-after-save-hook
      :desc "Toggle last after-save hook" "k t" 'zoo/toggle-last-after-save-hook
      :desc "Toogle Flycheck Error popup" "o e" 'zoo/toggle-to-error-list)

(when (and (featurep! :lang rust)
           (featurep! :emacs zoo +rust))
  (after! rustic
    ;; remove popup-rule of rustic-compilation
    (setq display-buffer-alist
          (delq (assoc "^\\*rustic-compilation" display-buffer-alist) display-buffer-alist)
          )
    ;; and replace it with something that makes more sense
    (set-popup-rule! "^\\*rustic-compilation" :side 'right :width 0.5 :modeline t)
    (map! :map rustic-mode-map
          :localleader
          :desc "last cargo test run" "," 'zoo/rustic-last-test-call
          (:prefix ("b" . "build")
           :desc "cargo check" "c" #'zoo/rustic-cargo-check
           )
          (:prefix ("t" . "cargo test")
           :desc "all"          "a" #'zoo/rustic-cargo-test-run
           :desc "current test" "t" #'zoo/rustic-cargo-current-test)))
  )
