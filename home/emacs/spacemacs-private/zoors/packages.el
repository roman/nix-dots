(defvar zoors-packages
  '(cargo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and (configuration-layer/layer-usedp 'zoo)
           (configuration-layer/layer-usedp 'rust))

  (defun zoors/pre-init-cargo ()
    (spacemacs|use-package-add-hook rust-mode
      :post-init
      (progn
        (require 'cargo)
        ;; reset pre-defined keybindings
        (spacemacs/set-leader-keys-for-major-mode 'rust-mode
          "cb" 'zoors/cargo-build
          "t"   nil
          "ct"  nil)
        ;; define keybinding prefixes
        (spacemacs/declare-prefix-for-mode 'rust-mode "mt" "tests")
        (spacemacs/declare-prefix-for-mode 'rust-mode "ms" "save hooks")
        (spacemacs/declare-prefix-for-mode 'rust-mode "mst" "tests")
        ;; setup new keybindings
        (spacemacs/set-leader-keys-for-major-mode 'rust-mode
          ","   'zoors/cargo-process-current-test
          "stt" 'zoors/toggle-run-stored-test-function-after-save
          "tt"  'zoors/cargo-process-current-test
          "tf"  'zoors/cargo-process-current-file-tests))
      )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
