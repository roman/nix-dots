(defvar zoogo-packages
  '(go-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and (configuration-layer/layer-usedp 'go)
           (configuration-layer/layer-usedp 'zoo))

  (defun zoogo/post-init-go-mode ()
    (spacemacs|use-package-add-hook go-mode
      :post-config
      (progn
        ;; enable LSP
        ;; (when (configuration-layer/layer-used-p 'lsp)
        ;;   (spacemacs/lsp-bind-keys-for-mode 'go-mode))

        ;; enable DAP
        (when (configuration-layer/layer-used-p 'dap)
          (require 'dap-go)
          (dap-go-setup)
          ;; (spacemacs/dap-bind-keys-for-mode 'go-mode)
          )

        (spacemacs/declare-prefix-for-mode
          'go-mode "s" "go/after-save")
        (spacemacs/declare-prefix-for-mode
          'go-mode "st" "go/after-save test")
        (spacemacs/set-leader-keys-for-major-mode 'go-mode
          "tt"  'zoogo/run-test-current-function
          "stt" 'zoogo/toggle-run-test-stored-function-after-save
          "stp" 'zoogo/toggle-run-package-tests-after-save
          "stc" 'zoogo/reset-stored-test-function)
        )))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
