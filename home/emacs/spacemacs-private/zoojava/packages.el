(defvar zoojava-packages
  '(
    realgud
    meghanada
    ))

(defun zoojava/init-realgud ()
  (use-package realgud
    :config
    (progn)))

(defun zoojava/pre-init-meghanada ()
  (spacemacs|use-package-add-hook meghanada
    :pre-init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'java-mode
        "xx"  'meghanada-exec-main
        "tab" 'meghanada-switch-testcase
        "t."  'zoojava/meghanada-run-test-case-at-point
        "ts." 'zoojava/meghanada-toggle-test-case-at-point-after-save
        "it" 'meghanada-typeinfo
        "ir" 'meghanada-reference
        "ip" 'meghanada-show-project))))
