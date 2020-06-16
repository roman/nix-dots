(defvar zoohs-packages
  '(
    haskell-mode
    intero))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell intero

(when (configuration-layer/layer-usedp 'haskell)

  (defvar zoohs/intero-auto-reload-status 'OFF)

  (defun zoohs/kill-intero-buffers ()
    (interactive)
    (--each (buffer-list)
      (let ((bname (s-trim (buffer-name it))))
        (when (s-starts-with? "intero:"  bname)
          (kill-buffer it)))))

  (defun zoohs/intero-repl-after-load (&optional prompt-options)
    (interactive)
    (let ((intero-pop-to-repl nil))
      (intero-with-repl-buffer prompt-options
        (intero-repl-after-load))))

  (defun zoohs/intero-repl-load (&optional prompt-options)
    (interactive)
    (intero-repl-load)
    (let ((intero-pop-to-repl nil))
      (intero-with-repl-buffer prompt-options
        (intero-repl-clear-buffer)
        (evil-goto-first-line))))

  (defun zoohs/intero-repl-reload (&optional prompt-options)
    (interactive "P")
    (when (eq major-mode 'haskell-mode)
      (save-buffer)
      (let ((intero-pop-to-repl nil)
            (file-buffer (current-buffer)))
        (intero-with-repl-buffer prompt-options
          (if (not intero-repl-last-loaded)
              (with-current-buffer file-buffer (intero-repl-load prompt-options))
            ;; else
            (progn
              (intero-repl-clear-buffer)
              (comint-simple-send
               (get-buffer-process (current-buffer))
               (concat ":reload"))
              (when intero-repl-send-after-load
                (comint-simple-send
                 (get-buffer-process (current-buffer))
                 intero-repl-send-after-load))
              (evil-normal-state)
              (evil-goto-first-line)))))))

  (defun zoohs/intero-toggle-after-reload (&optional prompt-options)
    (interactive)
    (if (eq zoohs/intero-auto-reload-status 'ON)
        (progn
          (zoo/remove-after-save-hook 'zoohs/intero-repl-reload)
          (setq zoohs/intero-auto-reload-status 'OFF)
          (message "Intero (re)load after save disabled"))
      (progn
        (zoo/add-after-save-hook 'zoohs/intero-repl-reload)
        (setq zoohs/intero-auto-reload-status 'ON)
        (message "Intero (re)load after save enabled"))))

  (defun zoohs/intero-clear-repl-send-after-load ()
    (interactive)
    (let ((intero-pop-to-repl nil))
      (intero-with-repl-buffer nil
        (setq intero-repl-send-after-load "")))
    (message "Intero command after load removed"))

  (defun zoohs/intero-restart-repl ()
    (interactive)
    (let ((intero-pop-to-repl nil))
      (intero-with-repl-buffer nil
        (setq intero-repl-last-loaded nil)))
    (intero-restart)
    (intero-repl-load))

  (defun zoohs/post-init-haskell-mode ()
    (spacemacs/declare-prefix-for-mode
      'haskell-mode "me" "haskell/edit")

    (when (configuration-layer/layer-usedp 'auto-completion)
      (advice-add 'yas-exit-snippet
                  :around
                  #'zoo/yasnippet-after-exit-snippet)

      (spacemacs/set-leader-keys-for-major-mode 'haskell-mode
        "ei" 'zoo/haskell-insert-import
        "eq" 'zoo/haskell-insert-import-qualified)
      )

    (spacemacs/set-leader-keys-for-major-mode 'haskell-mode
      "el" 'zoo/haskell-insert-lang-pragma
      "eo" 'zoo/haskell-insert-opt-pragma
      "gp" 'zoo/haskell-navigate-to-pragma-section
      ))

  (defun zoohs/pre-init-intero ()
    (spacemacs/declare-prefix-for-mode
      'haskell-mode "ma" "haskell/auto")

    (spacemacs|use-package-add-hook intero
      :post-config
      (progn
        (spacemacs/set-leader-keys-for-major-mode 'haskell-mode
          ","  'zoohs/intero-repl-reload
          "l"  'intero-repl-load
          "."  'hindent-reformat-decl-or-fill
          "a," 'zoohs/intero-toggle-after-reload
          "aa" 'zoohs/intero-repl-after-load
          "ac" 'zoohs/intero-clear-repl-send-after-load
          "iR" 'zoohs/intero-restart-repl
          )
        ))))
