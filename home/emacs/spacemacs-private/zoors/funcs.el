;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar zoors/current-test-file nil)
(defvar zoors/current-test-function nil)

;;;###autoload
(defun zoors/run-stored-test-function ()
  "Executes the existing stored go test function"
  (interactive)
  (when (eq major-mode 'rust-mode)
    (if zoors/current-test-function
        (progn
          (zoo/flash-mode-line zoogo/active-after-save-color 0.5)
          (if zoo/hide-on-compile-success
              (save-window-excursion
                (zoors/cargo-process-run-test zoors/current-test-function))
            ;; else
            (zoors/cargo-process-run-test zoors/current-test-function)
            )
          )
      ;; else
      (message "Must run test function command first"))))

;;;###autoload
(defun zoors/toggle-run-stored-test-function-after-save ()
  (interactive)
  (zoo/toggle-after-save-hook 'zoors/run-stored-test-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun zoors/cargo-process-current-test ()
  (interactive)
  (setq zoors/current-test-function (cargo-process--get-current-test-fullname))
  (if zoo/hide-on-compile-success
    (save-window-excursion
      (cargo-process-current-test))
    ;; else
    (cargo-process-current-test)))

;;;###autoload
(defun zoors/cargo-process-current-file-tests ()
  (interactive)
  (if zoo/hide-on-compile-success
      (save-window-excursion
        (cargo-process-current-file-tests))
    ;; else
    (cargo-process-current-file-tests)))

;;;###autoload
(defun zoors/cargo-process-run-test (name)
  (cargo-process--start "Test"
                        (concat cargo-process--command-current-test
                                " "
                                name)))
