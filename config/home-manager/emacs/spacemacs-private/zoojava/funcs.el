(defvar zoojava-meghanada-current-test-case-class nil)
(defvar zoojava-meghanada-current-test-case-method nil)

(defun zoojava/meghanada-exec-in-buffer-callback (cb result)
  (when (and result (file-exists-p result))
    (with-current-buffer (find-file-noselect result)
      (funcall cb))))

(defun zoojava/meghanada-exec-in-switch-buffer (cb)
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (meghanada--send-request "st" (apply-partially #'zoojava/meghanada-exec-in-buffer-callback cb) (buffer-file-name))
    (message "client connection not established")))

(defun zoojava/-meghanada-run-test-case-at-point (file-name test-case)
  (let* ((class-name (car (split-string
                           (car (last (split-string file-name "/")))
                           "\\.")))
         (test-name (format "%s#%s" class-name test-case)))
    (save-window-excursion
      (meghanada--run-junit file-name nil test-name))))

(defun zoojava/meghanada-run-test-case-at-point ()
  (interactive)
  (zoojava/-meghanada-run-test-case-at-point (buffer-file-name) (which-function)))

(defun zoojava/meghanada-run-stored-test-case-at-point ()
  (when zoojava-meghanada-current-test-case-method
    (proctor/begin-notification)
    (zoojava/-meghanada-run-test-case-at-point zoojava-meghanada-current-test-case-class zoojava-meghanada-current-test-case-method)))

(defun zoojava/meghanada-enable-test-case-at-point-after-save ()
  (zoojava/add-after-save-hook 'zoojava/meghanada-run-stored-test-case-at-point t))

(defun zoojava/meghanada-disable-test-case-at-point-after-save ()
  (zoojava/remove-after-save-hook 'zoojava/meghanada-run-stored-test-case-at-point t))

(defun zoojava/meghanada-toggle-test-case-at-point-after-save ()
  (interactive)
  (if zoojava-meghanada-current-test-case-method
      (progn
        (zoojava/meghanada-disable-test-case-at-point-after-save)
        (zoojava/meghanada-exec-in-switch-buffer
         #'zoojava/meghanada-disable-test-case-at-point-after-save)
        (message (format "automatic execution of testcase '%s' was disabled"
                         zoojava-meghanada-current-test-case-method))
        (setq zoojava-meghanada-current-test-case-class nil)
        (setq zoojava-meghanada-current-test-case-method nil))
    ;; else
    (progn
      (setq zoojava-meghanada-current-test-case-class (buffer-file-name))
      (setq zoojava-meghanada-current-test-case-method (which-function))
      (zoojava/meghanada-enable-test-case-at-point-after-save)
      (zoojava/meghanada-exec-in-switch-buffer
       #'zoojava/meghanada-enable-test-case-at-point-after-save)
      (message (format "automatic execution of testcase '%s' was enabled"
                       zoojava-meghanada-current-test-case-method)))))

(defun zoojava/meghanada-after-junit-test-hook ()
  (with-current-buffer meghanada--junit-buf-name
    (goto-char (point-min))
    (when (search-forward "success" nil t)
      (proctor/succeed))
    (goto-char (point-min))
    (when (search-forward "fail" nil t)
      (proctor/fail)
      (display-buffer meghanada--junit-buf-name))))
