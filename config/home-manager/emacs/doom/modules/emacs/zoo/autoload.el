;;; emacs/zoo/autoload.el -*- lexical-binding: t; -*-
;;;
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; After Save Hook

(defvar zoo/last-after-save-hook nil)
(defvar zoo/hook-overrides-hash-table (make-hash-table))

;;;###autoload
(defun zoo/-get-hook-funcs (hook)
  (delq nil (mapcar
             (lambda (e) (if (symbolp e) e))
             hook)))

;;;###autoload
(defun zoo/-get-hook-funcs-names (hook)
  (mapcar 'symbol-name
          (zoo/-get-hook-funcs
           (if (symbolp hook)
               (append (symbol-value hook)
                       (default-value hook))
             hook))))

;;;###autoload
(defun zoo/-get-all-hooks ()
  (let (hlist (list))
    (mapatoms (lambda (a)
                (if (and (not (null (string-match ".*-hook"
                                                  (symbol-name a))))
                         (not (functionp a)))
                    (add-to-list 'hlist a))))
    hlist))

;;;###autoload
(defun zoo/remove-from-hook (hook fname &optional local)
  (interactive
   (let ((hook (intern (ido-completing-read
                        "Which hook? "
                        (mapcar #'symbol-name (zoo/-get-all-hooks))))))
     (list hook
           (ido-completing-read "Which? " (zoo/-get-hook-funcs-names hook)))))
  (remove-hook hook
               (if (stringp fname)
                   (intern fname)
                 fname)
               local))

;;;###autoload
(defun zoo/remove-after-save-hook (fname &optional local)
  (interactive (list (ido-completing-read
                      "aWhich function: "
                      (zoo/-get-hook-funcs-names 'after-save-hook))))
  (zoo/remove-from-hook 'after-save-hook fname local))

;;;###autoload
(defun zoo/add-after-save-hook (fname &optional local)
  (interactive "aWhich function: ")
  ;; write the function to the buffer-file-name for easy
  ;; removal later on
  (setf (gethash (buffer-file-name (current-buffer))
                 zoo/hook-overrides-hash-table)
        fname)
  (setq zoo/last-after-save-hook fname)
  (add-hook 'after-save-hook fname t local)
  (message (format "%s will execute after a save" fname)))

;;;###autoload
(defun zoo/add-after-save-hook-kbd (key-seq &optional local)
  (interactive
   (list (read-key-sequence "Press key: ")))
  (let ((sym (key-binding key-seq)))
    (cond
     ((null sym)
      (user-error "No command is bound to %s" (key-description key-seq)))
     ((commandp sym)
      (zoo/add-after-save-hook sym local)))))

;;;###autoload
(defun zoo/check-after-save-hook (fname)
  (-contains? (zoo/-get-hook-funcs-names 'after-save-hook) (symbol-name fname)))

(defvar zoo/after-save-hook-activated-color "#6AAF6A")
(defvar zoo/after-save-hook-disabled-color  "#D70000")

;;;###autoload
(defun zoo/toggle-last-after-save-hook (&optional local)
  (interactive)
  (when zoo/last-after-save-hook
    (if (zoo/check-after-save-hook zoo/last-after-save-hook)
        (progn
          (zoo/flash-mode-line zoo/after-save-hook-disabled-color 0.5)
          (zoo/remove-after-save-hook zoo/last-after-save-hook local))
      ;; else
      (zoo/flash-mode-line zoo/after-save-hook-activated-color 0.5)
      (zoo/add-after-save-hook zoo/last-after-save-hook local))
    ))

;;;###autoload
(defun zoo/toggle-after-save-hook (fname &optional local)
  (if (zoo/check-after-save-hook fname)
      (progn
        (zoo/flash-mode-line zoo/after-save-hook-disabled-color 0.5)
        (zoo/remove-after-save-hook fname local))

      ;; else
    (zoo/flash-mode-line zoo/after-save-hook-activated-color 0.5)
    (zoo/add-after-save-hook fname local)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flash mode line

(defvar zoo/flash-mode-line-activate-color  "#8A2BE2"
  "Color for when signaling an activation via flash-mode-line.")
(defvar zoo/flash-mode-line-success-color "#6AAF6A"
  "Color for when signaling success outcome via flash-mode-line.")
(defvar zoo/flash-mode-line-failed-color  "#FF6347"
  "Color for when signaling failed outcome via flash-mode-line.")


(defvar zoo-mode-line-in-use nil
  "Private var to check usage of mode-line before changing.")

(defvar zoo-mode-line-usage-delay 0.5
  "Number of secs before trying to modify the mode-line again.")

;;;###autoload
(defun zoo/restore-modeline (orig-modeline-fg)
  (set-face-background 'mode-line orig-modeline-fg)
  (setq zoo-mode-line-in-use nil))

;;;###autoload
(defun zoo/flash-mode-line (&optional color time)
  "Flashes the mod-line with a given COLOR for a period of TIME."
  (interactive)
  (if (not zoo-mode-line-in-use)
      (let ((orig-modeline-fg (face-background 'mode-line))
            (time  (or time 2))
            (color (or color "#d70000")))
        (setq zoo-mode-line-in-use t)
        (set-face-background 'mode-line color)
        (run-with-timer time nil
                        'zoo/restore-modeline
                        orig-modeline-fg))
    ;; else
    (run-with-timer zoo-mode-line-usage-delay nil
                    'zoo/flash-mode-line
                    color
                    time
                    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Last Call feature

(defvar zoo/last-test-buffer nil)
(defvar zoo/last-test-point nil)
(defvar zoo/last-test-function nil)

(defun zoo/store-last-test (fname)
  (setq zoo/last-test-function fname)
  (setq zoo/last-test-point (point))
  (setq zoo/last-test-buffer (current-buffer)))

(defun zoo/call-last-test ()
  (interactive)
  (when zoo/last-test-buffer
    (with-current-buffer zoo/last-test-buffer
      (goto-char zoo/last-test-point)
      (call-interactively zoo/last-test-function))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust stuff

;; Used to cache the buffer that trigger a test
(defvar zoo/rustic-popup-buffer-name nil)
(defvar zoo/last-call-buffer nil)

;;;###autoload
(defun zoo/rust-cargo-process-sentinel (_process msg)
  (if (string= "finished\n" msg)
      (with-current-buffer zoo/last-call-buffer
        (delete-windows-on zoo/rustic-popup-buffer-name)
        (zoo/flash-mode-line zoo/flash-mode-line-success-color 0.5))

    (with-current-buffer zoo/last-call-buffer
      (when zoo/rustic-popup-buffer-name
        (display-buffer zoo/rustic-popup-buffer-name)
        (setq zoo/rustic-popup-buffer-name nil)
        )

      (zoo/flash-mode-line zoo/flash-mode-line-failed-color 0.5))
    ))

;;;###autoload
(defun zoo/rustic-cargo-current-test ()
  "Run 'cargo test' for the test near point."
  (interactive)
  (rustic-compilation-process-live)
  (-if-let (func-name (rustic-cargo--get-current-fn-fullname))
      (let* ((command (list rustic-cargo-bin "test" func-name))
             (c (append command (split-string rustic-test-arguments)))
             (buf rustic-test-buffer-name)
             (proc rustic-test-process-name)
             (mode 'rustic-cargo-test-mode))
        ;; Show modeline color to indicate test is active
        (zoo/flash-mode-line zoo/flash-mode-line-activate-color 0.5)
        (zoo/store-last-test 'zoo/rustic-cargo-current-test)

        (setq zoo/last-call-buffer (current-buffer))
        (setq zoo/rustic-popup-buffer-name rustic-test-buffer-name)
        (save-window-excursion
          (rustic-compilation c (list :buffer buf :process proc :mode mode :sentinel 'zoo/rust-cargo-process-sentinel))))
    (message "Could not find test at point.")))

;;;###autoload
(defun zoo/rustic-cargo-test-run (&optional test-args)
  "Start compilation process for 'cargo test' with optional TEST-ARGS."
  (interactive)
  (rustic-compilation-process-live)
  (let* ((command (list rustic-cargo-bin "test"))
         (c (append command (split-string (if test-args test-args ""))))
         (buf rustic-test-buffer-name)
         (proc rustic-test-process-name)
         (mode 'rustic-cargo-test-mode))
    ;; Show modeline color to indicate test is active
    (zoo/flash-mode-line zoo/flash-mode-line-activate-color 0.5)
    (zoo/store-last-test 'zoo/rustic-cargo-test-run)

    (setq zoo/last-call-buffer (current-buffer))
    (setq zoo/rustic-popup-buffer-name rustic-test-buffer-name)
    (save-window-excursion
      (rustic-compilation c (list :no-display t :buffer buf :process proc :mode mode :sentinel 'zoo/rust-cargo-process-sentinel)))))

;;;###autoload
(defun zoo/rustic-cargo-check ()
  (interactive)
  (zoo/flash-mode-line zoo/flash-mode-line-activate-color 0.5)
  (setq zoo/last-call-buffer (current-buffer))
  (setq zoo/rustic-popup-buffer-name rustic-compilation-buffer-name)
  (rustic-run-cargo-command "cargo check" (list :no-display t :sentinel 'zoo/rust-cargo-process-sentinel)))
