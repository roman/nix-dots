;;; proctor-mode.el --- Test supervisor and enforcer.

;; Copyright 2012 Birdseye Software.

;; Authors: <tavis at birdseye-sw com>, <roman at birdeseye-sw com>
;; Version: 0.0.1
;; Package-version: 0.0.1
;; Package-Requires: ((multi-term "0.8.7")
;;                    (notify ""))
;; Keywords: tooling, testing
;; URL: http://github.com/BirdseyeSoftware/proctor-mode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;; Comentary:
;;
;; This is a work in progress
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tramp)
(require 'multi-term)
;; (require 'notify)

(defgroup proctor-mode '()
  "proctor-mode: Supervising and enforcing your \"test-driven\" since 2013")

(defvar -proctor-mode-mode-line-in-use nil
  "Private var to check usage of mode-line before changing")

(defvar -proctor-mode-mode-line-usage-delay 0.5
  "Number of secs before trying to modify the mode-line again.")

(defvar proctor-mode-buffer nil
  "Contains the buffer created by proctor")

(defvar proctor-server-buffer nil
  "Contains the buffer created by proctor/run-server")

(defcustom proctor-mode-buffer-name "proctor"
  "Specifies the name of the buffer created by proctor (the name
is going to contain earmufs later on)."
  :type 'string
  :group 'proctor-mode)

(defcustom proctor-screen-session-name "proctor"
  "Name of the GNU screen session name created for the proctor
terminal"
  :type 'string
  :group 'proctor-mode)

(defcustom proctor-mode-command nil
  "Command that gets executed. This variable is going to be buffer-local
if setted with `proctor/set-command'."
  :group 'proctor-mode)

(defcustom proctor-mode-working-directory nil
  "The directory where the command test is executed."
  :type 'string
  :group 'proctor-mode)

(defcustom proctor-mode-before-test-hook '()
  "Hook that is called before a test execution."
  :group 'proctor-mode)

(defcustom proctor-mode-after-test-hook '()
  "Hook that is called after a test execution."
  :group 'proctor-mode)

(defcustom proctor-mode-success-test-hook '()
  "Hook that is called after a successful test execution."
  :group 'proctor-mode)

(defcustom proctor-mode-fail-test-hook '()
  "Hook that is called after a failed test execution."
  :group 'proctor-mode)

(defcustom proctor-mode-success-message nil
  "String that will be shwon in the notification when tests
  execution is successful."
  :type 'string
  :group 'proctor-mode)

(defcustom proctor-mode-failure-message nil
  "String that will be shwon in the notification when tests
  execution fails."
  :type 'string
  :group 'proctor-mode)

(defcustom proctor-mode-popup-buffer-on-failure nil
  "If true, pops out the proctor buffer with failure output."
  :type 'bool
  :group 'proctor-mode)


(defface proctor-mode-failure-face
  '((((class color) (background light))
     :background "orange red") ;; TODO: Hard to read strings over
    (((class color) (background dark))
     :background "firebrick"))
  "Face for failures in proctor-mode."
  :group 'proctor-mode)

(defface proctor-mode-warning-face
    '((((class color) (background light))
            :background "orange1")
          (((class color) (background dark))
                :background "orange4"))
      "Face for errors in proctor-mode."
        :group 'proctor-mode)

(defface proctor-mode-success-face
  '((((class color) (background light))
     :foreground "black"
     :background "green")
    (((class color) (background dark))
     :foreground "black"
     :background "green"))
  "Face for success in proctor-mode."
  :group 'proctor-mode)

;; Hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun proctor/add-before-test-hook (fname)
  (interactive "sWhich function: ")
  (add-hook 'proctor-mode-before-test-hook fname t t))

(defun proctor/add-after-test-hook (fname)
  (interactive "sWhich function: ")
  (add-hook 'proctor-mode-after-test-hook fname t t))

(defun proctor/add-success-test-hook (fname)
  (interactive "sWhich function: ")
  (add-hook 'proctor-mode-success-test-hook fname t t))

(defun proctor/add-fail-test-hook (fname)
  (interactive "sWhich function: ")
  (add-hook 'proctor-mode-fail-test-hook fname t t))

;; Setting test command / function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun proctor/set-function (fname)
  "Tell proctor which elisp function to run after save.

   NOTE: you will have to call `proctor/success' or
   `proctor/error' accordingly in your function."
  (interactive "aWhich function: ")
  (make-local-variable 'proctor-mode-command)
  (setq proctor-mode-command fname))

(defun proctor/set-command (command)
  "Tell proctor which command to run after save."
  (interactive
   (list
    (read-string "Command: " proctor-mode-command)))
  (make-local-variable 'proctor-mode-command)
  (setq proctor-mode-command command)
  command)

(defun proctor/reset-command ()
  "Remove command on buffer"
  (interactive)
  (setq proctor-mode-command nil))

;; Navigation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun proctor/switch-to-buffer ()
  "Switch back and forth between proctor and the previous buffer"
  (interactive)
  (if (string= (format "*%s*" proctor-mode-buffer-name) (buffer-name))
      (switch-to-buffer (other-buffer))
    (switch-to-buffer (format "*%s*" proctor-mode-buffer-name))))

;; Notifications ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -proctor-mode-restore-mode-line (orig-mode-line-fg)
  (set-face-background 'mode-line orig-mode-line-fg)
  (setq -proctor-mode-mode-line-in-use nil))

(defun -proctor-mode-flash-mode-line (&optional time color)
  (interactive)
  (if (not -proctor-mode-mode-line-in-use)
      (let ((orig-mode-line-fg (face-background 'mode-line))
            (time (or time 2))
            (color (or color "#d70000")))
        (setq -proctor-mode-mode-line-in-use t)
        (set-face-background 'mode-line color)
        (run-with-timer time nil
                        '-proctor-mode-restore-mode-line
                        orig-mode-line-fg))
    (run-with-timer -proctor-mode-mode-line-usage-delay nil
                    '-proctor-mode-flash-mode-line
                    time
                    color)))

(defun proctor/notify (header msg &optional type)
  "Prints a message using notify & message."
  (when (fboundp 'notify)
    (notify header msg))
  (let ((full-msg (format "%s - %s" header msg)))
    (message (propertize full-msg
                         'face
                         (cond
                          ((not type) nil)
                          ((eq type 'succcess) 'proctor-mode-success-face)
                          ((eq type 'failure)  'proctor-mode-failure-face)
                          ((eq type 'warning)  'proctor-mode-warning-face)
                          (t nil))))))

(defun proctor/begin-notification ()
  "Notifies when a test is about to run."
  (interactive)
  (-proctor-mode-flash-mode-line 0.3 "purple")
  ;; (proctor/notify "proctor-mode"
  ;;         (format "TESTING: %s"
  ;;                 proctor-mode-command))
  )

(defun proctor/succeed ()
  "Notifies when a test succeeded."
  (interactive)
  (-proctor-mode-flash-mode-line 0.6 "green")
  ;; (proctor/notify "proctor-mode"
  ;;                 (or (format "SUCCESS: %s"
  ;;                             proctor-mode-command)
  ;;                     proctor-mode-success-message)
  ;;                 'success)
  )

(defun proctor/warning (&optional msg)
  "Notifies when a there is some sort of error/warning."
  (interactive)
  (-proctor-mode-flash-mode-line 0.6 "yellow")
  (when msg
    (proctor/notify "proctor-mode" msg 'warning)))

(defun proctor/fail (&optional buffername)
  "Notifies when the test failed. "
  (interactive)
  (-proctor-mode-flash-mode-line 0.6 "red")
  ;; (proctor/notify "proctor-mode"
  ;;                  (or proctor-mode-failure-message
  ;;                      (format "FAILURE: %s"
  ;;                              proctor-mode-command))
  ;;                  'failure)
  )

;; Process Filter setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -proctor-mode-decorate-process-filter (term-proc)
  (set-process-filter term-proc
                      (-proctor-mode-process-filter (process-filter term-proc))))

(defun -proctor-mode-process-filter (orig-filter)
  `(lambda (term-proc output)
     (cond
      ((string-match "proctor_mode_test_success" output)
       (progn
         (proctor/succeed)
         (run-hooks 'proctor-mode-after-success-hook)))
      ;;
      ((string-match "proctor_mode_test_fail" output)
       (progn
         (proctor/fail)
         (when proctor-mode-popup-buffer-on-failure
           (pop-to-buffer proctor-mode-buffer))
         (run-hooks 'proctor-mode-after-fail-hook))))
     ;; call lower level process filter
     (run-hooks 'proctor-mode-after-test-hook)
     (if (fboundp #',orig-filter) (funcall ',orig-filter term-proc output))))

;; Buffer management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun -proctor-mode-remote-info ()
  (when (tramp-tramp-file-p default-directory)
    (let* ((tramp-info  (tramp-dissect-file-name default-directory))
           (remote-port (number-to-string (tramp-file-name-port tramp-info)))
           (remote-host (format "%s@%s"
                                (tramp-file-name-user tramp-info)
                                (tramp-file-name-real-host tramp-info))))
      `(("remote-port" . ,remote-port)
        ("remote-host" . ,remote-host)))))

(defun -proctor-mode-create-buffer (buffer-name)
  (or
   (get-buffer (format "*%s*" buffer-name))
   (let* ((multi-term-dedicated-buffer-name buffer-name)
          (term-buffer
           (progn
             (save-window-excursion
               (multi-term-dedicated-open))
             (get-buffer (format "*%s*" buffer-name)))))
     (with-current-buffer term-buffer
       (term-send-raw-string "
proctor_prefix=\"proctor_mode\"
function proctor_mode_check_test_result {
  if [[ $? == 0 ]]; then
    echo \"${proctor_prefix}_test_success\"
  else
    echo \"${proctor_prefix}_test_fail\"
  fi
  return $?
}\n"))
     ;; making buffer and command local to this buffer only
     (-proctor-mode-decorate-process-filter (get-buffer-process term-buffer))
     (display-buffer term-buffer)
     term-buffer)))

;; Main functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -proctor-mode-get-working-directory (directory-path)
  ;; if the file is remote, get the dir path without the
  ;; tramp remote protocol crap
  (if (tramp-tramp-file-p directory-path)
      (tramp-file-name-localname
       (tramp-dissect-file-name directory-path))
    directory-path))

(defun -proctor-mode-cd-to-working-directory (buffer)
  (let* ((proctor-mode-working-directory (-proctor-mode-get-working-directory
                                           (or proctor-mode-working-directory
                                               default-directory))))
    (with-current-buffer buffer
      (term-send-raw-string (format "cd %s\n" proctor-mode-working-directory)))))

(defun -proctor-mode-execute-command (buffer command)
  (if (not command)
      (proctor/warning "ERROR: M-x `proctor/set-command' to configure proctor")
    (progn
      (proctor/begin-notification)
      (cond
       ;; this is a function
       ((symbolp command)
        (funcall command))
       ((stringp command)
        (with-current-buffer buffer
          (term-send-raw-string (format "{ %s; proctor_mode_check_test_result; }\n"
                                        command))))))))

(defun -proctor-mode-execute-server-command (buffer command)
  (proctor/begin-notification)
  (with-current-buffer buffer
    (term-send-raw-string (format "{ %s; exit; }\n"
                                  command))))

(defun proctor/kill-buffer ()
  "Kill proctor buffer."
  (interactive)
  (with-current-buffer proctor-mode-buffer
    (term-send-raw-string "exit\n"))
  (setq proctor-mode-buffer nil)
  (proctor/disable-after-save))

(defun proctor/run-tests ()
  "Manually run tests."
  (interactive)
  (setq proctor-mode-buffer
        (-proctor-mode-create-buffer proctor-mode-buffer-name))
  (-proctor-mode-cd-to-working-directory proctor-mode-buffer)
  (-proctor-mode-execute-command
   proctor-mode-buffer
   (or proctor-mode-command
       (run-interactively' proctor/set-command))
   ))

(defun proctor/run-server (cmd &optional server-buffer-name)
  (interactive
   (list
    (read-string "Command: ")))
  (let* ((server-buffer-name (or server-buffer-name "server")))
    (setq proctor-server-buffer
          (-proctor-mode-create-buffer server-buffer-name))
    (-proctor-mode-cd-to-working-directory proctor-server-buffer)
    (-proctor-mode-execute-server-command proctor-server-buffer cmd)))

;; After save utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun proctor/enable-after-save ()
  "Start supervising file saves to run tests."
  (interactive)
  (add-hook 'after-save-hook 'proctor/run-tests t t)
  (proctor/notify "proctor-mode" "Supervising and enforcing!"))

(defun proctor/disable-after-save ()
  "Stop supervising file saves to run tests."
  (interactive)
  (remove-hook 'after-save-hook 'proctor/run-tests t)
  (proctor/notify "proctor-mode" "Supervision is over"))

(defalias 'proctor/on 'proctor/enable-after-save)
(defalias 'proctor/off 'proctor/disable-after-save)
(defalias 'proctor/start 'proctor/enable-after-save)
(defalias 'proctor/stop 'proctor/disable-after-save)


(provide 'proctor-mode)
;;; proctor-mode.el ends here
