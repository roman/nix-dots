(defvar cthulhu-dir
  (format "%s/Projects/cthulhu" (getenv "HOME")))

(defun zoogo/load-cthulhu ()
  (interactive)
  (let ((docode (format "%s/docode" cthulhu-dir)))
    (exec-path-from-shell-copy-envs '("PATH" "GOPATH" "MANPATH"))
    (setenv "CTHULHU_DIR" cthulhu-dir)
    (setenv "TEST_DB_USER" "root")
    (setenv "TEST_DB_ADDR" "127.0.0.1:3306")
    (setenv "TEST_CEPH_CLUSTER" "ceph")
    (setenv "TEST_CEPH_USER" "client.admin")
    (setenv "TEST_CEPH_CFG_PATH" (format "%s/ceph.conf" (getenv "HOME")))
    (setenv "GOPATH" (format "%s:%s" docode (getenv "GOPATH")))
    (setenv "PATH" (format "%s/bin:%s" docode (getenv "PATH")))))

(defvar zoogo/current-test-file nil)
(defvar zoogo/current-test-function nil)

(defvar zoogo/active-after-save-color  "#8A2BE2")
(defvar zoogo/test-success-color "#6AAF6A")
(defvar zoogo/test-failed-color  "#FF6347")

(defun zoogo/reset-stored-test-function ()
  "Resets the assigned current test to nil"
  (interactive)
  (setq zoogo/current-test-file nil)
  (setq zoogo/current-test-function nil))

(defun zoogo/store-current-test-function ()
  "Stores the current test at-point to execute later"
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (progn
        (setq zoogo/current-test-file buffer-file-name)

        (setq zoogo/current-test-function
              (save-excursion
                (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
                (concat "-run='" (match-string-no-properties 2) "$'" " " "-v"))))
    ;; else
    (message "Must be in a _test.go file to run zoogo/store-current-test-function")))

(defun zoogo/run-stored-test-function ()
  "Executes the existing stored go test function"
  (interactive)
  (when (eq major-mode 'go-mode)
    (if zoogo/current-test-function
        (with-current-buffer (get-file-buffer zoogo/current-test-file)
          (zoo/flash-mode-line zoogo/active-after-save-color 0.5)
          (message
           (format  "Running %s on file %s"
                    zoogo/current-test-function
                    zoogo/current-test-file))
          (save-window-excursion
            (spacemacs/go-run-tests zoogo/current-test-function)))
      ;; else
      (message "Must run function test in a _test.go file to store it"))))

(defun zoogo/run-test-current-function ()
  (interactive)
  (zoo/flash-mode-line zoogo/active-after-save-color 0.5)
  (zoogo/store-current-test-function)
  (save-window-excursion
    (spacemacs/go-run-test-current-function)))

(defun zoogo/toggle-run-test-stored-function-after-save ()
  (interactive)
  (zoo/toggle-after-save-hook 'zoogo/run-stored-test-function t)
  (save-window-excursion
    (ff-find-other-file)
    (zoo/toggle-after-save-hook 'zoogo/run-stored-test-function t)))

(defun zoogo/run-package-tests ()
  (interactive)
  (when (eq major-mode 'mode)
    (zoo/flash-mode-line zoogo/active-after-save-color 0.5)
    (save-window-excursion
      (spacemacs/go-run-tests "-v"))))

(defun zoogo/toggle-run-package-tests-after-save ()
  (interactive)
  (zoogo/toggle-after-save-hook 'zoo/run-package-tests))
