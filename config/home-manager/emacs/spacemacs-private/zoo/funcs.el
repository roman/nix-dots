;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; After Save Hook

(defvar zoo/evil-state-before-snippet nil)

(defun zoo/-get-hook-funcs (hook)
  (delq nil (mapcar
             (lambda (e) (if (symbolp e) e))
             hook)))

(defun zoo/-get-hook-funcs-names (hook)
  (mapcar 'symbol-name
          (zoo/-get-hook-funcs
           (if (symbolp hook)
               (append (symbol-value hook)
                       (default-value hook))
             hook))))

(defun zoo/-get-all-hooks ()
  (let (hlist (list))
    (mapatoms (lambda (a)
                (if (and (not (null (string-match ".*-hook"
                                                  (symbol-name a))))
                         (not (functionp a)))
                    (add-to-list 'hlist a))))
    hlist))

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

(defun zoo/remove-after-save-hook (fname &optional local)
  (interactive (list (ido-completing-read
                      "aWhich function: "
                      (zoo/-get-hook-funcs-names 'after-save-hook))))
  (zoo/remove-from-hook 'after-save-hook fname local))

(defun zoo/add-after-save-hook (fname &optional local)
  (interactive "aWhich function: ")
  (add-hook 'after-save-hook fname t local))

(defun zoo/check-after-save-hook (fname)
  (-contains? (zoo/-get-hook-funcs-names 'after-save-hook) (symbol-name fname)))

(defvar zoo/after-save-hook-activated-color "#6AAF6A")
(defvar zoo/after-save-hook-disabled-color  "#D70000")

(defun zoo/toggle-after-save-hook (fname &optional local)
  (if (zoo/check-after-save-hook fname)
      (progn
        (zoo/flash-mode-line zoo/after-save-hook-disabled-color 0.5)
        (zoo/remove-after-save-hook fname local))

      ;; else
    (zoo/flash-mode-line zoo/after-save-hook-activated-color 0.5)
    (zoo/add-after-save-hook fname local)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar zoo/active-after-save-color  "#8A2BE2")
(defvar zoo/test-success-color "#6AAF6A")
(defvar zoo/test-failed-color  "#FF6347")


(defvar zoo-mode-line-in-use nil
  "Private var to check usage of mode-line before changing")

(defvar zoo-mode-line-usage-delay 0.5
  "Number of secs before trying to modify the mode-line again.")

(defun zoo/restore-modeline (orig-modeline-fg)
  (set-face-background 'mode-line orig-modeline-fg)
  (setq zoo-mode-line-in-use nil))

(defun zoo/flash-mode-line (&optional color time)
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

;; from reddit discussion
;; https://www.reddit.com/r/emacs/comments/5aio50/align_text_with_alignregexp/
(defun zoo/align-tuples (start end)
  "Vertically aligns region based on lengths of the first value of each line.

Example output:

    foo       bar
    foofoo    bar
    foofoofoo bar"
  (interactive "r")
  (align-regexp start end "\\S-+\\(\\s-+\\)" 1 2 nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zoo/get-cli-output (cmd args)
  (s-trim (with-temp-buffer
            (apply #'call-process cmd nil t nil args)
            (goto-char (point-min))
            (buffer-string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zoo/navorski-terminal-line-mode ()
  (interactive)
  (when (term-in-char-mode)
    (term-line-mode)
    (linum-mode 1))
  (when (evil-emacs-state-p)
    (evil-normal-state)))

(defun zoo/navorski-terminal-char-mode ()
  (interactive)
  (when (evil-normal-state-p)
    (evil-emacs-state))
  (when (term-in-line-mode)
    (term-char-mode)
    (linum-mode 0)))

(defun zoo/navorski-terminal-toggle-mode ()
  (interactive)
  (cond
   ;; on char mode
   ((term-in-char-mode)
    (progn
      (term-line-mode)
      (linum-mode 1)))

   ;; on line mode
   ((term-in-line-mode)
    (progn
      (term-char-mode)
      (evil-emacs-state)
      (linum-mode 0)))

   ;; else
   (t nil)))

(defun zoo/set-navorski-keybidings ()
  (evil-local-set-key
   'emacs
   (kbd "<f7> n") 'zoo/navorski-terminal-line-mode)
  (evil-local-set-key
   'normal
   (kbd "<f7> e") 'zoo/navorski-terminal-char-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'org

  (defun zoo/org-mode-ask-effort ()
    (unless (org-entry-get (point) "Effort")
      (let ((effort
             (completing-read "Effort: " (org-entry-get-multivalued-property (point) "Effort"))))
        (unless (equal effort "")
          (org-set-property "Effort" effort)))))

  (defun zoo/org-current-timestamp ()
    (let ((fmt (concat
                "<" (substring (cdr org-time-stamp-formats) 1 -1) ">")))
      (format-time-string fmt)))

  (defun zoo/org-current-clock-id ()
    "Get the id of the current item being clocked."
    (save-window-excursion
      (save-excursion
        (org-clock-goto)
        (org-id-get-create))))

  (defun zoo/org-clocking-p ()
    (interactive)
    (and (fboundp 'org-clocking-p)
         (org-clocking-p)))

  (defun zoo/org-insert-heading-hook ()
    (interactive)
    ;; Create an ID for the current item
    (org-id-get-create)
    (org-set-property "CREATED"
                      (zoo/org-current-timestamp))
    (when (zoo/org-clocking-p)
      ;; ^ If a clock is active, add a reference to the task
      ;; that is clocked in
      (org-set-property "CLOCK-WHEN-CREATED"
                        (zoo/org-current-clock-id))))

  (defun zoo/org-after-demote-entry-hook ()
    (interactive)
    (org-delete-property "ID")
    (org-delete-property "CREATED")
    (ignore-errors (org-delete-property "CLOCK-WHEN-CREATED"))
    (org-remove-empty-drawer-at (point)))

  (defun zoo/org-after-promote-entry-hook ()
    (interactive)
    (when (eq (org-current-level) 1)
      (org-id-get-create)
      (org-set-property "CREATED"
                        (zoo/org-current-timestamp))
      (when (zoo/org-clocking-g)
        ;; ^ If a clock is active, add a reference to the task
        ;; that is clocked in
        (org-set-property "CLOCK-WHEN-CREATED"
                          (zoo/org-current-clock-id)))))

  (defun zoo/org-clock-out-hook ()
    (org-todo "PAUSED"))

  (defun zoo/org-after-todo-state-change-hook ()
    (when (string= org-state "DONE")
      (org-set-property "COMPLETED"
                        (zoo/org-current-timestamp))))

  (defun zoo/org-is-last-task-started-p ()
    (interactive)
    (save-window-excursion
      (org-clock-goto)
      (let ((state (org-get-todo-state)))
        (string= state "IN-PROGRESS"))))

  (defun zoo/org-clock-in-last ()
    (interactive)
    (if (zoo/org-is-last-task-started-p)
        (org-clock-in-last)
      (message "ignoring org-clock-in-last"))))
;; Always returns to the point where we started the snippet once we finish
;; completing it.
(defun zoo/yasnippet-after-exit-snippet (exit-snippet-fn &rest args)
  (let ((result (apply exit-snippet-fn args)))
    (when zoo/evil-state-before-snippet
      (avy-pop-mark)
      (evil-change-state zoo/evil-state-before-snippet)
      (setq zoo/evil-state-before-snippet nil))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compile after save utilities

(defvar zoo-compile-history nil)
(defvar zoo-compile-command nil)
(defvar zoo-compile-in-progress nil)
(defvar zoo-compile-registered-major-mode nil)

(defun zoo/set-compile-command (cmd)
  (interactive
   (list
    (read-string "Command: " zoo-compile-command zoo-compile-history)))
  (setq zoo-compile-command cmd)
  (setq zoo-compile-registered-major-mode major-mode)
  zoo-compile-command)

(defun zoo/compile ()
  (interactive)
  (when (eq major-mode zoo-compile-registered-major-mode)
    (unwind-protect
        (progn
          (if (and zoo-compile-command
                   (not zoo-compile-in-progress))
              (progn
                (setq zoo-compile-in-progress t)
                (let ((default-directory (projectile-project-root)))
                  (compile zoo-compile-command)))
            ;; else
            (message "Set command via 'zoo/set-compile-command' before using this command")))
      ;; on error
      (progn
        (setq zoo-compile-in-progress nil)))))

(defun zoo/toggle-compile-on-save ()
  (interactive)
  (if zoo-compile-command
      (progn
        (message "Disable compile on save")
        (zoo/remove-after-save-hook 'zoo/compile)
        (setq zoo-compile-command nil))
    ;; else
    (progn
      (or zoo-compile-command
          (call-interactively 'zoo/set-compile-command))
      (zoo/add-after-save-hook 'zoo/compile)
      (message "Enable compile on save"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compile mode modeline flash

(defvar zoo/hide-on-compile-success nil)
(defvar zoo/compile-success-color "#6AAF6A")
(defvar zoo/compile-failed-color  "#D70000")

(defun zoo/toggle-hide-on-compile-success ()
  (interactive)
  (setq zoo/hide-on-compile-success
        (if zoo/hide-on-compile-success
            nil
          ;; else
          t)))

(defun zoo/show-compilation-window ()
  (interactive)
  (when compilation-last-buffer
    (display-buffer compilation-last-buffer)))

(defun zoo/goto-compilation-window ()
  (interactive)
  (when compilation-last-buffer
    (pop-to-buffer compilation-last-buffer)))

(defun zoo/handle-compilation-result (buff msg)
  (let ((w (get-buffer-window buff)))
    (if (string= msg "finished\n")
        (progn
          (when zoo/hide-on-compile-success
            (when w
              (delete-window w)))
          (zoo/flash-mode-line zoo/test-success-color 1))
      ;; else
      (progn
        (zoo/flash-mode-line zoo/test-failed-color 1)
        (unless w
          (display-buffer buff)))
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Utilities

(defun zoo/main-frame-layout ()
  (interactive)
  ;; execute on the current frame

  (evil-write-all nil)
  (delete-other-windows)

  (let ((term-buffer
         (or (get-buffer "*Default-vterm-1*")
             (spacemacs/default-pop-shell))))
    (switch-to-buffer term-buffer))

  (split-window)

  (let ((today-org-filepath
         (format "%s/Projects/dotfiles/org/week/%s"
                 (getenv "HOME")
                 (s-downcase
                  (format-time-string "%Y-%m-%d_%a.org")))))

    (if (file-exists-p today-org-filepath)
        ;; Lookup the file if it is already in the fs
        (find-file today-org-filepath)
      ;; else
      ;; copy the file from the previous day
      (let* ((now (decode-time))
             (prev-day (copy-sequence now))
             (now-weekday (format-time-string "%u"))
             (_
              (cond
               ;; on monday, use friday
               ((string= now-weekday "1")
                (cl-incf (nth 3 prev-day) -3))
               ;; on sunday, use friday
               ((string= now-weekday "7")
                (cl-incf (nth 3 prev-day) -2))
               ;; any other day, use yesterday
               (t
                (cl-incf (nth 3 prev-day) -1))))

             (prev-day-org-filepath
              (format "%s/Projects/dotfiles/org/week/%s"
                      (getenv "HOME")
                      (s-downcase
                       (format-time-string "%Y-%m-%d_%a.org"
                                           (apply #'encode-time prev-day))))))

        ;; Copy the file from previous day if present
        (when (file-exists-p prev-day-org-filepath)
            (copy-file prev-day-org-filepath today-org-filepath))

        (find-file today-org-filepath)))

    ;; (evil-window-move-very-bottom)
    (enlarge-window -10)
    (set-window-dedicated-p (get-buffer-window (current-buffer))
                            t)))


(defun zoo/org-insert-toc ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    ;; org-insert-heading doesn't work on an empty file for some reason
    (insert "\n")
    (org-insert-heading "Table Of Contents" nil t)
    (org-edit-headline "Table Of Contents")
    (org-set-tags ":TOC:"))
  (end-of-buffer))

(defun zoo/org-get-topmost-parent-name ()
  (while (not (= 1 (org-current-level)))
    (org-previous-visible-heading 1))
  (org-entry-get nil "ITEM"))

(defvar zoo-backup-ticket-prefix "ORCA-")

(defvar zoo-backup-tickets-dir
  "/home/roman/Projects/notes/tickets/")

(defun zoo/backup-tickets ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((current-point nil)
          (at-end nil))
      (while (not at-end)
        (setq current-point (point))
        (outline-next-visible-heading 1)
       (zoo/backup-ticket)
        (outline-next-visible-heading 1)
        (setq at-end (eq current-point (point)))))))

(defun zoo/backup-ticket ()
  (interactive)
  (save-excursion
    (let ((heading-title (zoo/org-get-topmost-parent-name)))
      (when (s-starts-with? zoo-backup-ticket-prefix heading-title)
        (org-copy-subtree)
        (let ((org-file (f-join zoo-backup-tickets-dir
                                (f-swap-ext heading-title "org"))))

          (if (file-exists-p org-file)
              (progn
                (delete-file org-file)
                (message (format "Recreating file %s" org-file)))
            ;; else
            (format "Creating new file %s" org-file))

          (save-window-excursion
            (find-file org-file)
            (zoo/org-insert-toc)
            (newline)
            (org-paste-subtree)
            (save-buffer)
            (kill-buffer)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-pomodoro kick sound

(defvar zoo/org-pomodoro-kick-sound-file
  "~/Projects/dotfiles/nix/home-manager/kick_short_fade.mp3")

(defun zoo/org-pomodoro-play-kick-sound ()
  (interactive)
  (start-process-shell-command
   "org-pomodoro-kick-audio-player" nil
   (format "mpg123 %s" (shell-quote-argument (expand-file-name zoo/org-pomodoro-kick-sound-file)))))

(defun zoo/org-pomodoro-maybe-play-kick-sound ()
  (let ((seconds (org-pomodoro-remaining-seconds)))
    (when (= seconds 60)
      (zoo/org-pomodor-play-kick-sound))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git link utility

(defun zoo/git-permalink ()
  (interactive)
  (let ((git-link-use-commit t))
    (call-interactively 'git-link)))

(defun zoo/git-permalink-copy-url-only ()
  "Only copy the generated link to the kill ring."
  (interactive)
  (let ((git-link-use-commit t)
        (git-link-open-in-browser))
    (call-interactively 'git-link)))
