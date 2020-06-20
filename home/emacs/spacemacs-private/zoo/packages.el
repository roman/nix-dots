(defvar zoo-packages
  '(
    ;; (clocker :location local)
    ;; (proctor-mode :location local)
    (compile :location built-in)
    navorski
    key-chord
    savehist
    org
    magit
    vterm
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zoo/post-init-vterm ()
  (spacemacs|use-package-add-hook vterm
    :post-config
    (progn
      (when (eq dotspacemacs-editing-style 'vim)
        (evil-set-initial-state 'vterm-mode 'insert)
        (evil-define-key 'emacs vterm-mode-map
          (kbd "C-u") 'vterm-send-C-u)
        (evil-define-key 'insert vterm-mode-map
          (kbd "C-l") 'vterm-clear
          (kbd "C-r") 'vterm-send-C-r
          (kbd "C-d") 'vterm-send-C-d
          (kbd "C-e") 'vterm-send-C-e
          (kbd "C-a") 'vterm-send-C-a
          (kbd "C-u") 'vterm-send-C-u))
      )))

(defun zoo/post-init-savehist ()
  (spacemacs|use-package-add-hook savehist
    :post-config
    (progn
      (add-to-list 'savehist-additional-variables 'zoo-compile-history))))

(defun zoo/init-proctor-mode ()
  (use-package proctor-mode
    :config
    (progn)))

(defun zoo/init-key-chord ()
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-replace-state-map "jk" 'evil-normal-state))

(defun zoo/enable-term-normal-state ()
  (when (eq major-mode 'term-mode)
    (term-line-mode)))

(defun zoo/enable-term-emacs-state ()
  (when (and (eq major-mode 'term-mode)
             (get-buffer-process (current-buffer)))
    (term-char-mode)))

(defun zoo/init-navorski ()
  (use-package navorski
    :config
    (progn
      (setq-default multi-term-program
                    (or (getenv "SHELL")
                        "/bin/sh"))
      (evil-set-initial-state 'term-mode 'emacs)
      (spacemacs/set-leader-keys "'" 'nav/term)
      (add-hook 'evil-normal-state-entry-hook 'zoo/enable-term-normal-state)
      (add-hook 'evil-emacs-state-entry-hook 'zoo/enable-term-emacs-state)
      )))

(defun zoo/pre-init-magit ()
  (spacemacs|use-package-add-hook magit
    :post-config
    (progn
      (setq magit-display-buffer-function
            (lambda (buffer)
              (display-buffer
               buffer (if (and (derived-mode-p 'magit-mode)
                               (memq (with-current-buffer buffer major-mode)
                                     '(magit-process-mode
                                       magit-revision-mode
                                       magit-diff-mode
                                       magit-stash-mode
                                       magit-status-mode)))
                          nil '(display-buffer-same-window))))))))

(defun zoo/pre-init-compile ()
  (spacemacs|use-package-add-hook compile
    :post-config
    (progn
      (add-hook 'compilation-finish-functions
                'zoo/handle-compilation-result)
      (spacemacs/set-leader-keys
        "cs" 'zoo/show-compilation-window
        "cg" 'zoo/goto-compilation-window
        "ct" 'zoo/toggle-hide-on-compile-success)
      )))

(defun zoo/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (progn
      (require 'org-tempo)
      (with-eval-after-load 'org
        (setq org-hide-leading-stars t)
        (setq org-show-following-heading t)
        (setq org-show-hierarchy-above t)

        ;; ctrl-a/e will respect org-mode entries
        ;; jump to the start of the headline
        (setq org-special-ctrl-a/e t)

        ;; respect tags when killing line on a heading
        (setq org-special-ctrl-k t)
        (setq org-return-follows-link t)

        ;; Save the clock and entry when I close emacs
        (setq org-clock-persist t)

        ;; Check a clock that was left behind open when
        ;; starting emacs
        (org-clock-persistence-insinuate)

        ;; Store at least 35 clocks in memory
        (setq org-clock-history-length 35)

        ;; When clocking in, change the status of the item to
        ;; STARTED
        (setq org-clock-in-switch-to-state "STARTED")

        ;; When clocking out, change the status of the item to
        ;; PAUSED
        (setq org-clock-out-switch-to-state nil)


        ;; Have a special :LOGBOOK: drawer for clocks
        (setq org-clock-into-drawer "CLOCK")

        ;; Don't register clocks with zero-time length
        (setq org-clock-out-remove-zero-time-clocks t)

        ;; Stop clock when a task gets to state DONE.
        (setq org-clock-out-when-done t)

        ;; Resolve open-clocks if iddle more than 30 minutes
        (setq org-clock-idle-time 10)

        ;; Activate single letter commands at the beginning of
        ;; a headline
        (setq org-use-speed-commands t)

        ;; when changing the item to DONE, Don't add anything
        (setq org-log-done nil)

        ;; Add all notes and timestamps to the LOGBOOK drawer
        (setq org-log-into-drawer "LOGBOOK")
        (setq org-log-state-notes-into-drawer t)

        ;; When task is refilled, rescheduled or redeadline add
        ;; a timestamp to the task
        (setq org-log-refile 'time)
        (setq org-log-reschedule 'time)
        (setq org-log-redeadline 'time)


        (setq org-log-note-headings
              '((done .  "CLOSING NOTE %t")
                (state . "State %-12s from %-12S %t")
                (note .  "Note taken on %t")
                (reschedule .  "Rescheduled from %S on %t")
                (delschedule .  "Not scheduled, was %S on %t")
                (redeadline .  "New deadline from %S on %t")
                (deldeadline .  "Removed deadline, was %S on %t")
                (refile . "Refiled from %s to %S on %t")
                (clock-out . "")))

        (setq org-done-keywords
              '("DONE" "CANCELLED"))

        ;; Avoid adding a blank line after doing alt-return on an entry.
        (setq org-blank-before-new-entry '((heading . auto)
                                           (plain-list-item . auto)))

        ;; When hitting alt-return on a header, please create a new one without
        ;; messing up the one I'm standing on.
        (setq org-insert-heading-respect-content t)

        ;; Avoid adding a blank line after doing alt-return on an entry.
        (setq org-blank-before-new-entry
              '((heading . auto)
                (plain-list-item . auto)))

        ;; When hitting alt-return on a header, please create a new one without
        ;; messing up the one I'm standing on.
        (setq org-insert-heading-respect-content t)

        ;; Avoid setting entries as DONE when there are still sub-entries
        ;; that are not DONE.
        (setq org-enforce-todo-dependencies t)

        ;; Allow to iterate easily between todo-keywords using meta->/meta-<
        (setq org-use-fast-todo-selection t)
        (setq org-treat-S-cursor-todo-selection-as-state-change nil)

        ;; States that a todo can have
        (setq org-todo-keywords
              '((sequence "TODO(t)" "TODAY(y!)" "|" "STARTED(s!)" "|" "PAUSED(p!)"
                          "|" "DONE(d!/!)")
                (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")))

        ;; (setq org-todo-keywords
        ;;       '((sequence  "DONE(d!/!)" "|" "REVIEW(r!)" "|" "IN-PROGRESS(p!)" "|" "BACKLOG(b!)" )
        ;;         ;; (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")
        ;;         ))


        ;; Pretty styling for the different keywords of a TODO item
        (setq org-todo-keyword-faces
              '(
                ("TODO" :foreground "#280339" :background "#9974AA" :box (:line-width 2) :weight bold)
                ("DONE" :foreground "#2E6E12" :background "#A8DB92" :box (:line-width 2) :weight bold)
                ("STARTED" :foreground "#0D407A" :background "#7FA8D6" :box (:line-width 2) :weight bold)
                ("PAUSED" :foreground "#FF7C00" :background "#FFD2A7" :box (:line-width 2) :weight bold)
                ("WAITING" :foreground "#A07D00" :background "#FFDD65" :box (:line-width 2) :weight bold)
                ("CANCELLED" :foreground "#9D0008" :background "#FC646B" :box (:line-width 2) :weight bold)))



        ;; BABEL supported languages
        (setq org-babel-load-languages
              '((clojure . t)
                (haskell . t)
                (sql . t)
                (emacs-lisp . t)))

        (add-hook 'org-insert-heading-hook
                  'zoo/org-insert-heading-hook)

        (add-hook 'org-clock-in-prepare-hook
                  'zoo/org-mode-ask-effort)

        (add-hook 'org-clock-out-hook
                  'zoo/org-clock-out-hook)

        (add-hook 'org-after-todo-state-change-hook
                  'zoo/org-after-todo-state-change-hook)

        ;; (add-hook 'org-after-demote-entry-hook
        ;;           'zoo/org-after-demote-entry-hook)

        (add-hook 'org-after-promote-entry-hook
                  'zoo/org-after-promote-entry-hook)

        (global-set-key (kbd "<f8> i") 'org-clock-in)
        (global-set-key (kbd "<f8> o") 'org-clock-out)
        (global-set-key (kbd "<f8> l") 'zoo/org-clock-in-last)
        (global-set-key (kbd "<f8> -") 'org-clock-goto)))))
