;;; emacs/zoo/config.el -*- lexical-binding: t; -*-

(defun +zoo/toggle-to-error-list ()
  (interactive)
  (let ((buffer-name "*Flycheck errors*"))
    ;; when error list is on a window
    (if-let (win (get-buffer-window buffer-name))
        ;; when error list is on a window
        (if (eq (selected-window) win)
            ;; when sitting on error list window, close it
            (delete-window win)
          ;; otherwise, select that window
          (progn
            (select-window win)
            (goto-char (point-max))))
      ;; otherwise; look for the hidden buffer
      (if-let (buffer (get-buffer "*Flycheck errors*"))
          ;; when buffer exists, pop it out
          (progn
            (pop-to-buffer buffer))
        ;; otherwise; create the error list via flycheck method
        ;; and then pop it out
        (progn
          (save-window-excursion (flycheck-list-errors))
          (pop-to-buffer (get-buffer "*Flycheck errors*")))))))

(defun +zoo/recompile-and-switch-on-error ()
  (interactive)
  (save-window-excursion
    (recompile))
  ;; I'm reverse engineering projectile's compilation-buffer-name function as it
  ;; is not obvious what the compilation-mode variable is to use the function
  ;; directly
  (let* ((buffer-name (concat "*compilation*<" (projectile-project-name) ">"))
        (buffer (get-buffer buffer-name)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (goto-char (point-min)))))

(map! :leader
      :desc "Add after-save hook" "k a" 'zoo/add-after-save-hook-kbd
      :desc "Remove after-save hook" "k r" 'zoo/remove-after-save-hook
      :desc "Toggle last after-save hook" "k t" 'zoo/toggle-last-after-save-hook
      :desc "Recompile and switch to buffer" "c C" '+zoo/recompile-and-switch-on-error
      :desc "Toogle Flycheck Error popup" "o e" '+zoo/toggle-to-error-list)

(when (and (featurep! :lang rust)
           (featurep! :emacs zoo +rust))
  (after! rustic
    ;; remove popup-rule of rustic-compilation
    (setq display-buffer-alist
          (delq (assoc "^\\*rustic-compilation" display-buffer-alist) display-buffer-alist)
          )
    ;; and replace it with something that makes more sense
    (set-popup-rule! "^\\*rustic-compilation" :side 'right :width 0.5 :modeline t)
    (map! :map rustic-mode-map
          :localleader
          :desc "(enhanced) last test run" "," 'zoo/rustic-last-test-call
          (:prefix ("b" . "build")
           :desc "cargo check" "c" #'zoo/rustic-cargo-check
           )
          (:prefix ("t" . "cargo test")
           :desc "all"          "a" #'zoo/rustic-cargo-test-run
           :desc "current test" "t" #'zoo/rustic-cargo-current-test)))
  )
