;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                            Haskell Mode Functions                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun zoohs/fetch-module-point ()
  (save-excursion
    (beginning-of-buffer)
    (when (re-search-forward "^module" nil t)
      (match-beginning 0)
      )
    )
  )

(defun zoohs/fetch-first-import-point ()
  (save-excursion
    (beginning-of-buffer)
    (when (re-search-forward "^import" nil t)
      (match-beginning 0)
      )
    )
  )

(defun zoohs/fetch-first-pragma-point ()
  (save-excursion
    (beginning-of-buffer)
    (when (re-search-forward "^{-#" nil t)
      (beginning-of-line)
      (point)
      )
    )
  )

(defun zoohs/stack-script-point-multiline-p ()
  (save-excursion
    (not (not (re-search-forward "{-[\n\t ]+stack" nil t)))
    )
  )

(defun zoohs/fetch-multiline-stack-script-next-line ()
  (save-excursion
    (if-let ((last-point (re-search-forward "-}" nil t)))
        (progn
          (forward-line 1)
          (point)
          )
      ;; else
      (error "Invalid multiline stack script section; could not find '-}'")
      )
    )
  )

(defun zoohs/fetch-stack-script-point ()
  (save-excursion
    (beginning-of-buffer)
    (when (or (re-search-forward "{-[\n\t ]+stack" nil t)
              (re-search-forward "--[[:blank:]]+stack" nil t))
      (match-beginning 0)
      )
    )
  )

(defun zoohs/fetch-pragma-section-point ()
  (save-excursion
    ;; search for another pragma; else, the module section, else the import section
    (if-let ((pragma-section-point (or (zoohs/fetch-first-pragma-point)
                                       (zoohs/fetch-module-point)
                                       (zoohs/fetch-first-import-point))))

        (progn
          (goto-char pragma-section-point)
          (forward-line -1)
          (point)
          )

      ;; else; everything else fails, look for a stack script section
      (if-let ((script-section-point (and (zoohs/fetch-stack-script-point)
                                          (match-beginning 0)) ))
          (progn
            (goto-char script-section-point)
            ;; if it is multiline, we need to go to the line after the '-}' character
            (if (zoohs/stack-script-point-multiline-p)

                (when-let ((script-section-end-point (zoohs/fetch-multiline-stack-script-next-line)))
                  (goto-char script-section-end-point)
                  (point))

                ;; else; if it is a single line, go to the next one
                (progn
                  (forward-line 1)
                  (point)
                  )
              )
            )
          ;; else; is safe to point at the beginning of the buffer
        (progn
          (beginning-of-buffer)
          (point))
        )
      )
    )
  )

(defun zoohs/navigate-to-pragma-section ()
  (interactive)
  (when-let ((pragma-goto-point (zoohs/fetch-pragma-section-point)))
    (goto-char pragma-goto-point)))

(defun zoohs/navigate-to-module ()
  (interactive)
  (when-let ((module-goto-point (zoohs/fetch-module-point)))
    (goto-char module-goto-point)))

(defun zoohs/insert-lang-pragma ()
  (interactive)
  (save-excursion
    (zoohs/navigate-to-pragma-section)
    (yas-expand-snippet (yas--get-template-by-uuid 'haskell-mode "language extension pragma"))
    (newline)
    ))

(defun zoohs/insert-opt-pragma ()
  (interactive)
  (save-excursion
    (zoohs/navigate-to-pragma-section)
    (yas-expand-snippet (yas--get-template-by-uuid 'haskell-mode "GHC options pragma"))
    (newline)
    ))

;; TODO: Need to do something similar to navigate-pragma-section; this won't work
;; unless there is already an import line
(defun zoohs/insert-import ()
  (interactive)
  (setq zoo/evil-state-before-snippet evil-state)
  (goto-char (zoohs/fetch-first-import-point))
  (forward-line -1)
  (newline)
  (evil-insert-state)
  (yas-expand-snippet (yas--get-template-by-uuid 'haskell-mode "simple import")))

;; TODO: Need to do something similar to navigate-pragma-section; this won't work
;; unless there is already an import line
(defun zoohs/insert-import-qualified ()
  (interactive)
  (setq zoo/evil-state-before-snippet evil-state)
  (goto-char (zoohs/fetch-first-import-point))
  (forward-line -1)
  (newline)
  (evil-insert-state)
  (yas-expand-snippet (yas--get-template-by-uuid 'haskell-mode "qualified import")))
