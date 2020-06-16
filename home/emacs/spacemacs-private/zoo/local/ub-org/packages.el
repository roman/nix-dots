(defvar ub-org-packages
  '(projectile
    dash
    s
    org
    org-jira))

(defmacro ensure-on-issue-id (issue-id &rest body)
  "Make sure we are on an issue heading with id ISSUE-ID, before executing BODY."
  (declare (indent 1))
  `(save-excursion
     (save-restriction
       (widen)
       (show-all)
       (goto-char (point-min))
       (let (p)
         (setq p (org-find-entry-with-id ,issue-id))
         (unless p
           (error "Issue %s not found!" ,issue-id))
         (goto-char p)
         (org-narrow-to-subtree)
         ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ub-org-project-issue-folder
  "org")

(defvar ub-org-issue-format-regexs
  '("bee-[[:digit:]]+"
    "lp-[[:digit:]]+"
    "cs-[[:digit:]]+"))

;; Map a b -> Map b a
(defun ub-org/flip-key-val (alist)
  (-map (lambda (it)
          (cons (cdr it)
                (car it)))
        alist))

;; IssueId -> IO FilePath
(defun ub-org/mk-issue-org-file-path (issue-id)
  "Use PROJECT-ROOT and ISSUE-ID to infer a file name."
  (let ((project-root (projectile-project-root)))
    (concat project-root
            (file-name-as-directory ub-org-project-issue-folder)
            (concat issue-id ".org"))))

;; Regex -> String -> Maybe IssueId
(defun ub-org/get-issue-id-from-branch-name (issue-regex branch-name)
  "Use ISSUE-REGEX to get issue-id from a BRANCH-NAME."
  (when (and issue-regex branch-name (string-match issue-regex branch-name))
    (match-string 0 branch-name)))

;; IO IssueId
(defun ub-org/find-issue-id-from-branch-name ()
  "Infer an org file name from issue number on current's branch name.

This works when the `ub-org-issue-format-regexs` is not nil."
  (when (not (null ub-org-issue-format-regexs))
    (let* ((branch-name (car (vc-git-branches))))
      (--first
       (ub-org/get-issue-id-from-branch-name it
                                             branch-name)
       ub-org-issue-format-regexs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ub-org-issue-status|jira->org
  '(("Open" . "TODO")
    ("In Progress" . "STARTED")
    ("Code Reviewing" . "REVIEWING")
    ("Test Ready" . "WAITING")
    ("Testing" . "TESTING")
    ("Deploy Ready" . "DEPLOY")
    ("Closed" . "Done")))

(defvar ub-org-issue-status|org->jira
  (ub-org/flip-key-val ub-org-issue-status|jira->org))

(defvar ub-org-jira-metadata
  nil)

;; Map a b -> a -> Maybe b
(defun ub-org/get-attr (o attr-name)
  (cdr (assoc attr-name o)))

;; Map a b -> a -> Maybe b
(defun ub-org/get-jira-attr (o attr-name)
  (let* ((attr-metadata (ub-org/get-attr ub-org-jira-metadata
                                         attr-name))
         (attr-val0 (ub-org/get-attr o attr-name)))
    (or (and attr-metadata
             (ub-org/get-attr attr-metadata
                              attr-val0))
        attr-val0)))

;; String -> String
(defun ub-org/jira-status->org-status (status)
  (ub-org/get-attr ub-org-issue-status|jira->org
                   status))

;; [{name: String}] -> String
(defun ub-org/jira-components->org-entry (components)
  (s-join ", "
          (--map (ub-org/get-attr it 'name) components)))

;; JiraIssueId -> IO JiraIssue
(defun ub-org/fetch-jira-issue (issue-id)
  (car (jiralib-do-jql-search
        (format "id = %s" issue-id))))

;; JiraIssueId -> IO [JiraComment]
(defun ub-org/fetch-jira-issue-comments (issue-id)
  (jiralib-get-comments issue-id))

;; JiraComment -> IO ()
(defun ub-org/jira-comment->org (comment)
  (ensure-on-issue-id issue-id
    (let* ((comment-id (ub-org/get-attr comment 'id))
           (comment-author (ub-org/get-attr comment 'author))
           (comment-heading (format "Comment from %s" comment-author))
           (comment-body    (ub-org/get-attr comment 'body))
           (comment-created (ub-org/get-attr comment 'created))
           (comment-updated (ub-org/get-attr comment 'updated)))

      ;; delete old entry if found
      (setq p (org-find-entry-with-id comment-id))
      (when (and p
                 (>= p (point-min))
                 (<= p (point-max)))
        (goto-char p)
        (org-narrow-to-subtree)
        (delete-region (point-min) (point-max)))
      (goto-char (point-max))
      (unless (looking-at "^")
        (insert "\n"))

      ;; ** Comment from <user> heading
      (insert "** ")
      (insert comment-heading "\n")

      ;; :comment: TAG
      (org-narrow-to-subtree)
      (org-change-tag-in-region
       (point-min)
       (save-excursion
         (forward-line 1)
         (point))
       "comment"
       nil)

      ;; :PROPERTIES:

      (org-entry-put (point) "ID" comment-id)
      (org-entry-put (point) "created" comment-created)
      (unless (string= comment-created comment-updated)
        (org-entry-put (point) "updated" comment-updated))

      ;; :END:

      (goto-char (point-max))
      (insert
       (cond
        ((and (s-starts-with? "{code}" comment-body)
              (s-ends-with? "{code}" comment-body))
         (->> comment-body
              (s-chop-prefix "{code}")
              (s-chop-suffix "{code}")))
        (t
         (replace-regexp-in-string "^" "  "
                                   comment-body))))
      (widen))))

;; FilePath -> JiraIssue -> [JiraComment] -> IO ()
(defun ub-org/jira-issue->org (org-filepath jira-issue jira-issue-comments)
  (let* ((org-insert-heading-hook nil)

         (issue-id          (ub-org/get-jira-attr jira-issue 'key))
         (issue-summary     (ub-org/get-jira-attr jira-issue 'summary))
         (issue-description (ub-org/get-jira-attr jira-issue 'description))
         (issue-status      (ub-org/get-jira-attr jira-issue 'status))

         (org-buffer (or (find-buffer-visiting org-filepath)
                         (find-file org-filepath))))


    (with-current-buffer org-buffer
      (erase-buffer)

      (widen)
      (show-all)
      (goto-char (point-min))

      ;; * STATUS <jira summary>
      (insert (format "* %s %s\n"
                      (ub-org/jira-status->org-status issue-status)
                      issue-summary))


      ;; :issue-id: TAG
      (org-narrow-to-subtree)
      (org-change-tag-in-region
       (point-min)
       (save-excursion
         (forward-line 1)
         (point))
       (replace-regexp-in-string "-" "_" issue-id)
       nil)

      ;; :PROPERTIES:

      ;; Insert ID property
      (org-entry-put (point) "ID" issue-id)

      ;; Insert components property
      (org-entry-put (point)
                     (symbol-name 'components)
                     (ub-org/jira-components->org-entry
                      (ub-org/get-jira-attr jira-issue
                                            'components)))

      ;; Insert other properties
      (-each
          '(asignee reporter type priority resolution
                    status created updated)
        (lambda (attr-name)
          (let ((attr-val (ub-org/get-jira-attr jira-issue
                                                attr-name)))
            (when (and attr-val
                       (not (string= attr-val "")))

              (org-entry-put (point)
                             (symbol-name attr-name)
                             attr-val)))))

      ;; :END:

      ;; ** Description
      (ensure-on-issue-id issue-id
        (let* ((desc-heading (concat
                              "Description"
                              (format ": [[%s][%s]]"
                                      (concat jiralib-url "/browse/" issue-id)
                                      issue-id))))
          (if (org-goto-first-child)
              (org-insert-heading)
            ;; else
            (goto-char (point-max))
            (org-insert-subheading t))

          (insert desc-heading "\n")
          (insert
           (replace-regexp-in-string "^"
                                     "  "
                                     issue-description)))

        ;; :description: TAG
        (org-narrow-to-subtree)
        (org-change-tag-in-region
         (point-min)
         (save-excursion
           (forward-line 1)
           (point))
         "description"
         nil)
        (widen))

      ;; Comments
      (-each jira-issue-comments
        'ub-org/jira-comment->org))))

;; IO ()
(defun ub-org/fetch-jira-metadata ()
  (unless ub-org-jira-metadata
    (setq ub-org-jira-metadata
          `((status . ,(jiralib-get-statuses))
            (resolutions . ,(jiralib-get-resolutions))
            (type . ,(jiralib-get-issue-types))
            (priority . ,(jiralib-get-priorities))))))

;; IO ()
(defun ub-org/fetch-issue-from-branch ()
  (interactive)
  (let ((issue-id (ub-org/find-issue-id-from-branch-name)))
    (if issue-id
        (ub-org/fetch-issue-by-id issue-id)
      ;; else
      (message "Could not infer any issue-id from branch name"))))

;; JiraIssueId -> IO ()
(defun ub-org/fetch-issue-by-id (issue-id)
  (interactive
   (list (read-from-minibuffer "ID (e.g. CS-1234): ")))

  ;; fill up metadata
  (ub-org/fetch-jira-metadata)
  (let* ((jira-issue-id (s-upcase issue-id))
         (org-file-name (s-downcase issue-id))

         (jira-issue          (ub-org/fetch-jira-issue jira-issue-id))
         (jira-issue-comments (ub-org/fetch-jira-issue-comments jira-issue-id))
         (org-file-path       (ub-org/mk-issue-org-file-path org-file-name)))

    (ub-org/jira-issue->org org-file-path jira-issue jira-issue-comments)))


;; testing
;; (setq test-issue (ub-org/fetch-jira-issue "LP-9468"))
;; (setq test-issue-comments (ub-org/fetch-jira-issue-comments "LP-9468"))
;; (ub-org/jira-issue->org (ub-org/mk-issue-org-file-path "lp-9468")
;;                         test-issue
;;                         test-issue-comments)
