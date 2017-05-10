;;; a mode for supporting you while you do things on code that builds with bazel

(define-minor-mode bazel-support-mode
  "A minor mode for running things in a bazelified repository."
  :lighter " Bazel"
  :keymap `((,(kbd "C-c C-b C-t") . bazel-support-run-all-package-tests)
            (,(kbd "C-c C-b C-M-t") . bazel-support-run-all-tests))
  :group 'bazel)

;;;###autoload
(defun turn-on-bazel-support-mode ()
  "Turn on bazel-support-minor-mode if the current buffer is in a bazelified repository."
  (interactive)
  (when (locate-dominating-file buffer-file-name "WORKSPACE")
    (bazel-support-mode +1)))

;;;###autoload
(defun bazel-support-run-all-package-tests ()
  "Run all tests under the current file's package."
  (interactive)
  (bazel-support--run-tests-with-args (concat (bazel-support--guess-package buffer-file-name) "...")))

;;;###autoload
(defun bazel-support-run-all-tests ()
  "Run all tests available in a code repo."
  (interactive)
  (bazel-support--run-tests-with-args "//..."))

(defun bazel-support--run-tests-with-args (args)
  (let ((buffer "*Bazel Test*"))
    (bazel-support--cleanup buffer)
    (compilation-start (concat "bazel test --test_output=errors " args)
                       'bazel-support-compilation-mode
                       'bazel-support--buffer-name)
    (with-current-buffer "*Bazel Test*"
      (rename-buffer buffer))
    (set-process-sentinel (get-buffer-process buffer) 'bazel-support--finished-sentinel)))

(defun bazel-support--base-directory (pathname)
  (save-match-data
    (locate-dominating-file pathname "WORKSPACE")))

(defun bazel-support--guess-package (pathname)
  "Return a bazel package name that seems correct for the file-name."
  (let ((workspace-file (bazel-support--base-directory buffer-file-name)))
    (if workspace-file
        (let* ((dir (file-name-directory workspace-file)))
          (concat "//" (file-name-directory (file-relative-name pathname dir))))
      nil)))

;;; Compilation mode support

(defun bazel-support--buffer-name (mode-name)
  "Name of the bazel process buffer. MODE-NAME is unused."
  "*Bazel Test*")

(defun bazel-support--finished-sentinel (process event)
  "Execute after PROCESS return and EVENT is 'finished'."
  (when (equal event "finished\n")
    (message "bazel test finished.")))

(defun bazel-support--cleanup (buffer)
  "Clean up the old go-test process BUFFER when a similar process is run."
  (when (get-buffer buffer)
    (when (get-buffer-process (get-buffer buffer))
      (delete-process buffer))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))))

(defun bazel-support--go-backwards-find-filename ()
  (let ((relative-file (match-string 1)))
    (save-match-data
      (let* ((relative-dir (save-excursion
                             (when (re-search-backward "^==================== Test output for //\\([^:]+\\):go_default_xtest:$" (point-min) t)
                               (match-string 1))))
             (dir (when relative-dir
                    (concat
                     (bazel-support--base-directory default-directory)
                     relative-dir))))
        (if dir
            (progn
              (cons relative-file (expand-file-name dir)))
          relative-file)))))

(setq bazel-support-compilation-error-regexp-alist-alist
      '((bazel-go-test-testing . ("^\t\\([[:alnum:]-_/.]+\\.go\\):\\([0-9]+\\): .*$"
                                  bazel-support--go-backwards-find-filename 2 nil 3 1))))

(defvar bazel-support-compilation-error-regexp-alist
  '(bazel-go-test-testing))

(define-compilation-mode bazel-support-compilation-mode "Bazel compilation"
  "Major mode for bazel compilation / test running results."
  )

(provide 'bazel-support-mode)
