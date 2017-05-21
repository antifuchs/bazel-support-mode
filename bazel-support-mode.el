;;; bazel-support-mode.el ---  Launch bazel tests

;; Author: Andreas Fuchs <asf@boinkor.net>
;; Package-Version: 20170521.010
;; Version: 0.1.0
;; URL: https://github.com/antifuchs/bazel-support-mode
;; Keywords: tools, bazel, compilation

;; Package-Requires: ((emacs "24.3") (gotest "20170303.13"))

;; Copyright (C) 2017, Andreas Fuchs <asf@boinkor.net>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.
;;
;;; Code:

(require 'compile)
(require 'gotest)

(defgroup bazel-support nil
  "Bazel test and build support"
  :group 'compilation)

;;; Variables:

(defcustom bazel-support-pre-run-hook '()
  "List of functions to run before running any bazel build functions.
This could contain a BUILD file generator function or similar."
  :type 'hook
  :group 'bazel-support)

(defcustom bazel-support-compilation-scroll-output t
  "Whether to scroll output in the bazel compilation buffer.
This variable behaves exactly like `compilation-scroll-output'."
  :type '(radio (const :tag "On" t)
                (const :tag "Only to first error" first-error)
                (const :tag "Off" nil))
  :link '(variable-link compilation-scroll-output)
  :group 'bazel-support)

(defcustom bazel-support-keep-going-p nil
  "Indicates whether the bazel process gets the `--keep_going' flag."
  :type 'boolean
  :group 'bazel-support)

(defcustom bazel-support-test-output 'errors
  "Indicates the value to pass with `--test_output'.
This defaults to 'errors to maximize the utility of errors in the
buffer."
  :type '(radio (const :tag "Errors" errors)
                (const :tag "Summary only" summary)
                (const :tag "All" all)
                (const :tag "Streaming" streaming))
  :link '(url-link "https://bazel.build/versions/master/docs/command-line-reference.html#flag--test_output"))

;;; Faces:

(defface bazel-support--ok-face
  '((t (:foreground "#00ff00")))
  "Face indicating that something went well"
  :group 'bazel-support)

(defface bazel-support--target-pattern-face
  '((t (:inherit 'compilation-info)))
  "Face indicating that something went well"
  :group 'bazel-support)

;;; The minor mode to use in files:

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
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (let ((buffer "*Bazel Test*"))
    (bazel-support--cleanup buffer)
    (run-hooks 'bazel-support-pre-run-hook)
    (let ((final-args (concat (when bazel-support-keep-going-p
                               "--keep-going ")
                             (format "--test_output=%s "
                                     bazel-support-test-output)
                             args)))
      (compilation-start (concat "bazel test " final-args)
                         'bazel-support-compilation-mode
                         'bazel-support--buffer-name))
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
                             (when (re-search-backward "^==================== Test output for //\\([^:]+\\):[^:]+:$" (point-min) t)
                               (match-string 1))))
             (dir (when relative-dir
                    (concat
                     (bazel-support--base-directory default-directory)
                     relative-dir))))
        (if dir
            (progn
              (cons relative-file (expand-file-name dir)))
          relative-file)))))

(define-compilation-mode bazel-support-compilation-mode "Bazel compilation"
  "Major mode for bazel compilation / test running results."
  )

(defconst bazel-support-compilation-mode-font-lock-keywords
  '(;; Test statuses:
    ("^\\(//[[:word:]:_@]+\\)[[:space:]]+\\(?:(cached)\\)? \\(PASSED\\)"
     (1 'bazel-support--target-pattern-face)
     (2 'bazel-support--ok-face))
    ("^\\(//[[:word:]:_@]+\\)[[:space:]]+\\(?:(cached)\\)? \\(FAILED\\|NO STATUS\\)"
     (1 'bazel-support--target-pattern-face)
     (2 compilation-error-face))

    ;; Progress messages:
    ("^\\(PASS\\): \\(//[^ \t]*\\)"
     (1 'bazel-support--ok-face)
     (2 'bazel-support--target-pattern-face))
    ("^\\(FAIL\\): \\(//[^ \t]+\\)"
     (1 compilation-error-face)
     (2 'bazel-support--target-pattern-face))

    ;; Bazel meta-messages:
    ("^\\(INFO:\\) " (1 compilation-info-face))
    ("^\\(WARNING:\\) " (1 compilation-warning-face))
    ("^\\(ERROR:\\) \\([[:alnum:]-_/.]+/BUILD\\):"
     (1 compilation-error-face)
     (2 compilation-enter-directory-face))
    ("^Bazel compilation \\(started\\) at "
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil))
     (1 compilation-info-face))
    ("^Executed \\([[:digit:]]+\\) out of \\([[:digit:]]+\\) tests: \\([[:digit:]]\\) tests pass.$"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil))
     (1 compilation-message-face)
     (2 compilation-message-face)
     (3 compilation-message-face))
    ("^Bazel compilation exited \\(abnormally\\) with code \\([0-9]+\\)"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil))
     (1 compilation-error-face)
     (2 compilation-error-face nil t))
    ))

(setq bazel-support-compilation-error-regexp-alist-alist
      (append '((bazel-go-test-testing . ("^\t\\([[:alnum:]-_/.]+\\.go\\):\\([0-9]+\\): .*$"
                                          bazel-support--go-backwards-find-filename 2 nil 3 1))
                (bazel-go-compile . ("^\\([[:alnum:]-_/.]+\\.go\\):\\([0-9]+\\):\\(?:\\([0-9]+\\):\\)? .*$" 1 2 3)))
              go-test-compilation-error-regexp-alist-alist))

(setq bazel-support-compilation-error-regexp-alist
      '(bazel-go-test-testing bazel-go-compile))

(provide 'bazel-support-mode)

;;; bazel-support-mode.el ends here
