;;; test-helper.el --- Bitbake: Initialize test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2014  Sebastian Wiesner

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Initializes the test suite for use with `ert-runner'.
;;
;; Also serves as init file for interactive test running.

;;; Code:

(defun bitbake-test-insert-resource (file buffer)
  "Insert resource FILE in BUFFER."
  (with-current-buffer buffer
    (insert-file-contents-literally (format "%s/resources/%s" bitbake-test/test-path file))))

(defmacro bitbake-test-poky-git (format &rest args)
  "Call git command FORMAT in the poky directory with ARGS."
  `(call-process-shell-command (format "git --git-dir=%s %s" bitbake-test/poky-path (format ,format ,@args))))

(defmacro save-environment (&rest body)
  "Save environment, evaluate BODY and restore environment."
  `(let ((old-path (getenv "PATH")))
     (unwind-protect (progn ,@body)
       (setenv "PATH" old-path))))

(defmacro with-poky (&rest body)
  "Evaluate BODY in the root of poky for each branch."
  (declare (indent 0))
  `(dolist (branch bitbake-test/poky-branches)
     (unwind-protect
         (progn
           (bitbake-test-poky-git "checkout -b %s origin/%s" branch branch)
           (setq default-directory bitbake-test/poky-path)
           ,@body)
       (bitbake-test-poky-git "clean -d -f -q  -b"))))

(defmacro with-build (&rest body)
  "Evaluate BODY in a clean build environment for each boky branch."
  (declare (indent 0))
  `(with-poky
    (setq bb-current-poky-directory bitbake-test/poky-path
          bb-current-build-directory bitbake-test/build-path)
    (mkdir bitbake-test/build-path t)
    (unwind-protect
        (progn
          (call-process-shell-command (format "source %s/oe-init-build-env %s" bitbake-test/poky-path bitbake-test/build-path))
          ,@body)
      (delete-directory bitbake-test/build-path t))))

(defmacro with-server (&rest body)
  "Start bitbake server, evaluate BODY and stop server."
  (declare (indent 0))
  `(with-build
    (bb-start-server bitbake-test/poky-path bitbake-test/build-path)
    (unwind-protect
        (progn ,@body)
      (bb-stop-server))))

(defmacro with-bb-buffer (&rest body)
  "Evaluate BODY in the bitbake buffer and kill it after."
  (declare (indent 0))
  `(let ((buffer (bb-buffer)))
     (unwind-protect
         (with-current-buffer buffer
           ,@body)
       (progn
         (setq kill-buffer-query-functions '())
         (kill-process (get-buffer-process buffer))
         (kill-buffer buffer)
         (kill-buffer (bb-capture-buffer))))))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; test-helper.el ends here
