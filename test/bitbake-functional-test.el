;;; bitbake-functional-test.el --- Bitbake mode functional test suite  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Damien Merenne

;; Author: Damien Merenne
;; URL: https://github.com/canatella/bitbake

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Functional test suite for bitbake mode

;;; Code:

(require 'bitbake)
(require 'f)

(ert-deftest build-directory-setup ()
  :tags '(functional)
  (with-build
   (should (f-file? (f-join bitbake-test/build-path "conf" "local.conf")))))

(ert-deftest bitbake-start-stop-server ()
  :tags '(functional)
  (let ((process))
    (with-server
     (setq process (get-buffer-process (bitbake-buffer)))
     (should (getenv "BBSERVER"))
     (should process))
    (should (not (getenv "BBSERVER")))
    (should (not (process-live-p process)))
    (should (not (get-buffer "*bitbake*")))
    (should (not bitbake-recipes-cache))))

(ert-deftest bitbake-fetch-recipes/no-server ()
  :tags '(functional)
  "Should raise an error."
  (should-error (bitbake-fetch-recipes)))

(ert-deftest bitbake-fetch-recipes/no-error ()
  :tags '(functional)
  "Should fetch recipes."
  (with-server
   (should (> (length (bitbake-fetch-recipes)) 500))))

(ert-deftest bitbake-fetch-recipe-tasks/no-server ()
  :tags '(functional)
  "Should raise an error."
  (should-error (bitbake-fetch-recipes-tasks "busybox")))

(ert-deftest bitbake-fetch-recipe-tasks/no-error ()
  :tags '(functional)
  "Should fetch recipe tasks."
  (with-server
   (should (> (length (bitbake-fetch-recipe-tasks "busybox")) 5))))

(ert-deftest bitbake-fetch-recipe-variable/no-server ()
  :tags '(functional)
  "Should raise an error."
  (should-error (bitbake-fetch-recipe-variables)))

(ert-deftest bitbake-fetch-recipe-variable/no-error ()
  :tags '(functional)
  "Should fetch recipe variable."
  (with-server
   (should (assoc "FILES_busybox" (bitbake-fetch-recipe-variables "busybox")))))

(provide 'bitbake-functional-test)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; bitbake-functional-test.el ends here
