;;; bitbake-test.el --- Bitbake mode unit test suite  -*- lexical-binding: t; -*-

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

;; Unit test suite for bitbake mode

;;; Code:

(require 'bitbake)
(require 'el-mock)

(ert-deftest bb-add-path/new-path ()
  "Should add the new path to the PATH environment variable if not present"
  :tags '(unit)
  (save-environment
   (setenv "PATH" "foo")
   (bb-add-path "bar")
   (should (equal "bar:foo" (getenv "PATH")))
   (bb-add-path "baz")
   (should (equal "baz:bar:foo" (getenv "PATH")))))

(ert-deftest bb-add-path/existing-path ()
  "Should not add the new path to the PATH environment variable if already present"
  :tags '(unit)
  (save-environment
   (setenv "PATH" "foo:bar")
   (bb-add-path "bar")
   (should (equal "foo:bar" (getenv "PATH")))))

(ert-deftest bb-remove-path/existing-path ()
  "Should remove the path from the PATH environment variable if present"
  :tags '(unit)
  (save-environment
   (setenv "PATH" "foo:bar")
   (bb-remove-path "bar")
   (should (equal "foo" (getenv "PATH")))))

(ert-deftest bb-add-path/invalid-path ()
  "Should not alter the PATH environment variable when the path is not present"
  :tags '(unit)
  (save-environment
   (setenv "PATH" "foo:bar")
   (bb-remove-path "baz")
   (should (equal "foo:bar" (getenv "PATH")))))

(ert-deftest bb-setup-cleanup-environment ()
  "Should setup BBPATH and BUILDDIR environment variable and add scripts and bitbake/bin to PATH"
  :tags '(unit)
  (save-environment
   (setenv "PATH")
   (setenv "BUILDDIR")
   (setenv "BBPATH")
   (bb-setup-environment "pokydir/" "builddir/")
   (should (equal (getenv "PATH") "pokydir/bitbake/bin:pokydir/scripts"))
   (should (equal (getenv "BUILDDIR") "builddir/"))
   (should (equal (getenv "BBPATH") "builddir/"))
   (bb-cleanup-environment "pokydir/")
   (should (equal (getenv "PATH") ""))
   (should (equal (getenv "BUILDDIR") nil))
   (should (equal (getenv "BBPATH") nil))))

(ert-deftest bb-parse-recipe/no-error ()
  "Should parse the recipes"
  :tags '(unit)
  (with-temp-buffer
    (bitbake-test-insert-resource "recipes.txt" (current-buffer))
    (let ((recipes (bb-parse-recipes (current-buffer))))
      (should (equal (nth 14 recipes) '("xtrans-native" "1" "1.3.3" "r0")))
      (should (equal (length recipes) 1080)))))

(ert-deftest bb-recipes/in-cache ()
  "Should return cached recipes"
  :tags '(unit)
  (setq bb-recipes-cache '(("foo" "meta" "0.0")))
  (should (equal (bb-recipes) bb-recipes-cache)))

(ert-deftest bb-recipes/not-in-cache ()
  "Should stop bitbake server and start it again to fetch the recipes"
  :tags '(unit)
  (setq bb-recipes-cache '())
  (with-mock
   (mock (bb-fetch-recipes) => '(("foo" "meta" "0.0")))
   (should (equal (bb-recipes) '(("foo" "meta" "0.0"))))))

(ert-deftest bb-recipes/force-fetch ()
  "Should fetch again the recipe"
  :tags '(unit)
  (setq bb-recipes-cache '(("foo" "bar" "0.0")))
  (with-mock
   (mock (bb-fetch-recipes) => '(("foo" "meta" "0.0")))
   (should (equal (bb-recipes t) '(("foo" "meta" "0.0"))))))

(ert-deftest bb-recipe-info/valid-recipe ()
  "Should return the recipe info given a valid recipe name"
  :tags '(unit)
  (with-mock
   (stub bb-recipes => '(("foo" "meta" "0.0")))
   (should (equal (bb-recipe-info "foo") '("foo" "meta" "0.0")))))

(ert-deftest bb-recipe-info/invalid-recipe ()
  "Should error on invalid recipe name"
  :tags '(unit)
  (with-mock
   (stub bb-recipes => '(("foo" "meta" "0.0")))
   (should-error (bb-recipe-info "bar"))))

(ert-deftest bb-recipe-names/names ()
  "Should return the list of available recipe name."
  :tags '(unit)
  (with-mock
   (stub bb-recipes => '(("bar" "meta" "0.0") ("foo" "meta" "0.0")))
   (should (-same-items? (bb-recipe-names) '("bar" "foo")))))

(ert-deftest bb-image-names/no-images ()
  "Should return the list of available image name."
  :tags '(unit)
  (with-mock
   (stub bb-recipes => '(("bar" "meta" "0.0") ("foo" "meta" "0.0")))
   (should (equal (bb-image-names) '()))))

(ert-deftest bb-image-names/image-names ()
  "Should return the list of available image name."
  :tags '(unit)
  (with-mock
   (stub bb-recipes => '(("foo-image-bar" "meta" "0.0") ("foo" "meta" "0.0")))
   (should (equal (bb-image-names) '("foo-image-bar")))))

(ert-deftest bb-buffer-recipe/bb-file ()
  "Should return recipe name when visiting bb file"
  :tags '(unit)
  (with-poky
   (find-file "meta/recipes-core/util-linux/util-linux_2.24.2.bb")
   (should (equal (bb-buffer-recipe) "util-linux"))
   (kill-buffer)))

(ert-deftest bb-buffer-recipe/same-directory-file ()
  "Should return recipe name when visiting a file in the same directory as the bb file"
  :tags '(unit)
  (with-poky
   (find-file "meta/recipes-core/util-linux/util-linux.inc")
   (should (equal (bb-buffer-recipe) "util-linux"))
   (kill-buffer)))

(ert-deftest bb-buffer-recipe/subdirectory-file ()
  "Should return recipe name when visiting a file in a subdirectory of the bb file"
  :tags '(unit)
  (with-poky
   (find-file "meta/recipes-core/util-linux/util-linux/swapargs.h")
   (should (equal (bb-buffer-recipe) "util-linux"))
   (kill-buffer)))

(ert-deftest bb-parse-recipe-tasks/no-errors ()
  "Should parse tasks"
  :tags '(unit)
  (with-temp-buffer
    (bitbake-test-insert-resource "tasks.txt" (current-buffer))
    (let ((tasks (bb-parse-recipe-tasks (current-buffer))))
      (should (-contains? tasks "compile"))
      (should (-contains? tasks "fetch"))
      (should (-contains? tasks "install")))))

(ert-deftest bb-parse-recipe-tasks/errors ()
  "Should raise error"
  :tags '(unit)
  (with-temp-buffer
    (bitbake-test-insert-resource "tasks-errors.txt" (current-buffer))
    (should-error (bb-parse-recipe-tasks (current-buffer)))))

(ert-deftest bb-recipe-tasks/in-cache ()
  "Should return the tasks in cache"
  :tags '(unit)
  (setq bb-recipe-tasks-cache '(("foobar" ("task1" "task2"))))
  (should (equal (bb-recipe-tasks "foobar") '("task1" "task2"))))

(ert-deftest bb-recipe-tasks/not-in-cache ()
  "Should return the tasks in cache"
  :tags '(unit)
  (setq bb-recipe-tasks-cache '())
  (with-mock
   (mock (bb-fetch-recipe-tasks "foobar") => '("task1" "task2"))
   (should (equal (bb-recipe-tasks "foobar") '("task1" "task2")))))

(ert-deftest bb-recipe-tasks/force-fetch ()
  "Should return the tasks in cache"
  :tags '(unit)
  (setq bb-recipe-tasks-cache '(("foobar" ("old1" "old2"))))
  (with-mock
   (mock (bb-fetch-recipe-tasks "foobar") => '("task1" "task2"))
   (should (equal (bb-recipe-tasks "foobar" t) '("task1" "task2")))))

(ert-deftest bb-parse-recipe-variables/simple ()
  "Should parse variables"
  :tags '(unit)
  (with-temp-buffer
    (insert "name=\"value\"\ntask() {")
    (should (equal (bb-parse-recipe-variables (current-buffer)) '(("name" . "value"))))))

(ert-deftest bb-parse-recipe-variables/escaped-quotes ()
  "Should parse variables with escaped quotes"
  :tags '(unit)
  (with-temp-buffer
    (insert "name=\"\\\"\\\"\\\"\"\ntask() {")
    (should (equal (bb-parse-recipe-variables (current-buffer)) '(("name" . "\"\"\""))))))

(ert-deftest bb-parse-recipe-variables/real ()
  "Should parse real variable example"
  :tags '(unit)
    (with-temp-buffer
    (bitbake-test-insert-resource "variables.txt" (current-buffer))
    (should (equal (cdr (assoc "D" (bb-parse-recipe-variables (current-buffer))))
                   "/home/dam/Projects/FCS/04-Build/build/tmp/work/core2-poky-linux/busybox/1.21.1-r0/image"))))

(ert-deftest bb-recipe-variables/in-cache ()
  "Should return the variables in cache"
  :tags '(unit)
  (setq bb-recipe-variables-cache '(("recipe" (("foo" . "bar") ("baz" . "bal")))))
  (should (equal (bb-recipe-variables "recipe") '(("foo" . "bar") ("baz" . "bal")))))

(ert-deftest bb-recipe-variables/not-in-cache ()
  "Should return the variables in cache"
  :tags '(unit)
  (setq bb-recipe-variables-cache '())
  (with-mock
   (mock (bb-fetch-recipe-variables "foobar") => '(("var1" . "val2")))
   (should (equal (bb-recipe-variables "foobar") '(("var1" . "val2"))))))

(ert-deftest bb-recipe-variables/force-fetch ()
  "Should return the variables in cache"
  :tags '(unit)
  (setq bb-recipe-variables-cache  '(("foobar" (("old1" . "vold1") ("old2" . "vold2")))))
  (with-mock
   (mock (bb-fetch-recipe-variables "foobar") => '(("name" . "value")))
   (should (equal (bb-recipe-variables "foobar" t) '(("name" . "value"))))))

(ert-deftest bb-recipe-variable/variable ()
  "Should return the variable"
  :tags '(unit)
  (with-mock
   (mock (bb-recipe-variables "foobar" nil) => '(("foo" . "bar") ("baz" . "bal")))
   (should (equal (bb-recipe-variable "baz" "foobar") "bal"))))

(ert-deftest bb-buffer/ ()
  "Should create the *bitbake* buffer"
  :tags '(unit)
  (setq bb-build-directory "/tmp")
  (with-bb-buffer (bb-buffer)
    (should (equal default-directory "/tmp"))
    (should (equal (substring-no-properties (buffer-string)) bb-buffer-prompt))))

(ert-deftest bb-shell-command/ ()
    "Should run shell command and capture output"
    :tags '(unit)
    (with-bb-buffer
     (bb-shell-command "echo foo")
     (bb-wait-for-prompt)
     (should (equal (buffer-string) "/////---bitbake$ echo foo\nfoo\n/////---bitbake$ "))
     (with-current-buffer (bb-capture-buffer)
       (should (equal (buffer-string) "echo foo\nfoo\n")))))

(provide 'bitbake-test)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; bitbake-test.el ends here
