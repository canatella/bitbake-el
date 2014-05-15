;;; bitbake-test.el --- Bitbake mode test suite initialization  -*- lexical-binding: t; -*-

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

;; Initialization code for test suite for bitbake mode

;;; Code:

(require 'f)

(defvar bitbake-test/test-path
  (f-dirname (f-this-file)))

(defvar bitbake-test/root-path
  (f-parent bitbake-test/test-path))

(defvar bitbake-test/poky-path
  (f-join bitbake-test/test-path "poky"))

(defvar bitbake-test/build-path
  (f-join bitbake-test/test-path "build"))

(defvar bitbake-test/poky-branches '("dora" "daisy"))
(setq bitbake-test/poky-branches '("dora" "daisy"))

;; fetch poky
(message "Updating poky in %s" bitbake-test/poky-path)
(let ((git-command (if (f-directory? bitbake-test/poky-path)
                       (format "git pull --git-dir=%s" bitbake-test/poky-path)
                     (format "git clone http://git.yoctoproject.org/git/poky %s" bitbake-test/poky-path))))
  (call-process-shell-command git-command))


(load (f-expand "bitbake" bitbake-test/root-path))

(add-to-list 'load-path bitbake-test/root-path)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; bitbake-init.el ends here
