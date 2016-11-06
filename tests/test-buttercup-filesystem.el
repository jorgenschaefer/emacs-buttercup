;;; buttercup-test.el --- Tests for buttercup.el -*-lexical-binding:t-*-

;; Copyright (C) 2016  Jorgen Schaefer <contact@jorgenschaefer.de>

;; Author: Matus Goljer <matus.goljer@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'buttercup)
(require 'buttercup-filesystem)

(describe "With temp fs"

  (it "should create multiple files"
    (with-buttercup-filesystem '("foo" "bar" "baz")
      (expect "foo" :to-be-file)
      (expect "bar" :to-be-file)
      (expect "baz" :to-be-file)))

  (it "should create multiple directories and files"
    (with-buttercup-filesystem '("foo/" "bar/" "baz")
      (expect "foo" :to-be-directory)
      (expect "bar" :to-be-directory)
      (expect "baz" :to-be-file)))

  (it "should create nested directories"
    (with-buttercup-filesystem '("foo/bar" "foo/baz/")
      (expect "foo/bar" :to-be-file)
      (expect "foo/baz" :to-be-directory)))

  (it "should create non-empty file"
    (with-buttercup-filesystem '(("foo" "amazing content"))
      (expect "foo" :to-contain "amazing content")))

  (it "should create non-empty nested file"
    (with-buttercup-filesystem '(("foo/bar" "amazing content"))
      (expect "foo/bar" :to-contain "amazing content")))

  (it "should nest files recursively"
    (with-buttercup-filesystem '(("foo" ("bar" "baz" "bam/"))
                                 ("a/b" ("c" "d/"))
                                 ("x" (("y" ("z"))
                                       "w")))
      (expect "foo/bar" :to-be-file)
      (expect "foo/baz" :to-be-file)
      (expect "foo/bam" :to-be-directory)
      (expect "a/b/c" :to-be-file)
      (expect "a/b/d" :to-be-directory)
      (expect "x/y/z" :to-be-file)
      (expect "x/w" :to-be-file))))
