;;; buttercup-filesystem.el --- Filesystem utilities for buttercup -*-lexical-binding:t-*-

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

;;; Commentary:

;; This file contains buttercup functionality which deals with the
;; filesystem.

;; We define some new matchers to make expectations dealing with the
;; file system simpler.  Thus one can write:
;;
;;   (expect "foo.txt" :to-be-file)
;;
;; and similar.  For the complete list see the documentation.

;; To make testing functions which face the file system easier, we add
;; simple DSL which sets up temporary file hierarchies and runs the
;; code in that new context (cf. `with-temp-buffer')

;; The entry point is `with-buttercup-filesystem' which as first argument
;; accepts the specification and then variable number of forms
;; (i.e. "body").  To see the documentation of the DSL call:
;;
;;   M-x describe-function RET with-buttercup-filesystem RET

;;; Code:

(require 'buttercup)

;;;;;;;;;;;;;;;;;;;;;;;
;;; Filesystem matchers

(buttercup-define-matcher :to-be-file (file)
  (if (file-regular-p file)
      (cons t (format "Expected %S not to be a file" file))
    (cons nil (format "Expected %S to be a file" file))))

(buttercup-define-matcher :to-be-directory (dir)
  (if (file-directory-p dir)
      (cons t (format "Expected %S not to be a directory" dir))
    (cons nil (format "Expected %S to be a directory" dir))))

(buttercup-define-matcher :to-contain (file content)
  (if (with-temp-buffer
        (insert-file-contents file)
        (equal (buffer-string) content))
      (cons t (format "Expected the content of %S not to `equal' %S" file content))
    (cons nil (format "Expected the content of %S to `equal' %S" file content))))

(buttercup-define-matcher :content-to-match (file regexp)
  (if (with-temp-buffer
        (insert-file-contents file)
        (string-match-p regexp (buffer-string)))
      (cons t (format "Expected the content of %S not to `string-match' %S" file regexp))
    (cons nil (format "Expected the content of %S to `string-match' %S" file regexp))))

;;;;;;;;;;;;;;;;;;;;
;;; Mock file system

(defun with-buttercup-filesystem--make-parent (spec path)
  "If SPEC is a file name, create its parent directory rooted at PATH."
  (save-match-data
    (string-match "\\(.*\\)/" spec)
    (when (match-string 1 spec)
      (make-directory (concat path "/" (match-string 1 spec)) t))))

(defun with-buttercup-filesystem--init (spec &optional path)
  "Interpret the SPEC inside PATH."
  (setq path (or path "."))
  (cond
   ((listp spec)
    (cond
     ;; non-empty file
     ((and (stringp (car spec))
           (stringp (cadr spec)))
      (when (string-match-p "/\\'" (car spec))
        (error "Invalid syntax: `%s' - cannot create a directory with text content" (car spec)))
      (with-buttercup-filesystem--make-parent (car spec) path)
      (with-temp-file (concat path "/" (car spec))
        (insert (cadr spec))))
     ;; directory
     ((and (stringp (car spec))
           (consp (cadr spec)))
      (make-directory (concat path "/" (car spec)) t)
      (mapc (lambda (s) (with-buttercup-filesystem--init
                         s (concat path "/" (car spec)))) (cadr spec)))
     ;; recursive spec, this should probably never happen
     (t (mapc (lambda (s) (with-buttercup-filesystem--init s path)) spec))))
   ;; directory specified using a string
   ((and (stringp spec)
         (string-match-p "/\\'" spec))
    (make-directory (concat path "/" spec) t))
   ;; empty file
   ((stringp spec)
    (with-buttercup-filesystem--make-parent spec path)
    (write-region "" nil (concat path "/" spec) nil 'no-message))
   (t (error "Invalid syntax: `%s'" spec))))

(defmacro with-buttercup-filesystem (spec &rest forms)
  "Create temporary file hierarchy according to SPEC and run FORMS.

SPEC is a list of specifications for file system entities which
are to be created.

File system entities are specified as follows:

1. a string FILE is the name of file to be created
  - if the string contains \"/\", parent directories are created
    automatically
  - if the string ends with \"/\", a directory is created
2. a list of two elements (FILE CONTENT) specifies filename and the
  content to put in the file
  - the \"/\" rules apply in the same way as in 1., except you can not
    create a directory this way
3. a list where car is a string and cadr is a list (DIR SPEC) is a
  recursive specification evaluated with DIR as current directory
  - the \"/\" rules apply in the same way as in 1., except you can not
    create a file this way, a directory is always created

An example showing all the possibilities:

  (\"empty_file\"
   \"dir/empty_file\"
   \"dir/subdir/\"
   (\"non_empty_file\" \"content\")
   (\"dir/anotherdir/non_empty_file\" \"tralala\")
   (\"big_dir\" (\"empty_file\"
              (\"non_empty_file\" \"content\")
              \"subdir/empty_file\")))

If we want to run some code in a directory with an empty file
\"foo.txt\" present, we call:

  (with-buttercup-filesystem '(\"foo\")
    (code-here)
    (and-some-more-forms))

You should *not* depend on where exactly the hierarchy is created.
By default, a new directory in `temporary-file-directory' is
created and the specification is evaluated there, but this is up
for change."
  (declare (indent 1))
  (let ((temp-root (make-symbol "temp-root"))
        (old-dd (make-symbol "old-dd")))
    `(let ((,temp-root (make-temp-file "temp-fs-" t))
           (,old-dd default-directory))
       (unwind-protect
           (progn
             (setq default-directory ,temp-root)
             (mapc (lambda (s) (with-buttercup-filesystem--init s ".")) ,spec)
             ,@forms)
         (delete-directory ,temp-root t)
         (setq default-directory ,old-dd)))))


(provide 'buttercup-filesystem)
;;; buttercup-filesystem.el ends here
