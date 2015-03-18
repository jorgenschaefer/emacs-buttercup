;;; buttercup.el --- Behavior-Driven Emacs Lisp Testing

;; Copyright (C) 2015  Jorgen Schaefer <contact@jorgenschaefer.de>

;; Version: 0.1
;; Author: Jorgen Schaefer <contact@jorgenschaefer.de>

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

;; The ideas for project were shamelessly taken from Jasmine
;; <https://jasmine.github.io>.

;; All the good ideas are theirs. All the problems are mine.

;;; Code:

(require 'cl)

;;;;;;;;;;
;;; expect

(define-error 'buttercup-failed
  "Buttercup test failed")

(define-error 'buttercup-error
  "Buttercup test raised an error")

(defun expect (arg &optional matcher &rest args)
  (if (not matcher)
      (when (not arg)
        (signal 'buttercup-failed
                (format "Expected %S to be non-nil" arg)))
    (let ((result (buttercup--apply-matcher matcher (cons arg args))))
      (if (consp result)
          (when (not (car result))
            (signal 'buttercup-failed
                    (cdr result)))
        (when (not result)
          (signal 'buttercup-failed
                  (format "Expected %S %S %S"
                          arg
                          matcher
                          (mapconcat (lambda (obj)
                                       (format "%S" obj))
                                     args
                                     " "))))))))

(defun buttercup-fail (explanation form)
  (signal 'buttercup-failed (cons explanation
                                  form)))

(defmacro buttercup-define-matcher (matcher args &rest body)
  "Define a matcher to be used in `expect'.

The BODY should return either a simple boolean, or a cons cell of
the form (RESULT . MESSAGE). If RESULT is nil, MESSAGE should
describe why the matcher failed. If RESULT is non-nil, MESSAGE
should describe why a negated matcher failed."
  (declare (indent defun))
  `(put ,matcher 'buttercup-matcher
        (lambda ,args
          ,@body)))

(defun buttercup--apply-matcher (matcher args)
  (let ((function (or (get matcher 'buttercup-matcher)
                      matcher)))
    (when (not (functionp function))
      (error "Not a test: %S" matcher))
    (apply function args)))

(buttercup-define-matcher :not (obj matcher &rest args)
  (let ((result (buttercup--apply-matcher matcher (cons obj args))))
    (if (consp result)
        (cons (not (car result))
              (cdr result))
      (not result))))

(buttercup-define-matcher :to-be (a b)
  (if (eq a b)
      (cons t (format "Expected %S not to be `eq' to %S" a b))
    (cons nil (format "Expected %S to be `eq' to %S" a b))))

(buttercup-define-matcher :to-equal (a b)
  (if (equal a b)
      (cons t (format "Expected %S not to `equal' %S" a b))
    (cons nil (format "Expected %S to `equal' %S" a b))))

(buttercup-define-matcher :to-match (text regexp)
  (if (string-match regexp text)
      (cons t (format "Expected %S to match the regexp %S"
                      text regexp))
    (cons nil (format "Expected %S not to match the regexp %S"
                      text regexp))))

(buttercup-define-matcher :to-be-truthy (arg)
  (if arg
      (cons t (format "Expected %S not to be true" arg))
    (cons nil (format "Expected %S to be true" arg))))

(buttercup-define-matcher :to-contain (seq elt)
  (if (member elt seq)
      (cons t (format "Expected %S not to contain %S" seq elt))
    (cons nil (format "Expected %S to contain %S" seq elt))))

(buttercup-define-matcher :to-be-less-than (a b)
  (if (< a b)
      (cons t (format "Expected %S not to be less than %S" a b))
    (cons nil (format "Expected %S to be less than %S" a b))))

(buttercup-define-matcher :to-be-greater-than (a b)
  (if (> a b)
      (cons t (format "Expected %S not to be greater than %S" a b))
    (cons nil (format "Expected %S to be greater than %S" a b))))

(buttercup-define-matcher :to-be-close-to (a b precision)
  (if (< (abs (- a b))
         (/ 1 (expt 10.0 precision)))
      (cons t (format "Expected %S not to be close to %S to %s positions"
                      a b precision))
    (cons nil (format "Expected %S to be greater than %S to %s positions"
                      a b precision))))

(buttercup-define-matcher :to-throw (function)
  (condition-case err
      (progn
        (funcall function)
        (cons nil (format "Expected %S to throw an error" function)))
    (error
     (cons t (format "Expected %S not to throw an error" function)))))

;;;;;;;;;;
;;; Suites

(cl-defstruct buttercup-suite
  description
  nested
  specs)

(defun buttercup-suite-add-nested (parent child)
  "Add a CHILD suite as a nested suite to a PARENT suite."
  (setf (buttercup-suite-nested parent)
        (append (buttercup-suite-nested parent)
                (list child))))

;;;;;;;;;;;;
;;; describe

(defvar buttercup-suites nil
  "The list of all currently defined Buttercup suites.")

(defvar buttercup--current-suite nil
  "The suite currently being defined.

Do not set this globally. It is let-bound by the `describe'
form.")

(defmacro describe (description &rest body)
  "Describe a suite of tests."
  (declare (indent 1))
  `(buttercup--describe-internal ,description (lambda () ,@body)))

(defun buttercup--describe-internal (description body-function)
  "Function to handle a `describe' form."
  (let* ((enclosing-suite buttercup--current-suite)
         (buttercup--current-suite (make-buttercup-suite
                                    :description description)))
    (funcall body-function)
    (if enclosing-suite
        (buttercup-suite-add-nested enclosing-suite
                                    buttercup--current-suite)
      (setq buttercup-suites (append buttercup-suites
                                     (list buttercup--current-suite))))))

;;;;;;
;;; it

(defmacro it (description &rest body)
  "Define a spec."
  (declare (indent 1))
  `(buttercup--it-internal ,description (lambda () ,@body)))

(defun buttercup--it-internal (description body-function)
  "Function to handle an `it' form."
  (when (not description)
    (error "`it' has to be called from within a `describe' form."))
  (buttercup-suite-add-nested buttercup--current-suite
                              (cons description
                                    body-function)))

;; (let* ((buttercup--descriptions (cons description
;;                                       buttercup--descriptions))
;;        (debugger (lambda (&rest args)
;;                    (let ((backtrace (buttercup--backtrace)))
;;                      ;; If we do not do this, Emacs will not this
;;                      ;; handler on subsequent calls. Thanks to ert
;;                      ;; for this.
;;                      (cl-incf num-nonmacro-input-events)
;;                      (signal 'buttercup-error (cons args backtrace)))))
;;        (debug-on-error t)
;;        (debug-ignored-errors '(buttercup-failed buttercup-error)))
;;   (buttercup-report 'enter nil buttercup--descriptions)
;;   (condition-case sig
;;       (progn
;;         (funcall body-function)
;;         (buttercup-report 'success nil buttercup--descriptions))
;;     (buttercup-failed
;;      (buttercup-report 'failure (cdr sig) buttercup--descriptions))
;;     (buttercup-error
;;      (buttercup-report 'error (cdr sig) buttercup--descriptions))))

;; (defun buttercup--backtrace ()
;;   (let* ((n 5)
;;          (frame (backtrace-frame n))
;;          (frame-list nil))
;;     (while frame
;;       (push frame frame-list)
;;       (setq n (1+ n)
;;             frame (backtrace-frame n)))
;;     frame-list))

;;;;;;;;;;;;;;;;
;;; Test Runners

(defun buttercup-run ()
  (if buttercup-suites
      (mapc #'buttercup-run-suite buttercup-suites)
    (error "No suites defined")))

(defun buttercup-run-suite (suite &optional level)
  (let* ((level (or level 0))
         (indent (make-string (* 2 level) ?\s)))
    (message "%s%s\n" indent (buttercup-suite-description suite))
    (dolist (sub (buttercup-suite-nested suite))
      (if (buttercup-suite-p sub)
          (progn
            (message "")
            (buttercup-run-suite sub (1+ level)))
        (message "%s%s"
                 (make-string (* 2 (1+ level)) ?\s)
                 (car sub))
        (funcall (cdr sub))))
    (message "")))

(defun buttercup-markdown-runner ()
  (let ((lisp-buffer (generate-new-buffer "elisp")))
    (dolist (file command-line-args-left)
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (while (re-search-forward "```lisp\n\\(\\(?:.\\|\n\\)*?\\)```"
                                  nil t)
          (let ((code (match-string 1)))
            (with-current-buffer lisp-buffer
              (insert code))))))
    (with-current-buffer lisp-buffer
      (setq lexical-binding t)
      (eval-buffer))
    (buttercup-run)))

(provide 'buttercup)
;;; buttercup.el ends here
