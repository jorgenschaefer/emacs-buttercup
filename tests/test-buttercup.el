;;; buttercup-test.el --- Tests for buttercup.el -*-lexical-binding:t-*-

;; Copyright (C) 2015  Jorgen Schaefer <contact@jorgenschaefer.de>

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
;;
;; Define test-suites to test buttercup itself. This test suite
;; should pass for all Emacs versions defined in the .travis.yml file
;; in the project directory root.

;;; Code:

(require 'buttercup)
(require 'autoload)
(require 'ert)
(require 'cl-lib)

(defun make-list-of-closures (items)
  "For each element of ITEMS, return a closure returning it."
  (mapcar (lambda (item)
            (lambda () item))
          items))

;;;;;;;;;;
;;; expect

(describe "The buttercup-failed signal"
  (it "can be raised"
    (expect (signal 'buttercup-failed t)
            :to-throw
            'buttercup-failed)))

(describe "The buttercup-pending signal"
  (it "can be raised"
    (expect (signal 'buttercup-pending t)
            :to-throw
            'buttercup-pending)))

(describe "The `expect' form"
  (it "with a matcher should translate to the function call with closures"
    (let ((expansion (macroexpand '(expect (+ 1 1) :to-equal 2))))
      (expect (length expansion) :to-equal 4)
      (expect (nth 0 expansion) :to-be 'buttercup-expect)
      (expect (functionp (nth 1 expansion)))
      (expect (nth 2 expansion) :to-be :to-equal)
      (expect (functionp (nth 3 expansion)))))

  (it "with no matcher should use `:to-be-truthy' as the matcher"
    (let ((expansion (macroexpand '(expect (equal (+ 1 1) 2)))))
      (expect (length expansion) :to-equal 3)
      (expect (nth 0 expansion) :to-be 'buttercup-expect)
      (expect (functionp (nth 1 expansion)))
      (expect (nth 2 expansion) :to-be :to-be-truthy))))

(describe "The `buttercup-expect' function"
  (describe "with a function as a matcher argument"
    (it "should not raise an error if the function returns true"
      (expect (buttercup-expect
               (lambda () t)
               #'eq
               (lambda () t))
              :not :to-throw
              'buttercup-failed))

    (it "should raise an error if the function returns false"
      (expect (buttercup-expect
               (lambda () t)
               #'eq
               (lambda () nil))
              :to-throw
              'buttercup-failed)))

  (describe "with a matcher argument"
    (buttercup-define-matcher :always-true (a) t)
    (buttercup-define-matcher :always-false (a) nil)

    (it "should not raise an error if the matcher returns true"
      (expect (buttercup-expect (lambda () 1) :always-true)
              :not :to-throw
              'buttercup-failed))

    (it "should raise an error if the matcher returns false"
      (expect (buttercup-expect (lambda () 1) :always-false)
              :to-throw
              'buttercup-failed))))

(describe "The `buttercup-fail' function"
  (it "should raise a signal with its arguments"
    (expect (buttercup-fail "Explanation" )
            :to-throw
            'buttercup-failed "Explanation")))

(describe "The `assume' form"
  (it "should raise a signal if the condition is nil"
    (expect (assume nil "Explanation")
            :to-throw
            'buttercup-pending "!! CANCELLED !! Explanation"))

  (it "should show the format if no message is given"
    (expect (assume (< 1 0))
            :to-throw
            'buttercup-pending "!! CANCELLED !! (< 1 0) => nil"))

  (it "should not raise a signal if the condition is non-nil"
    (expect (assume 'non-nil "Explanation")
            :not :to-throw)))

(describe "The `buttercup-skip' function"
  (it "should raise a signal with its arguments"
    (expect (buttercup-skip "Explanation" )
            :to-throw
            'buttercup-pending "Explanation")))

(buttercup-define-matcher :test-matcher (a b)
  (+ (funcall a) (funcall b)))

(describe "The `buttercup-define-matcher' macro"
  (it "should create a matcher usable by apply-matcher"
    (expect (buttercup--apply-matcher
             :test-matcher (make-list-of-closures '(1 2)))
            :to-equal
            3)))

(describe "The `buttercup--apply-matcher' function"
  (it "should work with functions"
    (expect (buttercup--apply-matcher
             #'+
             (make-list-of-closures '(1 2)))
            :to-equal
            3))

  (it "should work with matchers"
    (expect (buttercup--apply-matcher
             :test-matcher (make-list-of-closures '(1 2)))
            :to-equal
            3))

  (it "should fail if the matcher is not defined"
    (expect (buttercup--apply-matcher
             :not-defined (make-list-of-closures '(1 2)))
            :to-throw)))

;;;;;;;;;;;;;;;;;;;;;
;;; Built-in matchers

;; Are tested in README.md


(buttercup-define-matcher-for-unary-function :test-to-be-truthy identity)

(describe "The :buttercup-define-matcher-for-unary-function helper"
  (it "should not modify match data"
    (string-match ".." "foo")
    (expect t :test-to-be-truthy)
    (expect (match-end 0) :to-equal 2)))

(buttercup-define-matcher-for-binary-function :test-to-be-eq eq)

(describe "The :buttercup-define-matcher-for-binary-function helper"
  (it "should not modify match data"
    (string-match ".." "foo")
    (expect t :test-to-be-eq t)
    (expect (match-end 0) :to-equal 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Suite and spec data structures

(describe "The `buttercup-suite-add-child' function"
  (it "should add an element at the end of the list"
    (let* ((specs (list (make-buttercup-spec)
                        (make-buttercup-spec)
                        (make-buttercup-spec)))
           (suite (make-buttercup-suite :children specs))
           (spec (make-buttercup-spec)))

      (buttercup-suite-add-child suite spec)

      (expect (buttercup-suite-children suite)
              :to-equal
              (append specs (list spec)))))

  (it "should add an element even if the list is empty"
    (let ((suite (make-buttercup-suite :children nil))
          (spec (make-buttercup-spec)))

      (buttercup-suite-add-child suite spec)

      (expect (buttercup-suite-children suite)
              :to-equal
              (list spec))))

  (it "should add the parent to the child"
    (let ((parent (make-buttercup-suite))
          (child (make-buttercup-suite)))

      (buttercup-suite-add-child parent child)

      (expect (buttercup-suite-parent child)
              :to-equal
              parent))))

(describe "The `buttercup-suite-parents' function"
  (it "should return the list of parents for a suite"
    (let ((grandparent (make-buttercup-suite))
          (parent (make-buttercup-suite))
          (child (make-buttercup-suite)))
      (buttercup-suite-add-child grandparent parent)
      (buttercup-suite-add-child parent child)

      (expect (buttercup-suite-parents child)
              :to-equal
              (list parent grandparent)))))

(describe "The `buttercup-spec-parents' function"
  (it "should return the list of parents for a spec"
    (let ((grandparent (make-buttercup-suite))
          (parent (make-buttercup-suite))
          (child (make-buttercup-spec)))
      (buttercup-suite-add-child grandparent parent)
      (buttercup-suite-add-child parent child)

      (expect (buttercup-spec-parents child)
              :to-equal
              (list parent grandparent)))))

(describe "The `buttercup-suites-total-specs-defined' function"
  (it "should return the number of specs in a list of suites"
    (let ((su1 (make-buttercup-suite :description "su1"))
          (su2 (make-buttercup-suite :description "su2"))
          (sp1 (make-buttercup-spec :description "sp1"))
          (sp2 (make-buttercup-spec :description "sp2")))
      (buttercup-suite-add-child su1 su2)
      (buttercup-suite-add-child su1 sp1)
      (buttercup-suite-add-child su2 sp2)

      (expect (buttercup-suites-total-specs-defined (list su1))
              :to-equal
              2))))

(describe "The `buttercup-suites-total-specs-failed' function"
  (it "should return the number of failed specs in a list of suites"
    (let ((su1 (make-buttercup-suite :description "su1"))
          (su2 (make-buttercup-suite :description "su2"))
          (sp1 (make-buttercup-spec :description "sp1"))
          (sp2 (make-buttercup-spec :description "sp2"
                                    :status 'failed)))
      (buttercup-suite-add-child su1 su2)
      (buttercup-suite-add-child su1 sp1)
      (buttercup-suite-add-child su2 sp2)

      (expect (buttercup-suites-total-specs-failed (list su1))
              :to-equal
              1))))

(describe "The `buttercup-suite-full-name' function"
  (let (su1 su2)
    (before-each
      (setq su1 (make-buttercup-suite :description "su1")
            su2 (make-buttercup-suite :description "su2"))
      (buttercup-suite-add-child su1 su2))

    (it "should return the full name of a suite without parents"
      (expect (buttercup-suite-full-name su1)
              :to-equal
              "su1"))

    (it "should return the full name of a suite with parents"
      (expect (buttercup-suite-full-name su2)
              :to-equal
              "su1 su2"))))

(describe "The `buttercup-spec-full-name' function"
  (let (su1 su2 sp1 sp2)
    (before-each
      (setq su1 (make-buttercup-suite :description "su1")
            su2 (make-buttercup-suite :description "su2")
            sp1 (make-buttercup-spec :description "sp1")
            sp2 (make-buttercup-spec :description "sp2"))
      (buttercup-suite-add-child su1 su2)
      (buttercup-suite-add-child su2 sp2))

    (it "should return the full name of a spec without parents"
      (expect (buttercup-spec-full-name sp1)
              :to-equal
              "sp1"))

    (it "should return the full name of a spec with parents"
      (expect (buttercup-spec-full-name sp2)
              :to-equal
              "su1 su2 sp2"))))

(describe "The `buttercup-elapsed-time' function"
  (let ((spytime (current-time)))
    (before-each
      (spy-on 'current-time
              :and-call-fake
              (lambda ()
                (setq spytime (time-add spytime (seconds-to-time 1.5))))))
    (it "should report elapsed time for suites"
      (let ((suite (make-buttercup-suite)))
        (buttercup--set-start-time suite)
        (buttercup--set-end-time suite)
        (expect (buttercup-elapsed-time suite)
                :to-equal (seconds-to-time 1.5))))
    (it "should report elapsed time for specs"
      (let ((spec (make-buttercup-spec)))
        (buttercup--set-start-time spec)
        (buttercup--set-end-time spec)
        (expect (buttercup-elapsed-time spec)
                :to-equal (seconds-to-time 1.5))))))

(defmacro with-local-buttercup (&rest body)
  "Execute BODY with local buttercup state variables."
  (declare (debug t) (indent defun))
  `(let (buttercup--after-all
         buttercup--after-each
         buttercup--before-all
         buttercup--before-each
         buttercup--cleanup-functions
         buttercup--current-suite
         (buttercup-reporter #'ignore)
         buttercup-suites
         (buttercup-warning-buffer-name " *ignored buttercup warnings*"))
     ,@body))

(describe "The `buttercup--run-suite' function"
  (before-each
    (spy-on 'buttercup--set-start-time :and-call-through)
    (spy-on 'buttercup--set-end-time :and-call-through))
  (it "should set start and end time of the suite"
    (with-local-buttercup
      (let ((suite (make-buttercup-suite)))
        (buttercup--run-suite suite)
        (expect 'buttercup--set-start-time :to-have-been-called-times 1)
        (expect (buttercup-suite-or-spec-time-started suite)
                :not :to-be nil)
        (expect 'buttercup--set-end-time :to-have-been-called-times 1)
        (expect (buttercup-suite-or-spec-time-ended suite)
                :not :to-be nil)))))

(describe "The `buttercup--run-spec' function"
    (before-each
      (spy-on 'buttercup--set-start-time :and-call-through)
      (spy-on 'buttercup--set-end-time :and-call-through))
    (it "should set start and end time of the spec"
       (with-local-buttercup
        (let ((spec (make-buttercup-spec)))
          (buttercup--run-spec spec)
          (expect 'buttercup--set-start-time :to-have-been-called-times 1)
          (expect (buttercup-suite-or-spec-time-started spec)
                  :not :to-be nil)
          (expect 'buttercup--set-end-time :to-have-been-called-times 1)
          (expect (buttercup-suite-or-spec-time-ended spec)
                  :not :to-be nil)))))

;;;;;;;;;;;;;;;;;;;;
;;; Suites: describe

(describe "The `describe' macro"
  (it "should expand to a simple call to the buttercup-describe function"
    (expect (macroexpand '(describe "description" (+ 1 1)))
            :to-equal
            '(buttercup-describe "description" (lambda () (+ 1 1)))))

  (it "should support the :var argument"
    (expect (macroexpand '(describe "description" :var (foo bar) (+ foo bar)))
            :to-equal
            '(buttercup-describe "description"
                                 (lambda () (let (foo bar) (+ foo bar))))))
  (it "should support the :var* argument"
    (expect (macroexpand '(describe "description" :var* (foo bar) (+ foo bar)))
            :to-equal
            '(buttercup-describe "description"
                                 (lambda () (let* (foo bar) (+ foo bar)))))))

(describe "The `buttercup-describe' function"
  (it "should run the enclosing body"
    (let ((it-ran nil))
      (buttercup-describe "foo" (lambda () (setq it-ran t)))
      (expect it-ran)))

  (it "should set the `buttercup-suites' variable"
    (let ((buttercup-suites nil)
          (description "test to set global value"))
      (buttercup-describe description (lambda () nil))
      (expect (buttercup-suite-description (car buttercup-suites))
              :to-equal
              description)))

  (it "should add child suites when called nested"
    (let ((buttercup-suites nil)
          (desc1 "description1")
          (desc2 "description2"))

      (buttercup-describe
       desc1
       (lambda ()
         (buttercup-describe
          desc2
          (lambda () nil))))

      (expect (buttercup-suite-description (car buttercup-suites))
              :to-equal
              desc1)
      (let ((child-suite (car (buttercup-suite-children
                               (car buttercup-suites)))))
        (expect (buttercup-suite-description child-suite)
                :to-equal
                desc2)))))

;;;;;;;;;;;;;
;;; Specs: it

(describe "The `it' macro"
  (it "should expand to a call to the `buttercup-it' function"
    (expect (macroexpand '(it "description" body))
            :to-equal
            '(buttercup-it "description"
               (lambda ()
                 (buttercup-with-converted-ert-signals
                   body)))))

  (it "without argument should expand to xit."
    (expect (macroexpand '(it "description"))
            :to-equal
            '(buttercup-xit "description"))))

(describe "The `buttercup-it' function"
  (it "should fail if not called from within a describe form"
    (expect (let ((buttercup--current-suite nil))
              (buttercup-it "" (lambda ())))
            :to-throw))

  (it "should add a spec to the current suite"
    (let ((buttercup--current-suite (make-buttercup-suite)))
      (buttercup-it "the test spec"
        (lambda () 23))
      (let ((spec (car (buttercup-suite-children buttercup--current-suite))))
        (expect (buttercup-spec-description spec)
                :to-equal
                "the test spec")
        (expect (funcall (buttercup-spec-function spec))
                :to-equal
                23)))))

;;;;;;;;;;;;;;;;;;;;;;
;;; Setup and Teardown

(describe "The `before-each' macro"
  (it "expands to a function call"
    (expect (macroexpand '(before-each (+ 1 1)))
            :to-equal
            '(buttercup-before-each (lambda () (+ 1 1))))))

(describe "The `buttercup-before-each' function"
  (it "adds its argument to the before-each list of the current suite"
    (let* ((suite (make-buttercup-suite))
           (buttercup--current-suite suite))
      (buttercup-before-each 23)

      (expect (buttercup-suite-before-each suite)
              :to-equal
              (list 23)))))

(describe "The `after-each' macro"
  (it "expands to a function call"
    (expect (macroexpand '(after-each (+ 1 1)))
            :to-equal
            '(buttercup-after-each (lambda () (+ 1 1))))))

(describe "The `buttercup-after-each' function"
  (it "adds its argument to the after-each list of the current suite"
    (let* ((suite (make-buttercup-suite))
           (buttercup--current-suite suite))
      (buttercup-after-each 23)

      (expect (buttercup-suite-after-each suite)
              :to-equal
              (list 23)))))

(describe "The `before-all' macro"
  (it "expands to a function call"
    (expect (macroexpand '(before-all (+ 1 1)))
            :to-equal
            '(buttercup-before-all (lambda () (+ 1 1))))))

(describe "The `buttercup-before-all' function"
  (it "adds its argument to the before-all list of the current suite"
    (let* ((suite (make-buttercup-suite))
           (buttercup--current-suite suite))
      (buttercup-before-all 23)

      (expect (buttercup-suite-before-all suite)
              :to-equal
              (list 23)))))

(describe "The `after-all' macro"
  (it "expands to a function call"
    (expect (macroexpand '(after-all (+ 1 1)))
            :to-equal
            '(buttercup-after-all (lambda () (+ 1 1))))))

(describe "The `buttercup-after-all' function"
  (it "adds its argument to the after-all list of the current suite"
    (let* ((suite (make-buttercup-suite))
           (buttercup--current-suite suite))
      (buttercup-after-all 23)

      (expect (buttercup-suite-after-all suite)
              :to-equal
              (list 23)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Disabled Suites: xdescribe

(describe "The `xdescribe' macro"
  (it "expands directly to a function call"
    (expect (macroexpand '(xdescribe "bla bla" (+ 1 1)))
            :to-equal
            '(buttercup-describe "bla bla"
                                 (lambda ()
                                   (signal 'buttercup-pending "PENDING")))))

  (it "changes contained it-specs to pending specs"
    (expect (macroexpand-all
             '(xdescribe "bla bla"
                (let ((a 1) b (c 2) (d (it "nested" (+ 1 1))))
                  (it "spec1" (+ 1 1))
                  (describe "inner suite"
                    (it "inner spec"))
                  (xit "spec2" (+ 1 1)))))
            :to-equal
            '(buttercup-describe
              "bla bla"
              #'(lambda ()
                  (buttercup-xit "nested")
                  (buttercup-xit "spec1")
                  (buttercup-describe
                   "inner suite"
                   #'(lambda ()
                       (buttercup-xit "inner spec")
                       (signal 'buttercup-pending "PENDING")))
                  (buttercup-xit "spec2")
                  (signal 'buttercup-pending "PENDING")))))

  (it "should add a pending suite"
    (let ((buttercup--current-suite nil)
          (buttercup-suites nil))
      (xdescribe
       "bla bla"
       (lambda () nil))
      (expect (buttercup-suite-status (car buttercup-suites))
              :to-be
              'pending))))

;;;;;;;;;;;;;;;;;;;;;;
;;; Pending Specs: xit

(describe "The `xit' macro"
  (it "expands directly to a function call"
    (expect (macroexpand '(xit "bla bla" (+ 1 1)))
            :to-equal
            '(buttercup-xit "bla bla"))))

(describe "The `buttercup-xit' function"
  (it "should be a no-op"
    (expect
     (let ((buttercup--current-suite (make-buttercup-suite)))
       (buttercup-xit
           "bla bla"
         (lambda () (error "Should not happen")))))
    :not :to-throw)

  (it "should add a function that raises a pending signal"
    (let ((buttercup--current-suite (make-buttercup-suite)))
      (buttercup-xit "bla bla" (lambda ()
                                 (error "Should not happen")))
      (expect (funcall
               (buttercup-spec-function
                (car (buttercup-suite-children buttercup--current-suite))))
              :to-throw 'buttercup-pending)))

  (it "should mark the suite as pending"
    (let ((buttercup--current-suite (make-buttercup-suite)))
      (buttercup-xit "bla bla" (lambda ()))
      (expect (buttercup-spec-status
               (car (last (buttercup-suite-children
                           buttercup--current-suite))))
              :to-be 'pending)))

  (it "should set the failure description to PENDING"
    (let ((buttercup--current-suite (make-buttercup-suite))
          spec)
      (buttercup-xit "bla bla")
      (setq spec (car (buttercup-suite-children buttercup--current-suite)))
      (buttercup--update-with-funcall spec (buttercup-spec-function spec))
      (expect (buttercup-suite-or-spec-failure-description spec) :to-equal "PENDING"))))

;;;;;;;;;
;;; Spies

(describe "The Spy "
  (let (saved-test-function
        saved-test-command
        saved-test-function-throws-on-negative)
    ;; We use `before-all' here because some tests need to access the
    ;; same function as previous tests in order to work, so overriding
    ;; the function before each test would invalidate those tests.
    ;; Unfortunately there's no way to do this lexically, so these
    ;; function definitions are leaked after the tests run.
    (before-all
      (setq saved-test-function (and (fboundp 'test-function)
                                     (symbol-function 'test-function))
            saved-test-command (and (fboundp 'test-command)
                                    (symbol-function 'test-command))
            saved-test-function-throws-on-negative
            (and (fboundp 'test-function-throws-on-negative)
                 (symbol-function 'test-function-throws-on-negative)))
      (fset 'test-function (lambda (a b)
                             (+ a b)))
      (fset 'test-command (lambda ()
                            (interactive)
                            t))
      (fset 'test-function-throws-on-negative
            (lambda (x) (if (>= x 0) x (error "x is less than zero")))))
    (after-all
      (if saved-test-function
          (fset 'test-function saved-test-function)
        (fmakunbound 'test-function))
      (if saved-test-command
          (fset 'test-command saved-test-command)
        (fmakunbound 'test-command))
      (if saved-test-function-throws-on-negative
          (fset 'test-test-function-throws-on-negative
                test-function-throws-on-negative)
        (fmakunbound 'test-function-throws-on-negative)))

    (describe "`spy-on' function"
      (it "replaces a symbol's function slot"
        (spy-on 'test-function)
        (expect (test-function 1 2) :to-be nil))

      (it "restores the old value after a spec run"
        (expect (test-function 1 2) :to-equal 3))

      (it "allows a spied-on command to be executed as a command"
        (spy-on 'test-command)
        (expect (commandp 'test-command))
        (expect (command-execute 'test-command)
                :not :to-throw)
        (expect 'test-command :to-have-been-called))

      (it "can spy on autoloaded functions"
        (let* ((function-file (make-temp-file "test-file-" nil ".el"))
               (function-name 'test-autoloaded-function)
               (defun-form `(defun ,function-name ()
                              "An autoloaded function"
                              :loaded-successfully))
               (autoload-form (make-autoload defun-form function-file)))
          (unwind-protect
              (progn
                ;; Create the real function in a file
                (with-temp-file function-file
                  (insert ";; -*-lexical-binding:t-*-\n"
                          (pp-to-string defun-form)))
                ;; Define the autoload for the function
                (fmakunbound function-name)
                (eval autoload-form)
                (expect (autoloadp (symbol-function function-name)))
                (spy-on function-name :and-call-through)
                (expect (not (autoloadp (symbol-function function-name))))
                (expect (funcall function-name)
                        :to-be :loaded-successfully))
            (delete-file function-file nil))))

      (it "only accepts ARG for keywords that use it"
        (expect
         (spy-on 'test-function :and-call-through :arg-not-allowed)
         :to-throw)
        (expect
         (spy-on 'test-function nil :arg-not-allowed)
         :to-throw)
        (expect
         (spy-on 'test-function :and-throw-error)
         :not :to-throw)
        (expect
         (test-function 1 2)
         :to-throw 'error)))

    (describe ":to-have-been-called matcher"
      (before-each
        (spy-on 'test-function))

      (it "returns false if the spy was not called"
        (expect (buttercup--apply-matcher
                 :to-have-been-called
                 (list (lambda () 'test-function)))
                :to-be
                nil))

      (it "returns true if the spy was called at all"
        (test-function 1 2 3)
        (expect (buttercup--apply-matcher
                 :to-have-been-called
                 (list (lambda () 'test-function)))
                :to-be
                t)))

    (describe ":to-have-been-called-with matcher"
      (before-each
        (spy-on 'test-function))

      (it "returns false if the spy was not called at all"
        (expect (buttercup--apply-matcher
                 :to-have-been-called-with
                 (make-list-of-closures '(test-function 1 2 3)))
                :to-equal
                (cons nil
                      "Expected `test-function' to have been called with (1 2 3), but it was not called at all")))

      (it "returns false if the spy was called with different arguments"
        (test-function 3 2 1)
        (expect (buttercup--apply-matcher
                 :to-have-been-called-with
                 (make-list-of-closures '(test-function 1 2 3)))
                :to-equal
                (cons nil
                      "Expected `test-function' to have been called with (1 2 3), but it was called with (3 2 1)")))

      (it "returns true if the spy was called with those arguments"
        (test-function 1 2 3)
        (expect (buttercup--apply-matcher
                 :to-have-been-called-with
                 (make-list-of-closures '(test-function 1 2 3)))
                :to-be
                t)))

    (describe ":to-have-been-called-times matcher"
      (before-each
        (spy-on 'test-function))

      (it "returns error if the spy was called less than expected"
        (expect (buttercup--apply-matcher
                 :to-have-been-called-times
                 (make-list-of-closures '(test-function 1)))
                :to-equal
                (cons nil
                      "Expected `test-function' to have been called 1 time, but it was called 0 times")))

      (it "returns error if the spy was called more than expected"
        (test-function)
        (test-function)
        (expect (buttercup--apply-matcher
                 :to-have-been-called-times
                 (make-list-of-closures '(test-function 1)))
                :to-equal
                (cons nil
                      "Expected `test-function' to have been called 1 time, but it was called 2 times")))

      (it "returns true if the spy was called the expected number of times"
        (test-function)
        (test-function)
        (expect (buttercup--apply-matcher
                 :to-have-been-called-times
                 (make-list-of-closures '(test-function 2)))
                :to-equal t))

      (it "use plural words in error message"
        (test-function)
        (test-function)
        (expect (buttercup--apply-matcher
                 :to-have-been-called-times
                 (make-list-of-closures '(test-function 3)))
                :to-equal
                (cons nil
                      "Expected `test-function' to have been called 3 times, but it was called 2 times")))

      (it "use singular expected word in error message"
        (expect (buttercup--apply-matcher
                 :to-have-been-called-times
                 (make-list-of-closures '(test-function 1)))
                :to-equal
                (cons nil
                      "Expected `test-function' to have been called 1 time, but it was called 0 times")))

      (it "use singular actual word in error message"
        (test-function)
        (expect (buttercup--apply-matcher
                 :to-have-been-called-times
                 (make-list-of-closures '(test-function 2)))
                :to-equal
                (cons nil
                      "Expected `test-function' to have been called 2 times, but it was called 1 time"))))

    (describe ":and-call-through keyword functionality"
      (before-each
        (spy-on 'test-function :and-call-through))

      (it "tracks calls to the function"
        (test-function 42 23)

        (expect 'test-function :to-have-been-called))

      (it "passes the arguments to the original function"
        (expect (test-function 2 3)
                :to-equal
                5)))

    (describe ":and-return-value keyword functionality"
      (before-each
        (spy-on 'test-function :and-return-value 23))

      (it "tracks calls to the function"
        (test-function 42 23)

        (expect 'test-function :to-have-been-called))

      (it "returns the specified value"
        (expect (test-function 2 3)
                :to-equal
                23))

      (it "works with strings"
	(spy-on 'test-function :and-return-value "return value")
	(expect (test-function 2 3)
		:to-equal
		"return value"))

      (it "works with vectors"
	(spy-on 'test-function :and-return-value [1 2 3 4])
	(expect (test-function 2 3)
		:to-equal
		[1 2 3 4]))

      (it "works with symbols"
	(spy-on 'test-function :and-return-value 'symbol)
	(expect (test-function 2 3)
		:to-equal
		'symbol))

      (it "works with conses"
	(spy-on 'test-function :and-return-value '(1 . 2))
	(expect (test-function 2 3)
		:to-equal
		(cons 1 2)))

      (it "works with lists"
	(spy-on 'test-function :and-return-value '(1 2 3))
	(expect (test-function 2 3)
		:to-equal
		'(1 2 3)))

      (it "works with alists"
	(spy-on 'test-function :and-return-value '((first . 1)
						   (second . 2)
						   (third . 3)))
	(expect (test-function 2 3)
		:to-equal
		'((first . 1)
		  (second . 2)
		  (third . 3)))))

    (describe ":and-call-fake keyword functionality"
      (before-each
        (spy-on 'test-function :and-call-fake (lambda (a b) 1001)))

      (it "tracks calls to the function"
        (test-function 42 23)

        (expect 'test-function :to-have-been-called))

      (it "returns the specified value"
        (expect (test-function 2 3)
                :to-equal
                1001)))

    (describe ":and-throw-error keyword functionality"
      (before-each
        (spy-on 'test-function :and-throw-error 'error))

      (it "throws an error when called"
        (expect (test-function 1 2)
                :to-throw
                'error "Stubbed error")))

    (describe "error-recording functionality"
      (before-each
        (spy-on 'test-function-throws-on-negative :and-call-through))

      (it "records the function as called even if it throws an error"
        (expect (test-function-throws-on-negative -5) :to-throw)
        (expect (buttercup--apply-matcher
                 :to-have-been-called
                 (list (lambda () 'test-function-throws-on-negative)))
                :to-be
                t))

      (it "counts both successful calls and calls that threw errors"
        (test-function-throws-on-negative 5)
        (expect (test-function-throws-on-negative -5) :to-throw)
        (expect (buttercup--apply-matcher
                 :to-have-been-called-times
                 (make-list-of-closures '(test-function-throws-on-negative 2)))
                :to-equal t))

      (it "records args to the function whether it throw an error or not"
        (test-function-throws-on-negative 5)
        (expect (test-function-throws-on-negative -5) :to-throw)
        (expect (buttercup--apply-matcher
                 :to-have-been-called-with
                 (make-list-of-closures '(test-function-throws-on-negative 5)))
                :to-be
                t)
        (expect (buttercup--apply-matcher
                 :to-have-been-called-with
                 (make-list-of-closures '(test-function-throws-on-negative -5)))
                :to-be
                t))

      (it "records the signal thrown by a call to the function"
        (test-function-throws-on-negative 5)
        (expect (test-function-throws-on-negative -5) :to-throw 'error)
        (expect (spy-context-thrown-signal
                 (spy-calls-first 'test-function-throws-on-negative))
                :to-be nil)
        (expect (car
                 (spy-context-thrown-signal
                  (spy-calls-most-recent 'test-function-throws-on-negative)))
                :to-be 'error)))))

;;;;;;;;;;;;;
;;; Reporters

(describe "The batch reporter"
  (let (parent-suite child-suite spec)
    (before-each
      (setq parent-suite (make-buttercup-suite :description "parent-suite")
            child-suite (make-buttercup-suite :description "child-suite")
            spec (make-buttercup-spec :description "spec"))
      (buttercup-suite-add-child parent-suite child-suite)
      (buttercup-suite-add-child child-suite spec)
      (spy-on 'buttercup--print))

    (describe "on the buttercup-started event"
      (it "should emit the number of specs"
        (let ((buttercup-reporter-batch--start-time nil)
              (buttercup-reporter-batch--failures nil))
          (buttercup-reporter-batch 'buttercup-started (list parent-suite)))

        (expect 'buttercup--print
                :to-have-been-called-with
                "Running %s specs.\n\n"
                1)))

    (describe "on the suite-started event"
      (it "should emit an indented suite description"
        (buttercup-reporter-batch 'suite-started child-suite)

        (expect 'buttercup--print
                :to-have-been-called-with
                "%s%s\n"
                "  "
                "child-suite")))

    (describe "on the spec-started event"
      (it "should emit an indented spec description"
        (buttercup-reporter-batch 'spec-started spec)

        (expect 'buttercup--print
                :to-have-been-called-with
                "%s%s"
                "    "
                "spec")))

    (describe "on the spec-done event"
      (it "should print no status tag for a passed spec"
        (buttercup--set-start-time spec)
        (setf (buttercup-spec-failure-description spec) "DONTSHOW")
        (buttercup--set-end-time spec)
        (buttercup-reporter-batch 'spec-done spec)

        (expect (mapconcat (apply-partially #'apply #'format)
                           (spy-calls-all-args 'buttercup--print)
                           "")
                :to-match "^\\s-*([0-9]+\\.[0-9]+\\(h\\|m\\|m?s\\))\n$"))

      (it "should say FAILED for a failed spec"
        (setf (buttercup-spec-status spec) 'failed)

        (let ((buttercup-reporter-batch--failures nil))
          (buttercup-reporter-batch 'spec-done spec))

        (expect (mapconcat (apply-partially #'apply #'format)
                           (spy-calls-all-args 'buttercup--print)
                           "")
                :to-match "FAILED\\(\\s-+.*\\)?\n$"))

      (it "should output the failure-description for a pending spec"
        (setf (buttercup-spec-status spec) 'pending
              (buttercup-spec-failure-description spec) "DESCRIPTION")
        (let ((buttercup-reporter-batch--failures nil))
          (buttercup-reporter-batch 'spec-done spec))
        (expect (mapconcat (apply-partially #'apply #'format)
                           (spy-calls-all-args 'buttercup--print) "")
                :to-match "DESCRIPTION\\(\\s-+.*\\)?\n$"))

      (it "should throw an error for an unknown spec status"
        (setf (buttercup-spec-status spec) 'unknown)

        (expect (buttercup-reporter-batch 'spec-done spec)
                :to-throw))

      (it "should print the elapsed time for all specs"
        (dolist (state '(pending failed passed))
          (spy-calls-reset 'buttercup--print)
          (setq spec (make-buttercup-spec :description "spec"
                                          :parent child-suite
                                          :status state
                                          :failure-description ""))
          (buttercup--set-start-time spec)
          (buttercup--set-end-time spec)
          (let ((buttercup-reporter-batch--failures nil))
            (buttercup-reporter-batch 'spec-done spec))

          (expect (mapconcat (apply-partially #'apply #'format)
                             (spy-calls-all-args 'buttercup--print)
                             "")
                  :to-match " ([0-9]+\\(\\.[0-9]+\\)?\\(h\\|m\\|m?s\\))\n$"))))

    (describe "on the suite-done event"
      (it "should emit a newline at the end of the top-level suite"
        (buttercup-reporter-batch 'suite-done parent-suite)

        (expect 'buttercup--print :to-have-been-called-with "\n"))

      (it "should not emit anything at the end of other suites"
        (buttercup-reporter-batch 'suite-done child-suite)

        (expect 'buttercup--print :not :to-have-been-called)))

    (describe "on the buttercup-done event"
      ;; This is a lie. It should do a ton more stuff. We should test
      ;; that, too.
      (it "should handle the end event"
        (buttercup-reporter-batch 'buttercup-done nil))

      (it "should not raise any error even if a spec failed"
        (setf (buttercup-spec-status spec) 'failed)

        (expect (buttercup-reporter-batch 'buttercup-done (list spec))
                :not :to-throw)))

    (describe "on an unknown event"
      (it "should raise an error"
        (expect (buttercup-reporter-batch 'unknown-event nil)
                :to-throw)))))

(describe "The `buttercup-run' function"
  :var (parent-suite child-suite spec reporter)
  (before-each
    (ignore reporter)
    (setf (symbol-function 'reporter) (lambda (event arg) (ignore event arg)))
    (setq parent-suite (make-buttercup-suite :description "parent-suite")
          child-suite (make-buttercup-suite :description "child-suite")
          spec (make-buttercup-spec :description "spec"))
    (buttercup-suite-add-child parent-suite child-suite)
    (buttercup-suite-add-child child-suite spec)
    (spy-on 'reporter))
  (it "should raise an error if at least one spec failed"
    (setf (buttercup-spec-status spec) 'failed)
    (cl-letf (((symbol-function 'buttercup--run-suite) #'ignore)
              (buttercup-reporter 'reporter))
      (let ((buttercup-suites (list parent-suite)))
        (expect (buttercup-run) :to-throw))))
  (it "should call the reporter twice with events buttercup-started and -done"
    (cl-letf (((symbol-function 'buttercup--run-suite) #'ignore)
              (buttercup-reporter 'reporter))
      (let ((buttercup-suites (list parent-suite)))
        (expect (buttercup-run) :not :to-throw)
        (expect 'reporter :to-have-been-called-times 2)
        (expect 'reporter :to-have-been-called-with 'buttercup-started buttercup-suites)
        (expect 'reporter :to-have-been-called-with 'buttercup-done buttercup-suites)))
    )
  (it "should call `buttercup--run-suite once per suite"
    (let ((buttercup-suites (list parent-suite)) runner)
      (ignore runner)
      (setf (symbol-function 'runner) (lambda (suite) (ignore suite)))
      (spy-on 'runner)
      (cl-letf (((symbol-function 'buttercup--run-suite) #'runner)
                (buttercup-reporter 'reporter)
                (buttercup-suites (make-list 5 parent-suite)))
        (expect (buttercup-run) :not :to-throw)
        (expect 'runner :to-have-been-called-times 5)))))

(describe "The `buttercup--print' function"
  (before-each
    (spy-on 'send-string-to-terminal))

  (it "should send a formatted string to the terminal"
    (buttercup--print "Hello, %s" "world")

    (expect 'send-string-to-terminal
            :to-have-been-called-with
            "Hello, world")))

;;;;;;;;;;;;;;;;;;;;;
;;; ERT Compatibility

(describe "Buttercup's ERT compatibility wrapper"
  (it "should convert `ert-test-failed' into `buttercup-failed"
    (expect
     (buttercup-with-converted-ert-signals
       (should (equal 1 2)))
     :to-throw 'buttercup-failed))
  (it "should convert `ert-test-skipped' into `buttercup-pending"
    (assume (functionp 'ert-skip) "Loaded ERT version does not provide `ert-skip'")
    (expect
     (buttercup-with-converted-ert-signals
       (ert-skip "Skipped this test"))
     :to-throw 'buttercup-pending)))

;;;;;;;;;;;;;
;;; Utilities

;; We can't test `buttercup--funcall' with buttercup, because the way
;; we get the backtrace from Emacs does not nest.

(let ((res (buttercup--funcall (lambda () (+ 2 3))))
      (expected '(passed 5 nil)))
  (when (not (equal res expected))
    (error "Expected passing buttercup--funcall to return `%S', not `%S'"
           expected res)))

(let ((res (buttercup--funcall (lambda () (/ 1 0)))))
  (when (not (and
              (equal (car res) 'failed)
              (equal (cadr res) '(error (arith-error)))))
    (error "Expected erroring buttercup--funcall not to return `%S'"
           res)))

;;;;;;;;;;;;;
;;; Buttercup-minor-mode

(describe "butter-minor-mode"

  (it "should fontify `describe' special form"
    (with-temp-buffer
      (emacs-lisp-mode)
      (buttercup-minor-mode 1)
      (font-lock-mode)
      (insert "(describe \"A test suite\" (it \"should fontify special keywords\"))")
      (font-lock-fontify-region (point-min) (point-max))
      (expect
       (text-property-any (point-min) (point-max) 'face 'font-lock-keyword-face)
       :to-equal 2)))

  (it "should fontify `it' special form"
    (with-temp-buffer
      (emacs-lisp-mode)
      (buttercup-minor-mode 1)
      (font-lock-mode)
      (insert "(describe \"A test suite\" (it \"should fontify special keywords\"))")
      (font-lock-fontify-region (point-min) (point-max))
      (expect
       (text-property-any 15 (point-max) 'face 'font-lock-keyword-face)
       :to-equal 27)))

  (it "should add special forms to `imenu'"
    (with-temp-buffer
      (require 'imenu)
      (emacs-lisp-mode)
      (buttercup-minor-mode 1)
      (insert "(describe \"A test suite\"
  (it \"should fontify special keywords\"))")
      (imenu--make-index-alist)
      (let ((suites (assoc "Test Suites" imenu--index-alist))
            (specs (assoc "Spec" imenu--index-alist)))
        (expect suites :to-be-truthy)
        (expect (length (cdr suites)) :to-equal 1)
        (expect (cl-caadr suites) :to-equal "A test suite")
        (expect specs :to-be-truthy)
        (expect (length (cdr specs)) :to-equal 1)
        (expect (cl-caadr specs) :to-equal "should fontify special keywords")))))

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:
(provide 'test-buttercup)
;;; test-buttercup.el ends here
