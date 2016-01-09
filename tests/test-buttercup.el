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

;;; Code:

(require 'buttercup)

;;;;;;;;;;
;;; expect

(describe "The buttercup-failed signal"
  (it "can be raised"
    (expect (lambda ()
              (signal 'buttercup-failed t))
            :to-throw
            'buttercup-failed)))

(describe "The buttercup-pending signal"
  (it "can be raised"
    (expect (lambda ()
              (signal 'buttercup-pending t))
            :to-throw
            'buttercup-pending)))

(describe "The `expect' form"
  (it "with a matcher should translate directly to the function call"
    (expect (macroexpand '(expect (+ 1 1) :to-equal 2))
            :to-equal
            '(buttercup-expect (+ 1 1) :to-equal 2)))

  (it "with a form argument should extract the matcher from the form"
    (expect (macroexpand '(expect (equal (+ 1 1) 2)))
            :to-equal
            '(buttercup-expect (+ 1 1) #'equal 2)))

  (it "with a single argument should pass it to the function"
    (expect (macroexpand '(expect t))
            :to-equal
            '(buttercup-expect t))))

(describe "The `buttercup-expect' function"
  (describe "with a single argument"
    (it "should not raise an error if the argument is true"
      (expect (lambda ()
                (buttercup-expect t))
              :not :to-throw
              'buttercup-failed))

    (it "should raise an error if the argument is false"
      (expect (lambda ()
                (buttercup-expect nil))
              :to-throw
              'buttercup-failed
              "Expected nil to be non-nil")))

  (describe "with a function as a matcher argument"
    (it "should not raise an error if the function returns true"
      (expect (lambda ()
                (buttercup-expect t #'eq t))
              :not :to-throw
              'buttercup-failed))

    (it "should raise an error if the function returns false"
      (expect (lambda ()
                (buttercup-expect t #'eq nil))
              :to-throw
              'buttercup-failed)))

  (describe "with a matcher argument"
    (buttercup-define-matcher :always-true (a) t)
    (buttercup-define-matcher :always-false (a) nil)

    (it "should not raise an error if the matcher returns true"
      (expect (lambda ()
                (buttercup-expect 1 :always-true))
              :not :to-throw
              'buttercup-failed))

    (it "should raise an error if the matcher returns false"
      (expect (lambda ()
                (buttercup-expect 1 :always-false))
              :to-throw
              'buttercup-failed))))

(describe "The `buttercup-fail' function"
  (it "should raise a signal with its arguments"
    (expect (lambda ()
              (buttercup-fail "Explanation" ))
            :to-throw
            'buttercup-failed "Explanation")))

(buttercup-define-matcher :test-matcher (a b)
  (+ a b))

(describe "The `buttercup-define-matcher' macro"
  (it "should create a matcher usable by apply-matcher"
    (expect (buttercup--apply-matcher :test-matcher '(1 2))
            :to-equal
            3)))

(describe "The `buttercup--apply-matcher'"
  (it "should work with functions"
    (expect (buttercup--apply-matcher #'+ '(1 2))
            :to-equal
            3))

  (it "should work with matchers"
    (expect (buttercup--apply-matcher :test-matcher '(1 2))
            :to-equal
            3))

  (it "should fail if the matcher is not defined"
    (expect (lambda ()
              (buttercup--apply-matcher :not-defined '(1 2)))
            :to-throw)))

;;;;;;;;;;;;;;;;;;;;;
;;; Built-in matchers

;; Are tested in README.md

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
  (let (su1 su2 sp1)
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

;;;;;;;;;;;;;;;;;;;;
;;; Suites: describe

(describe "The `describe' macro"
  (it "should expand to a simple call to the describe function"
    (expect (macroexpand '(describe "description" (+ 1 1)))
            :to-equal
            '(buttercup-describe "description" (lambda () (+ 1 1))))))

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
            '(buttercup-it "description" (lambda () body))))

  (it "without argument should expand to a pending signal raiser."
    (expect (macroexpand '(it "description"))
            :to-equal
            '(buttercup-it "description"
                           (lambda () (signal 'buttercup-pending "PENDING"))))))

(describe "The `buttercup-it' function"
  (it "should fail if not called from within a describe form"
    (expect (lambda ()
              (let ((buttercup--current-suite nil))
                (buttercup-it "" (lambda ()))))
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
            '(buttercup-xdescribe "bla bla" (lambda () (+ 1 1))))))

(describe "The `buttercup-xdescribe' function"
  (it "should be a no-op"
    (expect (lambda ()
              (buttercup-xdescribe
               "bla bla"
               (lambda () (error "should not happen"))))
            :not :to-throw))

  (it "should add a pending suite"
    (let ((buttercup--current-suite nil)
          (buttercup-suites nil))
      (buttercup-xdescribe
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
            '(buttercup-xit "bla bla" (lambda () (+ 1 1))))))

(describe "The `buttercup-xit' function"
  (it "should be a no-op"
    (expect (lambda ()
              (let ((buttercup--current-suite (make-buttercup-suite)))
                (buttercup-xit
                 "bla bla"
                 (lambda () (error "should not happen")))))
            :not :to-throw))

  (it "should add a function that raises a pending signal"
    (let ((buttercup--current-suite (make-buttercup-suite)))
      (buttercup-xit "bla bla" (lambda ()
                                 (error "should not happen")))
      (expect (buttercup-spec-function
               (car (buttercup-suite-children buttercup--current-suite)))
              :to-throw 'buttercup-pending))))

;;;;;;;;;
;;; Spies

(describe "The Spy "
  (let (test-function)
    (before-each
      (fset 'test-function (lambda (a b)
                             (+ a b))))

    (describe "`spy-on' function"
      (it "replaces a symbol's function slot"
        (spy-on 'test-function)
        (expect (test-function 1 2) :to-be nil))

      (it "restores the old value after a spec run"
        (expect (test-function 1 2) :to-equal 3)))

    (describe ":to-have-been-called matcher"
      (before-each
        (spy-on 'test-function))

      (it "returns false if the spy was not called"
        (expect (buttercup--apply-matcher :to-have-been-called
                                          '(test-function))
                :to-be
                nil))

      (it "returns true if the spy was called at all"
        (test-function 1 2 3)
        (expect (buttercup--apply-matcher :to-have-been-called
                                          '(test-function))
                :to-be
                t)))

    (describe ":to-have-been-called-with matcher"
      (before-each
        (spy-on 'test-function))

      (it "returns false if the spy was not called at all"
        (expect (buttercup--apply-matcher
                 :to-have-been-called-with '(test-function 1 2 3))
                :to-equal
                (cons nil
                      "Expected `test-function' to have been called with (1 2 3), but it was not called at all")))

      (it "returns false if the spy was called with different arguments"
        (test-function 3 2 1)
        (expect (buttercup--apply-matcher
                 :to-have-been-called-with '(test-function 1 2 3))
                :to-equal
                (cons nil
                      "Expected `test-function' to have been called with (1 2 3), but it was called with (3 2 1)")))

      (it "returns true if the spy was called with those arguments"
        (test-function 1 2 3)
        (expect (buttercup--apply-matcher
                 :to-have-been-called-with '(test-function 1 2 3))
                :to-be
                t)))

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
                23)))

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
        (expect (lambda () (test-function 1 2))
                :to-throw
                'error "Stubbed error")))))

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
        (let ((buttercup-reporter-batch--start-time nil))
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
      (it "should simply emit a newline for a passed spec"
        (buttercup-reporter-batch 'spec-done spec)

        (expect 'buttercup--print :to-have-been-called-with "\n"))

      (it "should say FAILED for a failed spec"
        (setf (buttercup-spec-status spec) 'failed)

        (let ((buttercup-reporter-batch--failures nil))
          (buttercup-reporter-batch 'spec-done spec))

        (expect 'buttercup--print :to-have-been-called-with "  FAILED\n"))

      (it "should throw an error for an unknown spec status"
        (setf (buttercup-spec-status spec) 'unknown)

        (expect (lambda ()
                  (buttercup-reporter-batch 'spec-done spec))
                :to-throw)))

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

      (it "should raise an error if at least one spec failed"
        (setf (buttercup-spec-status spec) 'failed)

        (expect (lambda ()
                  (buttercup-reporter-batch 'buttercup-done (list spec)))
                :to-throw)))

    (describe "on an unknown event"
      (it "should raise an error"
        (expect (lambda ()
                  (buttercup-reporter-batch 'unknown-event nil))
                :to-throw)))))

(describe "The `buttercup--print' function"
  (before-each
    (spy-on 'send-string-to-terminal))

  (it "should send a formatted string to the terminal"
    (buttercup--print "Hello, %s" "world")

    (expect 'send-string-to-terminal
            :to-have-been-called-with
            "Hello, world")))

;;;;;;;;;;;;;
;;; Utilities

;; We can't test `buttercup--funcall' with buttercup, because the way
;; we get the backtrace from Emacs does not nest.

(let ((res (buttercup--funcall (lambda () (+ 2 3)))))
  (when (not (equal res (list 'passed 5 nil)))
    (error "Expected passing buttercup--funcall not to return %S"
           res)))

(let ((res (buttercup--funcall (lambda () (/ 1 0)))))
  (when (not (equal res (list 'failed
                              '(error (arith-error))
                              (list '(t / 1 0)))))
    (error "Expected erroring buttercup--funcall not to return %S"
           res)))
