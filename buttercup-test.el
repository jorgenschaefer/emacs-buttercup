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

(describe "The buttercup-failed signal"
  (it "can be raised"
    (expect (lambda ()
              (signal 'buttercup-failed t))
            :to-throw
            'buttercup-failed)))

(describe "The buttercup-error signal"
  (it "can be raised"
    (expect (lambda ()
              (signal 'buttercup-error t))
            :to-throw
            'buttercup-error)))

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

(describe "The `buttercup-define-matcher' macro"
  (buttercup-define-matcher :test-matcher (a b)
    (+ a b))

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

;; Built-in matchers are tested in README.md

(describe "The `buttercup-suite-add-child' function"
  (it "should add an element at the end of the list"
    (let ((suite (make-buttercup-suite :children '(1 2 3))))

      (buttercup-suite-add-child suite 4)

      (expect (buttercup-suite-children suite)
              :to-equal
              '(1 2 3 4))))

  (it "should add an element even if the list is empty"
    (let ((suite (make-buttercup-suite :children nil)))

      (buttercup-suite-add-child suite 23)

      (expect (buttercup-suite-children suite)
              :to-equal
              '(23)))))
