;;; buttercup-test.el --- Tests for buttercup.el -*-lexical-binding:t-*-

;; Copyright (C) 2015, 2021  Jorgen Schaefer <contact@jorgenschaefer.de>

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
(require 'ansi-color)
(require 'ert)
(require 'ert-x)
(require 'cl-lib)
(require 'imenu)

(defun make-list-of-closures (items)
  "For each element of ITEMS, return a closure returning it."
  (mapcar (lambda (item)
            (lambda () item))
          items))

(defmacro with-local-buttercup (&rest body)
  "Execute BODY with local buttercup state variables.
Keyword arguments kan be used to override the values of certain
variables:
 :color       -> `buttercup-color'
 :frame-style -> `buttercup-stack-frame-style'
 :reporter    -> `buttercup-reporter'
 :suites      -> `buttercup-suites'
 :quiet       -> `buttercup-reporter-batch-quiet-statuses'
\n(fn &keys COLOR SUITES REPORTER &rest BODY)"
  (declare (debug t) (indent defun))
  ;; extract keyword arguments
  (let ((keys '(:color buttercup-color
                       :frame-style buttercup-stack-frame-style
                       :reporter buttercup-reporter
                       :suites buttercup-suites
                       :quiet buttercup-reporter-batch-quiet-statuses))
        extra-vars)
    (while (plist-member keys (car body))
      (push (list (plist-get keys (pop body)) (pop body)) extra-vars))
  `(let (buttercup--after-each
         buttercup--before-each
         (buttercup--cleanup-functions :invalid)
         buttercup--current-suite
         (buttercup-reporter #'ignore)
         buttercup-suites
         buttercup-color
         buttercup-reporter-batch-quiet-statuses
         buttercup-reporter-batch--suite-stack
         buttercup-reporter-batch--failures
         (buttercup-stack-frame-style 'crop)
         (buttercup-warning-buffer-name " *ignored buttercup warnings*")
         ,@(nreverse extra-vars))
     ,@body)))

(defmacro buttercup--test-with-tempdir (files &rest body)
  "Create FILES and execute BODY in a temporary directory.
FILES shall be a list of file names. An empty file with that name
will be created in the temporary directory. Any path prefix for a
file will be created in the temporary directory. Elements in FILE
can also be a list of up to two elements where the first is the
filename as above and the second is the file contents.
Return the value of the last form in BODY."
  (declare (debug t) (indent defun))
  (let ((tmproot (cl-gensym))
        (olddir (cl-gensym)))
    `(let ((,tmproot (make-temp-file "buttercup-test-temp-" t))
           (,olddir default-directory))
       (cl-labels ((make-file (file &optional content)
                     (setq file (expand-file-name file ,tmproot))
                     (make-directory (file-name-directory file) t)
                     (write-region (or content "") nil file)))
         (dolist (file ,files)
           (if (listp file)
               (apply #'make-file file)
             (make-file file))))
       ;; It is tempting to use unwind-protect or condition-case here,
       ;; but that will mask actual test failures by interfering with
       ;; the debugger installed by buttercup
       (cd ,tmproot)
       (progn ,@body)
       (cd ,olddir)
       (delete-directory ,tmproot t))))

(defun send-string-to-ansi-buffer (buffer string)
  "A `send-string-to-terminal' variant that sends STRING to BUFFER.
Any backspace, tab, newline, vertical tab, formfeed, or carriage
return in STRING will be translared to in-buffer movement to
emulate a terminal. Escape sequences in STRING are translated to
text properties using `ansi-color-apply'."
  (setq string (ansi-color-apply string))
  (cl-labels ((insert-owrt (text)
                 "Insert TEXT by first overwriting until end of line."
                 ;; Delete and insert separately. Otherwise characters
                 ;; with text properties may remain when the new and
                 ;; the old text share substrings.
                 (delete-region (point) ; only delete up to the end of line
                                (min (+ (point) (length text))
                                     (line-end-position)))
                 (insert text))
              (line-feed ()
                 "Go to beginning of next line, creating it if necessary."
                  (end-of-line)
                  (or (zerop (forward-line))
                      (insert-and-inherit "\n"))))
    (with-current-buffer buffer
      (let ((tab-width 8)          ; terminal uses 8 char tabs
            (indent-tabs-mode nil) ; make sure move-* does not insert tabs
            ;; default tab-stops (8 char interval)  Can be nil from 24.4 forward...
            (tab-stop-list (eval (car (get 'tab-stop-list 'standard-value))))
            ctrl-char)
        (save-match-data
          (while (string-match "\\(.*?\\)\\([\b\t\n\v\f\r]\\)\\([^z-a]*\\)" string)
            (insert-owrt (match-string 1 string))
            (setq ctrl-char (aref (match-string 2 string) 0)
                  string (match-string 3 string))
            (cl-case ctrl-char
              (?\b (unless (bolp) (backward-char)))
              (?\t (move-to-tab-stop))
              (?\n (line-feed))
              ((?\v ?\f) (let ((line-pos (current-column)))
                           (line-feed)
                           (move-to-column line-pos t)))
              (?\r (forward-line 0))))
          ;; print remaining text
          (insert-owrt string))))))


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
      (expect (buttercup--wrapper-fun-p (nth 1 expansion)))
      (expect (nth 2 expansion) :to-be :to-equal)
      (expect (functionp (nth 3 expansion)))
      (expect (buttercup--wrapper-fun-p (nth 3 expansion)))))

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
    (buttercup-define-matcher :always-true (_a) t)
    (buttercup-define-matcher :always-false (_a) nil)

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

;; Are tested in docs/writing-tests.md

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
  (it "should add an element at the end of the list and return it"
    (let* ((specs (list (make-buttercup-spec)
                        (make-buttercup-spec)
                        (make-buttercup-spec)))
           (suite (make-buttercup-suite :children specs))
           (spec (make-buttercup-spec)))

      (expect (buttercup-suite-add-child suite spec)
              :to-be spec)

      (expect (buttercup-suite-children suite)
              :to-equal
              (append specs (list spec)))))

  (it "should add an element even if the list is empty and return it"
    (let ((suite (make-buttercup-suite :children nil))
          (spec (make-buttercup-spec)))

      (expect (buttercup-suite-add-child suite spec)
              :to-be spec)

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

      (expect (buttercup-suite-or-spec-parents child)
              :to-equal
              (list parent grandparent)))))

(describe "The `buttercup-spec-parents' function"
  (it "should return the list of parents for a spec"
    (let ((grandparent (make-buttercup-suite))
          (parent (make-buttercup-suite))
          (child (make-buttercup-spec)))
      (buttercup-suite-add-child grandparent parent)
      (buttercup-suite-add-child parent child)

      (expect (buttercup-suite-or-spec-parents child)
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

(describe "The `buttercup-suites-total-specs-pending' function"
  :var (suites)
  (before-each
    (with-local-buttercup
      (describe "first suite"
        (it "active test" (expect 1 :to-equal 1))
        (xit "pending test"))
      (xdescribe "second suite"
        (it "forced pending" (expect 1 :to-equal 2)))
      (describe "third suite"
        (it "potentially skipped" (expect 1 :to-equal 1)))
      (setq suites buttercup-suites)))
  (it "should return the number of pending specs in a list of suites"
    (with-local-buttercup
      (expect (buttercup-suites-total-specs-pending suites)
              :to-equal 2)))
  (it "should also count skipped specs"
    (with-local-buttercup :suites suites
      (buttercup-mark-skipped "skipped" t)
      (expect (buttercup-suites-total-specs-pending suites)
              :to-equal 3))))

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
                  :not :to-be nil))))

  (it "should not overwrite pending status with `after-each' results"
    (with-local-buttercup
      (let ((suite (make-buttercup-suite))
            spec)
        (let ((buttercup--current-suite suite))
          (after-each (ignore))
          (setq spec (xit "pending")))
        (buttercup--run-suite suite)
        (expect (buttercup-spec-status spec) :to-be 'pending))))

  (describe "should set status to pending"
    (it "for assume in `before-each'"
      (with-local-buttercup
        (describe "suite"
          (before-each (assume nil "assume nil in before-each"))
          (it "spec" (expect 1 :to-equal 1))
          (after-each (ignore)))
        (buttercup-run)
        (expect (buttercup-suites-total-specs-pending buttercup-suites)
                :to-equal 1)))
    (it "for assume in spec"
      (with-local-buttercup
        (describe "suite"
          (before-each (ignore))
          (it "spec" (assume nil "assume nil in spec"))
          (after-each (ignore)))
        (buttercup-run)
        (expect (buttercup-suites-total-specs-pending buttercup-suites)
                :to-equal 1)))
    (it "for assume in `after-each'"
      (with-local-buttercup
        (describe "suite"
          (before-each (ignore))
          (it "spec" (expect 1 :to-equal 1))
          (after-each (assume nil "assume nil in after-each")))
        (buttercup-run)
        (expect (buttercup-suites-total-specs-pending buttercup-suites)
                :to-equal 1)))))

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
                   (buttercup--mark-stackframe)
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

  (it "should add a spec to the current suite and return the spec"
    (let ((buttercup--current-suite (make-buttercup-suite)))
      (let* ((created (buttercup-it "the test spec"
                        (lambda () 23)))
             (spec (car (buttercup-suite-children buttercup--current-suite))))
        (expect created :to-be spec)
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
    (let* ((buttercup--current-suite (make-buttercup-suite))
           (spec (buttercup-xit "bla bla")))
      (buttercup--update-with-funcall spec (buttercup-spec-function spec))
      (expect (buttercup-suite-or-spec-failure-description spec) :to-equal "PENDING")
      (expect (buttercup-suite-or-spec-status spec) :to-equal 'pending))))

;;;;;;;;;
;;; Spies

(describe "The Spy "
  (let (saved-test-function
        saved-test-command
        saved-test-function-throws-on-negative)
    ;; We use `before-all' here because some tests need to access the
    ;; same function as previous tests in order to work, so overriding
    ;; the function before each test would invalidate those tests.
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
          (fset 'test-function-throws-on-negative
                saved-test-function-throws-on-negative)
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

      (it "can spy on non-existing functions"
        (spy-on 'local-function)
        (local-function)
        (expect 'local-function :to-have-been-called))

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
         :to-throw 'error))

      (describe "will signal en error if"
        (it "used in before-all"
          (with-local-buttercup
            (let ((suite (describe "A bad spy scope"
                           (before-all
                             (spy-on 'some-function)))))
              (expect (buttercup--run-suite suite)
                      :to-throw))))
        (it "used directly in describe"
          (with-local-buttercup
            (expect (describe "Not in describe"
                      (spy-on 'foo)) :to-throw)))))

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
        (spy-on 'test-function :and-call-fake (lambda (_a _b) 1001)))

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
                :to-throw)
        (expect (spy-context-thrown-signal
                 (spy-calls-most-recent 'test-function-throws-on-negative))
                :to-equal '(error "x is less than zero"))))))

;;;;;;;;;;;;;
;;; Reporters
(buttercup-define-matcher-for-binary-function
    :to-equal-including-properties ert-equal-including-properties)

(describe "The batch reporters"
  :var (print-buffer)
  (let (parent-suite child-suite spec)
    (before-each
      (setq parent-suite (make-buttercup-suite :description "parent-suite")
            child-suite (make-buttercup-suite :description "child-suite")
            spec (make-buttercup-spec :description "spec")
            print-buffer (generate-new-buffer "*btrcp-reporter-test*"))
      (buttercup-suite-add-child parent-suite child-suite)
      (buttercup-suite-add-child child-suite spec)
      (spy-on 'send-string-to-terminal :and-call-fake
              (apply-partially #'send-string-to-ansi-buffer print-buffer))
      ;; Convenience function
      (spy-on 'buttercup-output :and-call-fake
              (lambda ()
                "Return the text of print-buffer."
                (with-current-buffer print-buffer
                  (buffer-string)))))
    (after-each
      (kill-buffer print-buffer)
      (setq print-buffer nil))

    (describe "on the buttercup-started event"
      :var (skipped
            ;; Local var for testing. The real variable is used by the
            ;; reporter attached to the buttercup instance running
            ;; these tests.
            buttercup-reporter-batch--start-time)
      (before-each
        (setq skipped (make-buttercup-spec :description "skipped" :status 'pending)))

      (it "should print the number of specs"
        (with-local-buttercup :color nil
          (buttercup-reporter-batch 'buttercup-started (list parent-suite)))
        (expect (buttercup-output) :to-equal-including-properties "Running 1 specs.\n\n"))

      (it "should color-print the number of specs with the default color"
        (with-local-buttercup :color t
          (buttercup-reporter-batch 'buttercup-started (list parent-suite)))
        (expect (buttercup-output) :to-equal-including-properties "Running 1 specs.\n\n"))

      (it "should print the number of skipped specs"
        (with-local-buttercup :color nil
          (buttercup-suite-add-child child-suite skipped)
          (buttercup-reporter-batch 'buttercup-started (list parent-suite)))
        (expect (buttercup-output) :to-equal-including-properties "Running 1 out of 2 specs.\n\n"))

      (it "should color-print the number of skipped specs with the default color"
        (with-local-buttercup :color t
          (buttercup-suite-add-child child-suite skipped)
          (buttercup-reporter-batch 'buttercup-started (list parent-suite)))
        (expect (buttercup-output) :to-equal-including-properties "Running 1 out of 2 specs.\n\n")))

    (describe "on the suite-started event"
      (it "should emit an indented suite description"
        (with-local-buttercup :color nil
         (buttercup-reporter-batch 'suite-started child-suite))
        (expect (buttercup-output) :to-equal-including-properties "  child-suite\n"))

      (it "should color-print an indented suite description with the default color"
        (with-local-buttercup :color t
         (buttercup-reporter-batch 'suite-started child-suite))
        (expect (buttercup-output) :to-equal-including-properties "  child-suite\n")))

    (describe "on the spec-started event"
      (it "should emit an indented spec description"
        (with-local-buttercup :color nil
         (buttercup-reporter-batch 'spec-started spec))
        (expect (buttercup-output) :to-equal-including-properties "    spec"))

      (it "should color-print an indented spec description with the default color"
        (with-local-buttercup :color t
         (buttercup-reporter-batch 'spec-started spec))
        (expect (buttercup-output) :to-equal-including-properties "    spec")))

    (describe "on the spec-done event"
      (describe "for a passed spec"
        (before-each
          (buttercup--set-start-time spec)
          (setf (buttercup-spec-failure-description spec) "DONTSHOW")
          (buttercup--set-end-time spec))

        (it "should print no status tag"
          (with-local-buttercup :color nil
           (buttercup-reporter-batch 'spec-started spec)
           (buttercup-reporter-batch 'spec-done spec))
          (expect (buttercup-output) :to-equal-including-properties
                  (format "    spec (%s)\n"
                          (buttercup-elapsed-time-string spec))))

        (it "should color-print the description in green and no status tag"
          (with-local-buttercup :color t
           (buttercup-reporter-batch 'spec-started spec)
           (buttercup-reporter-batch 'spec-done spec))
          (expect (buttercup-output) :to-equal-including-properties
                  (ansi-color-apply
                   (format "\e[32m    spec\e[0m (%s)\n"
                           (buttercup-elapsed-time-string spec)))))

        (it "should print multiline specs cleanly"
          (setf (buttercup-spec-description spec) "one\ntwo\vthree")
          (with-local-buttercup :color nil
           (buttercup-reporter-batch 'spec-started spec)
           (buttercup-reporter-batch 'spec-done spec))
          (expect (buttercup-output) :to-equal-including-properties
                  (format "    one\ntwo\n   three (%s)\n"
                          (buttercup-elapsed-time-string spec))))

        (it "should color-print multiline specs cleanly"
          (setf (buttercup-spec-description spec) "one\ntwo\vthree")
          (with-local-buttercup :color t
           (buttercup-reporter-batch 'spec-started spec)
           (buttercup-reporter-batch 'spec-done spec))
          (expect (buttercup-output) :to-equal-including-properties
                  (ansi-color-apply
                   (format "\e[32m    one\ntwo\n   three\e[0m (%s)\n"
                           (buttercup-elapsed-time-string spec))))))

      (describe "for a failed spec"
        (before-each
          (buttercup--set-start-time spec)
          (setf (buttercup-spec-status spec) 'failed)
          (buttercup--set-end-time spec))

        (it "should say FAILED"
          (with-local-buttercup :color nil
            (buttercup-reporter-batch 'spec-started spec)
            (buttercup-reporter-batch 'spec-done spec))
          (expect (buttercup-output) :to-equal-including-properties
                  (format "    spec  FAILED (%s)\n"
                          (buttercup-elapsed-time-string spec))))

        (it "should color-print the description in red and say FAILED"
          (with-local-buttercup :color t
            (buttercup-reporter-batch 'spec-started spec)
            (buttercup-reporter-batch 'spec-done spec))
          (expect (buttercup-output) :to-equal-including-properties
                  (ansi-color-apply
                   (format "\e[31m    spec  FAILED\e[0m (%s)\n"
                           (buttercup-elapsed-time-string spec))))))

      (describe "for a pending spec"
        (before-each
          (buttercup--set-start-time spec)
          (setf (buttercup-spec-status spec) 'pending
                (buttercup-spec-failure-description spec) "DESCRIPTION")
          (buttercup--set-end-time spec))

        (it "should output the failure-description"
          (with-local-buttercup :color nil
            (buttercup-reporter-batch 'spec-started spec)
            (buttercup-reporter-batch 'spec-done spec))
          (expect (buttercup-output) :to-equal-including-properties
                  (format "    spec  DESCRIPTION (%s)\n"
                          (buttercup-elapsed-time-string spec))))

        (it "should color-print the description and failure-description in yellow"
          (with-local-buttercup :color t
            (buttercup-reporter-batch 'spec-started spec)
            (buttercup-reporter-batch 'spec-done spec))
          (expect (buttercup-output) :to-equal-including-properties
                  (ansi-color-apply
                   (format "\e[33m    spec  DESCRIPTION\e[0m (%s)\n"
                        (buttercup-elapsed-time-string spec))))))

      (describe "should throw an error for an unknown spec status"
        (before-each (setf (buttercup-spec-status spec) 'unknown))
        (it "for plain output"
          (with-local-buttercup :color nil
            (expect (buttercup-reporter-batch 'spec-done spec)
                    :to-throw)))
        (it "for colored output"
          (with-local-buttercup :color t
            (expect (buttercup-reporter-batch 'spec-done spec)
                    :to-throw)))))

    (describe "on the suite-done event"
      (it "should emit a newline at the end of a top-level suite"
        (with-local-buttercup :color nil
          (buttercup-reporter-batch 'suite-done parent-suite))
        (expect (buttercup-output) :to-equal-including-properties "\n"))

      (it "should color-print a newline at the end of a top-level suite"
        (with-local-buttercup :color t
         (buttercup-reporter-batch 'suite-done parent-suite))
        (expect (buttercup-output) :to-equal-including-properties "\n"))

      (it "should not emit anything at the end of other suites"
        (with-local-buttercup :color nil
          (buttercup-reporter-batch 'suite-done child-suite))
        (expect (buttercup-output) :to-equal-including-properties ""))

      (it "should not color-print anything at the end of other suites"
        (with-local-buttercup :color t
          (buttercup-reporter-batch 'suite-done child-suite))
        (expect (buttercup-output) :to-equal-including-properties "")))

    (describe "on the buttercup-done event"
      :var ((buttercup-reporter-batch--start-time (current-time))
            defined-specs pending-specs failed-specs)

      (before-each
        (setq defined-specs 10 pending-specs 0 failed-specs 0)
        (spy-on 'buttercup-suites-total-specs-defined :and-call-fake (lambda (&rest _) defined-specs))
        (spy-on 'buttercup-suites-total-specs-pending :and-call-fake (lambda (&rest _) pending-specs))
        (spy-on 'buttercup-suites-total-specs-failed :and-call-fake (lambda (&rest _) failed-specs)))

      (it "should print a summary of run and failing specs"
        (setq failed-specs 6)
        (with-local-buttercup :color nil
          (buttercup-reporter-batch 'buttercup-done nil))
        (expect (buttercup-output) :to-match
                "Ran 10 specs, 6 failed, in [0-9]+.[0-9]+[mu]?s.\n"))

      (it "should color-print `0 failed' specs in green"
        (with-local-buttercup :color t
          (buttercup-reporter-batch 'buttercup-done nil))
        (expect (buttercup-output) :to-match
                "Ran 10 specs, 0 failed, in [0-9]+.[0-9]+[mu]?s.\n")
        (expect (substring (buttercup-output) 0 (length "Ran 10 specs, 0 failed, in"))
                :to-equal-including-properties
                (ansi-color-apply "Ran 10 specs, \e[32m0 failed\e[0m, in")))

      (it "should color-print `X failed' specs in red"
        (setq failed-specs 6)
        (with-local-buttercup :color t
          (buttercup-reporter-batch 'buttercup-done nil))
        (expect (buttercup-output) :to-match
                "Ran 10 specs, 6 failed, in [0-9]+.[0-9]+[mu]?s.\n")
        (expect (substring (buttercup-output) 0 (length "Ran 10 specs, 6 failed, in"))
                :to-equal-including-properties
                (ansi-color-apply "Ran 10 specs, \e[31m6 failed\e[0m, in")))

      (it "should print a summary separating run and pending specs"
        (setq pending-specs 3)
        (with-local-buttercup :color nil
          (buttercup-reporter-batch 'buttercup-done nil))
        (expect (buttercup-output) :to-match
                "Ran 7 out of 10 specs, 0 failed, in [0-9]+.[0-9]+[mu]?s.\n"))

      (it "should color-print pending spec count in default color"
        (setq pending-specs 3)
        (with-local-buttercup :color t
          (buttercup-reporter-batch 'buttercup-done nil))
        (expect (buttercup-output) :to-match
                "Ran 7 out of 10 specs, 0 failed, in [0-9]+.[0-9]+[mu]?s.\n")
        (expect (substring (buttercup-output)
                           0 (length "Ran 7 out of 10 specs"))
                :to-equal-including-properties
                "Ran 7 out of 10 specs"))

      (it "should not raise any error even if a spec failed"
        (setf (buttercup-spec-status spec) 'failed)
        (with-local-buttercup :color nil
          (expect (buttercup-reporter-batch 'buttercup-done (list spec))
                  :not :to-throw)))
      )

    (describe "on an unknown event"
      (it "should raise an error"
        (expect (buttercup-reporter-batch 'unknown-event nil)
                :to-throw)))))

(describe "Backtraces"
  :var (print-buffer)
  ;; redirect output to a buffer
  (before-each
    (setq print-buffer (generate-new-buffer "*btrcp-reporter-test*"))
    (spy-on 'send-string-to-terminal :and-call-fake
            (apply-partially #'send-string-to-ansi-buffer print-buffer))
    ;; Convenience function
    (spy-on 'buttercup-output :and-call-fake
            (lambda ()
              "Return the text of print-buffer."
              (with-current-buffer print-buffer
                (buffer-string)))))
  (after-each
    (kill-buffer print-buffer)
    (setq print-buffer nil))
  ;; define a buttercup-reporter-batch variant that only outputs on
  ;; buttercup-done
  (before-each
    (spy-on 'backtrace-reporter :and-call-fake
            (lambda (event arg)
              (if (eq event 'buttercup-done)
                  (buttercup-reporter-batch event arg)
                (cl-letf (((symbol-function 'buttercup--print) #'ignore))
                  (buttercup-reporter-batch event arg))))))
  ;; suppress the summary line
  (before-each
    (spy-on 'buttercup-reporter-batch--print-summary))
  ;; define a known backtrace with a typical error
  (before-all
	(defun bc-bt-foo (a) (bc-bt-bar a))
	(defun bc-bt-bar (a) (bc-bt-baz a))
	(defun bc-bt-baz (a)
      (or (number-or-marker-p a)
        (signal 'wrong-type-argument `(number-or-marker-p ,a)))))
  (after-all
	(fmakunbound 'bc-bt-foo)
	(fmakunbound 'bc-bt-bar)
	(fmakunbound 'bc-bt-baz))
  (it "should be printed for each failed spec"
    (with-local-buttercup
      :reporter #'backtrace-reporter
      (describe "suite"
        (it "expect 2" (expect (+ 1 2) :to-equal 2))
        (it "expect nil" (expect nil)))
      (buttercup-run :noerror))
    (expect (buttercup-output) :to-match
            (rx string-start
                (= 2 (seq (= 40 ?=) "\n"
                          "suite expect " (or "2" "nil") "\n"
                          "\n"
                          "Traceback (most recent call last):\n"
                          (* (seq "  " (+ not-newline) "\n"))
                          (or "FAILED" "error") ": " (+ not-newline) "\n\n"))
                string-end)))
  (describe "with style"
    :var (test-suites long-string)
    ;; Set up tests to test
    (before-each
      (setq long-string
            ;; It's important that this string doesn't contain any
            ;; regex special characters, it's used in a `rx' `eval'
            ;; form that will escape them. Later Emacsen have
            ;; `literal' that is much easier to use.
            "a string that will be truncated in backtrace crop, at least 70 chars long")
      (with-local-buttercup
       (describe "suite"
         (it "bc-bt-backtrace"
           (expect
            (bc-bt-foo long-string)
            :to-be-truthy)))
       (setq test-suites buttercup-suites)))
    (it "`crop' should print truncated lines"
      (with-local-buttercup
       :suites test-suites :reporter #'backtrace-reporter
       :frame-style 'crop
       (buttercup-run :noerror)
       (setq long-string (truncate-string-to-width long-string 63))
       (expect (buttercup-output) :to-match
               (rx-to-string
                `(seq
                  string-start
                  (= 40 ?=) "\n"
                  "suite bc-bt-backtrace\n"
                  "\n"
                  "Traceback (most recent call last):\n"
                  "  bc-bt-foo(\"" (eval ,long-string) "...\n"
                  "  bc-bt-bar(\"" (eval ,long-string) "...\n"
                  "  bc-bt-baz(\"" (eval ,long-string) "...\n"
                  (* (seq "  " (or (seq (= 74 not-newline) (= 3 ?.))
                                   (seq (** 0 74 not-newline) (= 3 (not (any ?.))))) "\n"))
                  "error: (" (* anything) ")\n\n"
                  string-end)))))
    (it "`full' should print full lines"
      (with-local-buttercup
       :suites test-suites :reporter #'backtrace-reporter
       :frame-style 'full
       (buttercup-run :noerror)
       (expect (buttercup-output) :to-match
               (rx-to-string
                `(seq
                  string-start
                  (= 40 ?=) "\n"
                  "suite bc-bt-backtrace\n"
                  "\n"
                  "Traceback (most recent call last):\n"
                  "  bc-bt-foo(\"" (eval ,long-string) "\")\n"
                  "  bc-bt-bar(\"" (eval ,long-string) "\")\n"
                  "  bc-bt-baz(\"" (eval ,long-string) "\")\n"
                  (* (seq "  " (* not-newline) (= 3 (not (any ?.))) "\n"))
                  "error: (" (* anything) ")\n\n"
                  string-end)))))
    (it "`pretty' should pretty-print frames"
      (with-local-buttercup
       :suites test-suites :reporter #'backtrace-reporter
       :frame-style 'pretty
       (buttercup-run :noerror)
       (expect (buttercup-output) :to-match
               (rx-to-string
                `(seq
                  string-start
                  (= 40 ?=) "\n"
                  "suite bc-bt-backtrace\n"
                  "\n"
                  "Traceback (most recent call last):\n"
                  "λ (bc-bt-foo \"" (regex ,long-string) "\")\n"
                  "λ (bc-bt-bar \"" (regex ,long-string) "\")\n"
                  "λ (bc-bt-baz \"" (regex ,long-string) "\")\n"
                  (* (seq (or ?M ?λ) " (" (* not-newline) ; frame start
                          (*? (seq "\n   " (* not-newline))) ; any number of pp lines
                          (* not-newline) ")\n")) ;; frame end
                  "error: (" (* anything) ")\n\n"
                  string-end)))))
    (it "`omit' should print nothing"
      (with-local-buttercup
        :suites test-suites :reporter #'backtrace-reporter
        :frame-style 'omit
        (buttercup-run :noerror)
        (expect (buttercup-output) :to-equal ""))))
  (it "should signal an error for unknown styles"
    (let ((buttercup-stack-frame-style 'not-a-valid-style))
      (expect (buttercup--format-stack-frame '(t myfun 1 2))
              :to-throw 'error '("Unknown stack trace style: not-a-valid-style"))))
  (describe "should generate correct backtrace for"
    (cl-macrolet
        ((matcher-spec
          (description &rest matcher)
          `(it ,description
             (with-local-buttercup
              :reporter #'backtrace-reporter
              (describe "backtrace for"
                (it "matcher"
                  (expect (bc-bt-baz "text") ,@matcher)))
              (buttercup-run :noerror)
              (expect (buttercup-output) :to-equal
                      ,(mapconcat
                        #'identity
                        `(,(make-string 40 ?=)
                          "backtrace for matcher"
                          ""
                          "Traceback (most recent call last):"
                          "  bc-bt-baz(\"text\")"
                          ,(concat
                            "  (or (number-or-marker-p a) (signal "
                            (if (< emacs-major-version 27)
                                "(quote wrong-type-argument) (list (quot..."
                               "'wrong-type-argument (list 'number-or-m..."))
                          "  signal(wrong-type-argument (number-or-marker-p \"text\"))"
                          "error: (wrong-type-argument number-or-marker-p \"text\")"
                          "" "") "\n"))))))
      (matcher-spec "no matcher")
      (matcher-spec ":to-be-truthy" :to-be-truthy)
      (matcher-spec ":not :to-be-truthy" :not :to-be-truthy)
      (matcher-spec ":to-be" :to-be 3)
      (matcher-spec ":not :to-be" :not :to-be 3)
      (matcher-spec ":to-equal" :to-equal 3)
      (matcher-spec ":not :to-equal" :not :to-equal 3)
      (matcher-spec ":to-have-same-items-as" :to-have-same-items-as '(3))
      (matcher-spec ":not :to-have-same-items-as" :not :to-have-same-items-as '(3))
      (matcher-spec ":to-match" :to-match ".")
      (matcher-spec ":not :to-match" :not :to-match ".")
      (matcher-spec ":to-be-in" :to-be-in '(2))
      (matcher-spec ":not :to-be-in" :not :to-be-in '(2))
      (matcher-spec ":to-contain" :to-contain 2)
      (matcher-spec ":not :to-contain" :not :to-contain 2)
      (matcher-spec ":to-be-less-than" :to-be-less-than 2)
      (matcher-spec ":not :to-be-less-than" :not :to-be-less-than 2)
      (matcher-spec ":to-be-greater-than" :to-be-greater-than 2)
      (matcher-spec ":not :to-be-greater-than" :not :to-be-greater-than 2)
      (matcher-spec ":to-be-weakly-less-than" :to-be-weakly-less-than 2)
      (matcher-spec ":not :to-be-weakly-less-than" :not :to-be-weakly-less-than 2)
      (matcher-spec ":to-be-weakly-greater-than" :to-be-weakly-greater-than 2)
      (matcher-spec ":not :to-be-weakly-greater-than" :not :to-be-weakly-greater-than 2)
      (matcher-spec ":to-be-close-to" :to-be-close-to 2 0.3)
      (matcher-spec ":not :to-be-close-to" :not :to-be-close-to 2 0.2)
      ;; (matcher-spec ":to-throw" :to-throw)
      ;; (matcher-spec ":not :to-throw" :not :to-throw)
      (matcher-spec ":to-have-been-called" :to-have-been-called)
      (matcher-spec ":not :to-have-been-called" :not :to-have-been-called)
      (matcher-spec ":to-have-been-called-with" :to-have-been-called-with 2)
      (matcher-spec ":not :to-have-been-called-with" :not :to-have-been-called-with 2)
      (matcher-spec ":to-have-been-called-times" :to-have-been-called-times 2)
      (matcher-spec ":not :to-have-been-called-times" :not :to-have-been-called-times 2)))
  (it "should not generate backtraces for skipped specs"
    (let (test-spec)
      (spy-on 'buttercup--backtrace :and-call-through)
      (with-local-buttercup
        (describe "one description"
          (it "with a pending spec")
          (buttercup-skip "skip"))
        (buttercup-run :noerror)
        (setq test-spec (car (buttercup-suite-children (car buttercup-suites)))))
      (expect 'buttercup--backtrace :not :to-have-been-called)
      (expect (buttercup-spec-failure-stack test-spec) :to-be nil))))


(describe "When using quiet specs in the batch reporter"
  :var (print-buffer)
  (before-each
    (setq print-buffer (generate-new-buffer "*btrcp-reporter-test*"))
    (spy-on 'send-string-to-terminal :and-call-fake
            (apply-partially #'send-string-to-ansi-buffer print-buffer))
    ;; Convenience function
    (spy-on 'buttercup-output :and-call-fake
            (lambda ()
              "Return the text of `print-buffer'."
              (with-current-buffer print-buffer
                (buffer-string)))))
  (after-each
    (kill-buffer print-buffer)
    (setq print-buffer nil))

  (it "should print nothing if all specs are quiet"
    (with-local-buttercup :color nil :quiet '(pending) :reporter #'buttercup-reporter-batch
      (describe "top"
        (it "spec 1")
        (describe "second"
          (it "spec 2")
          (it "spec 3")))
      (describe "empty")
      (buttercup-run))
    (expect (buttercup-output) :to-match
            "^Running 0 out of 3 specs\\.\n\nRan 0 out of 3 specs, 0 failed, in [0-9.]+ms\\.$"))

  (it "should print the containing suites for non-quiet specs"
    (with-local-buttercup :color nil :quiet '(pending) :reporter #'buttercup-reporter-batch
      (describe "top"
        (it "spec 1" (ignore))
        (describe "second"
          (it "spec 2")
          (it "spec 3" (ignore))
          (describe "third"
            (it "spec 4"))))
      (describe "empty")
      (buttercup-run))
    (expect (buttercup-output) :to-match
            (concat "^Running 2 out of 4 specs\\.\n\n"
                    "top\n"
                    "  spec 1 ([0-9.]+ms)\n"
                    "  second\n"
                    "    spec 3 ([0-9.]+ms)\n\n"
                    "Ran 2 out of 4 specs, 0 failed, in [0-9.]+ms\\.$")))

  (it "should quiet all of the given spec statuses"
    ;; suppress stacktraces printed at buttercup-done
    (spy-on 'buttercup-reporter-batch--print-failed-spec-report)
    (with-local-buttercup
      :color nil :quiet '(pending passed failed) :reporter #'buttercup-reporter-batch
      (describe "passed"
        (it "passed" (ignore)))
      (describe "failed"
        (it "failed" (buttercup-fail "because")))
      (describe "pending"
        (it "pending"))
      (buttercup-run t))
    (expect (buttercup-output) :to-match
            "^Running 2 out of 3 specs\\.\n\nRan 2 out of 3 specs, 1 failed, in [0-9.]+ms\\.$"))

  (it "should handle `skipped' virtual status in quiet list"
    ;; suppress stacktraces printed at buttercup-done
    (spy-on 'buttercup-reporter-batch--print-failed-spec-report)
    (with-local-buttercup
      :color nil :quiet '(skipped) :reporter #'buttercup-reporter-batch
      (describe "passed"
        (it "passed" (ignore)))
      (describe "failed"
        (it "failed" (buttercup-fail "because")))
      (describe "pending"
        (it "pending"))
      (describe "skipped"
        (it "skipped" (ignore)))
      (buttercup-mark-skipped "skipped")
      (buttercup-run t))
    (expect (buttercup-output) :to-match
            (concat "^Running 2 out of 4 specs\\.\n\n"
                    "passed\n  passed ([0-9.]+ms)\n\n"
                    "failed\n  failed  because ([0-9.]+ms)\n\n"
                    "pending\n  pending  PENDING ([0-9.]+ms)\n\n"
                    "Ran 2 out of 4 specs, 1 failed, in [0-9.]+ms\\.\n$")))

    (it "should handle `disabled' virtual status in quiet list"
      ;; suppress stacktraces printed at buttercup-done
      (spy-on 'buttercup-reporter-batch--print-failed-spec-report)
      (with-local-buttercup
        :color nil :quiet '(disabled) :reporter #'buttercup-reporter-batch
        (describe "passed"
          (it "passed" (ignore)))
        (describe "failed"
          (it "failed" (buttercup-fail "because")))
        (describe "pending"
          (it "pending"))
        (describe "skipped"
          (it "skipped" (ignore)))
        (buttercup-mark-skipped "skipped")
        (buttercup-run t))
      (expect (buttercup-output) :to-match
              (concat "^Running 2 out of 4 specs\\.\n\n"
                      "passed\n  passed ([0-9.]+ms)\n\n"
                      "failed\n  failed  because ([0-9.]+ms)\n\n"
                      "skipped\n  skipped  SKIPPED ([0-9.]+ms)\n\n"
                      "Ran 2 out of 4 specs, 1 failed, in [0-9.]+ms\\.\n$"))))

;;;;;;;;;;;;;;;;;;;;;
;;; buttercup-run

(describe "The `buttercup-run' function"
  :var (parent-suite child-suite spec)
  (before-each
    (setq parent-suite (make-buttercup-suite :description "parent-suite")
          child-suite (make-buttercup-suite :description "child-suite")
          spec (make-buttercup-spec :description "spec"))
    (buttercup-suite-add-child parent-suite child-suite)
    (buttercup-suite-add-child child-suite spec)
    (spy-on 'reporter)
    (spy-on 'buttercup--run-suite))
  (it "should signal an error if no suites are defined"
    (with-local-buttercup
     (expect (buttercup-run) :to-throw 'error '("No suites defined"))))
  (it "should return :no-suites for no suites and noerror"
    (with-local-buttercup
     (expect (buttercup-run t) :to-equal :no-suites)))
  (it "should raise an error if at least one spec failed"
    (setf (buttercup-spec-status spec) 'failed)
    (with-local-buttercup :suites (list parent-suite)
      (expect (buttercup-run) :to-throw 'buttercup-run-specs-failed '(""))))
  (it "should return nil for failing specs and noerror"
    (setf (buttercup-spec-status spec) 'failed)
    (with-local-buttercup :suites (list parent-suite)
      (expect (buttercup-run t) :not :to-be-truthy)))
  (it "should return t for passing specs"
    (with-local-buttercup :suites (list parent-suite)
      (expect (buttercup-run) :to-be-truthy)
      (expect (buttercup-run t) :to-be-truthy)))
  (it "should call the reporter twice with events buttercup-started and -done"
    (with-local-buttercup :suites (list parent-suite) :reporter 'reporter
      (expect (buttercup-run) :not :to-throw)
      (expect 'reporter :to-have-been-called-times 2)
      (expect 'reporter :to-have-been-called-with 'buttercup-started buttercup-suites)
      (expect 'reporter :to-have-been-called-with 'buttercup-done buttercup-suites)))
  (it "should call `buttercup--run-suite' once per suite"
    (with-local-buttercup :reporter 'reporter :suites (make-list 5 parent-suite)
      (expect (buttercup-run) :not :to-throw)
      (expect 'buttercup--run-suite :to-have-been-called-times 5))))

(describe "The `buttercup--print' function"
  (before-each
    (spy-on 'send-string-to-terminal))

  (it "should send a formatted string to the terminal"
    (buttercup--print "Hello, %s" "world")

    (expect 'send-string-to-terminal
            :to-have-been-called-with
            "Hello, world")))

(describe "The `buttercup-mark-skipped' function"
  :var (suites)
  (before-each
    (with-local-buttercup
      (describe "first suite"
        (describe "inner suite"
          (it "1-1-1 spec" (ignore))
          (it "1-1-2 spec" (ignore))
          (it "1-1-3 spec" (ignore))
          (it "1-1-4 spec" (ignore))
          (it "1-1-5 spec" (ignore))
          (xit "1-1-6 spec" (ignore)))
        (it "1-1 spec" (ignore)))
      (xdescribe "second suite"
        (it "2-1 spec" (ignore))
        (it "2-2 spec" (ignore))
        (it "2-3 spec" (ignore))
        (it "2-4 spec" (ignore)))
      (setq suites buttercup-suites)))
  (it "should do nothing with a reversed match-all pattern"
    (expect (buttercup-suites-total-specs-defined suites) :to-equal 11)
    (expect (buttercup-suites-total-specs-pending suites) :to-equal 5)
    (with-local-buttercup :suites suites
      (buttercup-mark-skipped "." t))
    (expect (buttercup-suites-total-specs-defined suites) :to-equal 11)
    (expect (buttercup-suites-total-specs-pending suites) :to-equal 5)
    (with-local-buttercup :suites suites
      (buttercup-run))
    (expect (buttercup-suites-total-specs-pending suites) :to-equal 5)
    (expect (cl-count "SKIPPED" (buttercup--specs suites)
                      :key #'buttercup-spec-failure-description)
            :to-equal 0))
  (it "should mark all specs as pending with a reversed match none pattern"
    (with-local-buttercup :suites suites
      (buttercup-mark-skipped "[z-a]" t))
    (expect (buttercup-suites-total-specs-defined suites) :to-equal 11)
    (expect (buttercup-suites-total-specs-pending suites) :to-equal 11))
  (it "should handle multiple patterns"
    (with-local-buttercup :suites suites
      (buttercup-mark-skipped '("1-1-1" "1-1-2" "1-4" "2-4") t))
    (expect (buttercup-suites-total-specs-defined suites) :to-equal 11)
    (expect (buttercup-suites-total-specs-pending suites) :to-equal 8))
  (it "should support predicates"
    (with-local-buttercup :suites suites
      (buttercup-mark-skipped
       (lambda (spec) (= 2 (cl-count ?- (buttercup-spec-full-name spec))))))
    (expect (buttercup-suites-total-specs-defined suites) :to-equal 11)
    (expect (buttercup-suites-total-specs-pending suites) :to-equal 10))
  (it "should support reversed predicates"
    (with-local-buttercup :suites suites
      (buttercup-mark-skipped
       (lambda (spec) (= 2 (cl-count ?- (buttercup-spec-full-name spec))))
       t))
    (expect (buttercup-suites-total-specs-defined suites) :to-equal 11)
    (expect (buttercup-suites-total-specs-pending suites) :to-equal 6))
  (it "should signal an error for invalid matchers"
    (with-local-buttercup
      (expect (buttercup-mark-skipped 4) :to-throw))
    (with-local-buttercup
      (expect (buttercup-mark-skipped (list "re" "re" 5 "re")) :to-throw)))
  )

;;;;;;;;;;;;;;;;;;;;;
;;; ERT Compatibility

(describe "Buttercup's ERT compatibility wrapper"
  (it "should convert `ert-test-failed' into `buttercup-failed'"
    (expect
     (buttercup-with-converted-ert-signals
       (should (equal 1 2)))
     :to-throw 'buttercup-failed))
  (it "should convert `ert-test-skipped' into `buttercup-pending'"
    (assume (functionp 'ert-skip) "Loaded ERT version does not provide `ert-skip'")
    (expect
     (buttercup-with-converted-ert-signals
       (ert-skip "Skipped this test"))
     :to-throw 'buttercup-pending)))

;;;;;;;;;;;;;;;;;;
;;; test discovery

(describe "`buttercup-run-discover' should"
  (describe "parse command line arguments"
    (before-each
      (spy-on 'buttercup-run)
      (spy-on 'buttercup-mark-skipped)
      (spy-on 'directory-files-recursively)
      (spy-on 'buttercup-error-on-stale-elc))
    (it "ignoring `--'"
      (let ((command-line-args-left '("--")))
        (buttercup-run-discover)
        (expect command-line-args-left :to-equal nil)))
    (it "requiring an extra argument for `--traceback'"
      (let ((command-line-args-left '("--traceback")))
        (expect (buttercup-run-discover) :to-throw 'error '("Option requires argument: --traceback"))))
    (it "checking `--traceback' argument for validity"
      (let ((command-line-args-left '("--traceback" "unknown")))
        (with-local-buttercup
          (expect (buttercup-run-discover) :to-throw 'error '("Unknown stack trace style: unknown")))))
    (it "setting `buttercup-stack-frame-style' from `--traceback' arg"
      (let ((command-line-args-left '("--traceback" "full")))
        (with-local-buttercup
          (buttercup-run-discover)
          (expect buttercup-stack-frame-style :to-equal 'full))
        (expect command-line-args-left :to-equal nil)))
    (it "requiring an extra argument for `--pattern' or `-p'"
      (let ((command-line-args-left '("--pattern")))
        (expect (buttercup-run-discover) :to-throw 'error '("Option requires argument: --pattern"))
        (setq command-line-args-left '("-p"))
        (expect (buttercup-run-discover) :to-throw 'error '("Option requires argument: -p"))))
    (it "collecting `--pattern' and `-p' args and send to `buttercup-mark-skipped'"
      (let ((command-line-args-left '("--pattern" "foo" "-p" "bar" "--pattern" "baz"))
            buttercup-mark-skipped-args)
        (buttercup-run-discover)
        (expect command-line-args-left :to-equal nil)
        (expect 'buttercup-mark-skipped :to-have-been-called-times 1)
        (setq buttercup-mark-skipped-args (car (spy-calls-args-for 'buttercup-mark-skipped 0)))
        (expect buttercup-mark-skipped-args :to-have-same-items-as '("foo" "bar" "baz"))))
    (it "clearing `buttercup-color' if `--no-color' is given"
      (let ((command-line-args-left '("--no-color"))
            (buttercup-color t))
        (buttercup-run-discover)
        (expect buttercup-color :to-equal nil)
        (expect command-line-args-left :to-equal nil)
        (setq command-line-args-left '("-c")
              buttercup-color t)
        (buttercup-run-discover)
        (expect buttercup-color :to-equal nil)
        (expect command-line-args-left :to-equal nil)))
    (it "adding `skipped' and `disabled' to quiet statuses if `--no-skip' is given"
      (let ((command-line-args-left '("--no-skip")))
        (with-local-buttercup
          (buttercup-run-discover)
          (expect buttercup-reporter-batch-quiet-statuses :to-contain 'skipped)
          (expect buttercup-reporter-batch-quiet-statuses :to-contain 'disabled))
        (expect command-line-args-left :to-equal nil)))
    (it "adding `pending' and `passed' to quiet statuses if `--only-error' is given"
      (let ((command-line-args-left '("--only-error")))
        (with-local-buttercup
          (buttercup-run-discover)
          (expect buttercup-reporter-batch-quiet-statuses :to-contain 'pending)
          (expect buttercup-reporter-batch-quiet-statuses :to-contain 'passed))
        (expect command-line-args-left :to-equal nil)))
    (it "calling `buttercup-error-on-stale-elc' if `--stale-file-error' is given"
      (let ((command-line-args-left '("--stale-file-error")))
        (with-local-buttercup
          (buttercup-run-discover)
          (expect 'buttercup-error-on-stale-elc :to-have-been-called-times 1)
          (expect command-line-args-left :to-equal nil))))
    (it "search any unknown args for test files"
      (let ((command-line-args-left '("foo" "--traceback" "full" "bar" "--strange" "baz")))
        (with-local-buttercup
          (buttercup-run-discover)
          (expect 'directory-files-recursively :to-have-been-called-times 4)
          (expect 'directory-files-recursively :to-have-been-called-with "foo" "\\`test-.*\\.el\\'\\|-tests?\\.el\\'")
          (expect 'directory-files-recursively :to-have-been-called-with "bar" "\\`test-.*\\.el\\'\\|-tests?\\.el\\'")
          (expect 'directory-files-recursively :to-have-been-called-with "--strange" "\\`test-.*\\.el\\'\\|-tests?\\.el\\'")
          (expect 'directory-files-recursively :to-have-been-called-with "baz" "\\`test-.*\\.el\\'\\|-tests?\\.el\\'"))
        (expect command-line-args-left :to-equal nil)))
    )
  (describe "find and load files"
    (before-each
      (spy-on 'buttercup-run)
      (spy-on 'buttercup-mark-skipped)
      (spy-on 'load)
      (spy-on 'relative-load-path :and-call-fake
              (lambda (args)
                "Return `car' of args relative to `default-directory'."
                (replace-regexp-in-string
                 (format "^\\(\\./\\|%s\\)" (regexp-quote default-directory))
                 ""
                 (car args))))
      )
    (it "named test-*.el and *-tests?.el but no other files"
      (buttercup--test-with-tempdir
        '("test.el" "tests.el" "test-actually.el"
          "foo/test-foo.el" "foo/bar/bar-test.el"
          "baz/no-test-here.el" "baz/baz-tests.el")
        (buttercup-run-discover)
        (expect 'load :to-have-been-called-times 4)
        (let ((loaded-files (mapcar #'relative-load-path
                                    (spy-calls-all-args 'load))))
          (expect loaded-files :to-have-same-items-as '("test-actually.el"
                                                        "foo/test-foo.el"
                                                        "foo/bar/bar-test.el"
                                                        "baz/baz-tests.el")))))
    (it "only in given directories"
      (buttercup--test-with-tempdir
        '("root-tests.el"
          "a/a-tests.el" "a/b/ab-tests.el"
          "b/b-tests-el" "b/a/ba-tests.el")
        (let ((command-line-args-left '("a")))
          (buttercup-run-discover))
        (expect 'load :to-have-been-called-times 2)
        (let ((loaded-files (mapcar #'relative-load-path (spy-calls-all-args 'load))))
          (expect loaded-files :to-have-same-items-as '("a/a-tests.el"
                                                        "a/b/ab-tests.el")))))))

;; The nested debuggers of running buttercup specs inside other
;; buttercup specs does not do the right thing. Write out-of-framework
;; tests for now.

(buttercup--test-with-tempdir
  '("ok-test.el"
    ("test-a.el" "(describe \"foo\"")
    ("test-b.el" "(describe \"bar\" (it \"baz\" (ignore)))"))
  (let ((load-path (cons default-directory load-path))
        buttercup-status-error-caught)
    (with-local-buttercup
      (condition-case condition
          (buttercup-run-discover)
        (buttercup-run-specs-failed (setq buttercup-status-error-caught t)))
      (unless buttercup-status-error-caught
        (error "Expected buttercup-run-discover to signal a buttercup-run-specs-failed error"))
      (unless (equal 2 (length buttercup-suites))
        (error "Expected suites from test-b.el to be in buttercup-suites"))
      )))

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

;;;;;;;;;;;;;;;;;;;
;;; Stale elc files

(describe "For stale `elc' file checks"
  (describe "`buttercup-check-for-stale-elc'"
    :var (el-time elc-time)
    (before-each
      (spy-on 'file-attributes :and-call-fake
              (lambda (filename &optional _id-format)
                (make-list
                 10
                 (make-list 4
                            (pcase (file-name-extension filename)
                              ("elc" elc-time)
                              ("el" el-time)))))))
    (it "should do nothing for `el' files"
      (setq el-time 2  ;; elc is older than el
            elc-time 1)
      (expect (buttercup-check-for-stale-elc "buttercup.el") :not :to-throw))
    (it "should signal error when `elc' is older than `el'"
      (setq el-time 2  ;; elc is older than el
            elc-time 1)
      (expect (buttercup-check-for-stale-elc "buttercup.elc") :to-throw))
    (it "should not signal error when `elc' is newer than `el'"
      (setq el-time 2  ;; elc is older than el
            elc-time 3)
      (expect (buttercup-check-for-stale-elc "buttercup.elc") :not :to-throw))
    (it "should do nothing if the `el' file does not exist"
      (setq el-time 3  ;; el is older than elc
            elc-time 2)
      (spy-on 'file-exists-p)
      (expect (buttercup-check-for-stale-elc "buttercup.elc") :not :to-throw)))

  (describe "`buttercup-error-on-stale-elc'"
    (it "should activate with no argument"
      (let (after-load-functions)
        (buttercup-error-on-stale-elc)
        (expect after-load-functions :to-contain 'buttercup-check-for-stale-elc)))
    (it "should deactivate with almost any argument"
      (let ((after-load-functions '(buttercup-check-for-stale-elc)))
        (buttercup-error-on-stale-elc 2)
        (expect after-load-functions :not :to-contain 'buttercup-check-for-stale-elc)))
    (it "should toggle when given `toggle' as argument"
      (let (after-load-functions)
        (buttercup-error-on-stale-elc 'toggle)
        (expect after-load-functions :to-contain 'buttercup-check-for-stale-elc)
        (buttercup-error-on-stale-elc 'toggle)
        (expect after-load-functions :not :to-contain 'buttercup-check-for-stale-elc)))))

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:
(provide 'test-buttercup)
;;; test-buttercup.el ends here
