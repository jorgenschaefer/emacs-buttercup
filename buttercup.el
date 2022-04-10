;;; buttercup.el --- Behavior-Driven Emacs Lisp Testing -*-lexical-binding:t-*-

;; Copyright (C) 2015-2017  Jorgen Schaefer <contact@jorgenschaefer.de>

;; Version: 1.25
;; Author: Jorgen Schaefer <contact@jorgenschaefer.de>
;; Maintainer: Ola Nilsson <ola.nilsson@gmail.com>
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/jorgenschaefer/emacs-buttercup

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

;; Buttercup is a behavior-driven development framework for testing
;; Emacs Lisp code. It is heavily inspired by the Jasmine test
;; framework for JavaScript.

;; A test suite begins with a call to the Buttercup macro `describe` with
;; the first parameter describing the suite and the rest being the body
;; of code that implements the suite.

;; (describe "A suite"
;;   (it "contains a spec with an expectation"
;;     (expect t :to-be t)))

;; The ideas for project were shamelessly taken from Jasmine
;; <https://jasmine.github.io>.

;; All the good ideas are theirs. All the problems are mine.

;;; Code:

(require 'cl-lib)
(require 'buttercup-compat)
(require 'format-spec)
(require 'ert nil t)
(require 'warnings)

;; A base error for all errors raised by buttercup.
(define-error 'buttercup-error-base "error")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wrapper function manipulation

(defun buttercup--enclosed-expr (fun)
  "Given a zero-arg function FUN, return its unevaluated expression.

The function MUST have one of the following forms:

\(lambda () EXPR)
\(lambda () (buttercup--mark-stackframe) EXPR)
\(closure (ENVLIST) () EXPR)
\(closure (ENVLIST) () (buttercup--mark-stackframe) EXPR)
\(lambda () (quote EXPR) EXPR)
\(closure (ENVLIST) () (quote EXPR) EXPR)

and the return value will be EXPR, unevaluated. The latter 2
forms are useful if EXPR is a macro call, in which case the
`quote' ensures access to the un-expanded form."
  (pcase fun
    (`(closure ,(pred listp) nil ,expr) expr)
    (`(closure ,(pred listp) nil (buttercup--mark-stackframe) ,expr) expr)
    (`(closure ,(pred listp) nil (quote ,expr) . ,_rest) expr)
    (`(closure ,(pred listp) nil ,_expr . ,(pred identity))
     (error "Closure contains multiple expressions: %S" fun))
    (`(closure ,(pred listp) ,(pred identity) . ,(pred identity))
     (error "Closure has nonempty arglist: %S" fun))
    (`(lambda nil ,expr) expr)
    (`(lambda nil (buttercup--mark-stackframe) ,expr) expr)
    (`(lambda nil (quote ,expr) . ,_rest) expr)
    (`(lambda nil ,_expr . ,(pred identity))
     (error "Function contains multiple expressions: %S" fun))
    (`(lambda ,(pred identity) . ,(pred identity))
     (error "Function has nonempty arglist: %S" fun))
    (_ (error "Not a zero-arg one-expression closure: %S" fun))))

(defun buttercup--expr-and-value (fun)
  "Given a function, return its quoted expression and value.

FUN must be a zero-argument one-expression function, i.e.
something that satisfies `buttercup--wrapper-fun-p'. The return
value is `(cons EXPR VALUE)', where EXPR is the unevaluated
expression in the function, and VALUE is the result of calling
the function (thus evaluating EXPR in the proper lexical
environment)."
  (cons (buttercup--enclosed-expr fun)
        (funcall fun)))

(defun buttercup--wrapper-fun-p (fun)
  "Return non-nil if FUN is a zero-arg one-expression function."
  (condition-case nil
      (prog1 t
        (buttercup--enclosed-expr fun))
    (error nil)))

;;;;;;;;;;;;;;;;;;;;
;;; Helper functions

(defun buttercup-format-spec (format specification)
  "Return a string based on FORMAT and SPECIFICATION.

This is a wrapper around `format-spec', which see. This also adds
a call to `save-match-data', as `format-spec' modifies that."
  (save-match-data
    (format-spec format specification)))

;;;;;;;;;;
;;; expect

(define-error 'buttercup-failed "Buttercup test failed" 'buttercup-error-base)

(define-error 'buttercup-pending "Buttercup test is pending" 'buttercup-error-base)

(defmacro expect (arg &optional matcher &rest args)
  "Expect a condition to be true.

This macro knows three forms:

\(expect ARG :MATCHER ARGS...)
  Fail the current test if the matcher does not match these arguments.
  See `buttercup-define-matcher' for more information on matchers.

\(expect (function ARG...))
  Fail the current test if the function call does not return a true value.

\(expect ARG)
  Fail the current test if ARG is not true."
  (let ((wrapped-args
         (mapcar (lambda (expr) `(lambda ()
                                   (quote ,expr)
                                   (buttercup--mark-stackframe)
                                   ,expr))
                 args)))
    `(buttercup-expect
      (lambda ()
        (quote ,arg)
        (buttercup--mark-stackframe)
        ,arg)
      ,(or matcher :to-be-truthy)
      ,@wrapped-args)))

(defun buttercup-expect (arg &optional matcher &rest args)
  "The function for the `expect' macro.

See the macro documentation for details and the definition of
ARG, MATCHER and ARGS."
  (cl-assert (cl-every #'buttercup--wrapper-fun-p (cons arg args)) t)
  (if (not matcher)
      (progn
        (cl-assert (not args) t)
        (when (not (funcall arg))
          (buttercup-fail "Expected %S to be non-nil"
                          (buttercup--enclosed-expr arg))))
    (let ((result (buttercup--apply-matcher matcher (cons arg args))))
      (if (consp result)
          (when (not (car result))
            (buttercup-fail "%s" (cdr result)))
        (when (not result)
          (buttercup-fail "Expected %S %S %s"
                          (buttercup--enclosed-expr arg)
                          matcher
                          (mapconcat (lambda (obj)
                                       (format "%S" (funcall obj)))
                                     args
                                     " ")))))))

(defun buttercup-fail (format &rest args)
  "Fail the current test with the given description.

This is the mechanism underlying `expect'. You can use it
directly if you want to write your own testing functionality.

FORMAT and ARGS are passed to `format'."
  (signal 'buttercup-failed (apply #'format format args)))

(defun buttercup-skip (format &rest args)
  "Skip the current test with the given description.

FORMAT and ARGS are passed to `format'."
  (signal 'buttercup-pending (apply #'format format args)))

(defmacro assume (condition &optional message)
  "Assume CONDITION for the current test.

Assume that CONDITION evaluates to non-nil in the current test.
If it evaluates to nil cancel the current test with MESSAGE. If
MESSAGE is omitted or nil show the condition form instead."
  (let ((message (or message (format "%S => nil" condition))))
    `(unless ,condition
       (buttercup-skip "!! CANCELLED !! %s" ,message))))

(defmacro buttercup-define-matcher (matcher args &rest body)
  "Define a matcher named MATCHER to be used in `expect'.

ARGS is a list of the elements to match together.

The BODY will receive ARGS as functions that can be called (using
`funcall') to get their values. BODY should return either a
simple boolean, or a cons cell of the form (RESULT . MESSAGE). If
RESULT is nil, MESSAGE should describe why the matcher failed. If
RESULT is non-nil, MESSAGE should describe why a negated matcher
failed."
  (declare (indent defun))
  `(put ,matcher 'buttercup-matcher
        (lambda ,args
          ,@body)))

(defun buttercup--function-as-matcher (fun)
  "Wrap FUN in code to unpack function-wrapped arguments."
  (cl-assert (functionp fun) t)
  (lambda (&rest args)
    (apply fun (mapcar #'funcall args))))

(defun buttercup--find-matcher-function (matcher)
  "Return the matcher function for MATCHER."
  (let ((matcher-prop
         (when (symbolp matcher)
           (get matcher 'buttercup-matcher))))
    (cond
     ;; Use `buttercup-matcher' property if it's a function
     ((functionp matcher-prop)
      matcher-prop)
     (matcher-prop
      (error "%S %S has a `buttercup-matcher' property that is not a function. Buttercup has been misconfigured"
             (if (keywordp matcher) "Keyword" "Symbol") matcher))
     ;; Otherwise just use `matcher' as a function, wrapping it in
     ;; code to unpack function-wrapped arguments.
     ((functionp matcher)
      (buttercup--function-as-matcher matcher))
     (matcher (error "Not a test: `%S'" matcher))
     ;; If `matcher' is nil, then we just want a basic truth test
     ((null matcher)
      (buttercup--find-matcher-function :to-be-truthy))
     (t (error "This line should never run")))))

(defun buttercup--apply-matcher (matcher args)
  "Apply MATCHER to ARGS.

ARGS is a list of functions that must be `funcall'ed to get their
values.
MATCHER is either a matcher keyword defined with
`buttercup-define-matcher', or a function."
  (cl-assert (cl-every #'buttercup--wrapper-fun-p args) t)
  (let ((function
         (buttercup--find-matcher-function matcher)))
    (apply function args)))

(cl-defmacro buttercup--test-expectation
    (expression &key expect-match-phrase expect-mismatch-phrase)
  "Wrapper for the common matcher case of two possible messages.

The logic for the return values of buttercup matchers can be
unintuitive, since the return value is a cons cell whose first
element is t for a mismatch and nil for a match. In the simple
case where there are only two possible
messages (EXPECT-MATCH-PHRASE for a match and
EXPECT-MISMATCH-PHRASE for a mismatch), this macro allows you to
simply specify those two phrases and the EXPRESSION to test."
  (declare (indent 1))
  (cl-assert expect-match-phrase)
  (cl-assert expect-mismatch-phrase)
  `(let ((value ,expression))
     (if value
         (cons t ,expect-mismatch-phrase)
       (cons nil ,expect-match-phrase))))

(cl-defmacro buttercup-define-matcher-for-unary-function
    (matcher function &key
             expect-match-phrase expect-mismatch-phrase function-name)
  "Shortcut to define a MATCHER for a 1-argument FUNCTION.

When the matcher is used, keyword arguments EXPECT-MATCH-PHRASE
and EXPECT-MISMATCH-PHRASE are used to construct the return
message. It may contain `%f', `%A', and `%a', which will be
replaced with the function name, the expression of the argument
the matcher was called on, and the value of that argument,
respectively. If not provided, the default EXPECT-MATCH-PHRASE
is:

    Expected `%A' to match `%f', but instead it was `%a'.

Similarly, the default EXPECT-MISMATCH-PHRASE is:

    Expected `%A' not to match `%f', but it was `%a'.

To include a literal `%' in either message, use `%%'.

If FUNCTION is passed as a lambda expression or other non-symbol, then
you must provide a keyword argument FUNCTION-NAME to be used in
the match/mismatch messages. Otherwise, FUNCTION-NAME will be
used instead of FUNCTION if both are non-nil SYMBOLS.

If FUNCTION (or FUNCTION-NAME) has an `ert-explainer' property,
this will be used to generate the default EXPECT-MATCH-PHRASE.

See also `buttercup-define-matcher'."
  (declare (indent 2))
  ;; Use the ERT explainer for FUNCTION if available to generate the
  ;; default expect-match phrase.
  (let ((explainer (or (when function-name
                         (get function-name 'ert-explainer))
                       (when (symbolp function)
                         (get function 'ert-explainer)))))
    (cl-assert (symbolp function-name) t)
    (cl-assert (functionp function) t)
    (unless expect-match-phrase
      (setq expect-match-phrase
            (if explainer
                ;; %x is the undocumented substitution for the
                ;; explainer's output
                "Expected `%A' to match `%f', but instead it was `%a' which did not match because: %x."
              "Expected `%A' to match `%f', but instead it was `%a'.")))
    (unless expect-mismatch-phrase
      (setq expect-mismatch-phrase
            "Expected `%A' not to match `%f', but it was `%a'."))
    (when (and (null function-name)
               ;; Only need a function name if either phrase contains
               ;; an unescaped `%f'.
               (string-match-p
                "%f"
                (replace-regexp-in-string
                 "%%" ""
                 (concat expect-match-phrase " "
                         expect-mismatch-phrase))))
      (if (symbolp function)
          (setq function-name function)
        (error "The `:function-name' keyword is required if FUNCTION is not a symbol")))
    `(buttercup-define-matcher ,matcher (arg)
       (let* ((expr (buttercup--enclosed-expr arg))
              (value (funcall arg))
              (explanation (and ',explainer (funcall ',explainer arg)))
              (spec (format-spec-make
                     ?f ',function-name
                     ?A (format "%S" expr)
                     ?a (format "%S" value)
                     ?x (format "%S" explanation))))
         (buttercup--test-expectation (funcall ',function value)
           :expect-match-phrase (buttercup-format-spec ,expect-match-phrase spec)
           :expect-mismatch-phrase (buttercup-format-spec ,expect-mismatch-phrase spec))))))

(cl-defmacro buttercup-define-matcher-for-binary-function
    (matcher function &key
             expect-match-phrase expect-mismatch-phrase function-name)
  "Shortcut to define a MATCHER for a 2-argument FUNCTION.

When the matcher is used, keyword arguments EXPECT-MATCH-PHRASE
and EXPECT-MISMATCH-PHRASE are used to construct the return
message. It may contain `%f', `%A', `%a', `%B', and `%b'. The
token `%f' will be replaced with the function name. `%A' and `%B'
will be replaced with the unevaluted expressions of the two
arguments passed to the function, while `%a' and `%b' will be
replaced with their values. not provided, the default
EXPECT-MATCH-PHRASE is:

    Expected `%A' to be `%f' to `%b', but instead it was `%a'.

Similarly, the default EXPECT-MISMATCH-PHRASE is:

    Expected `%A' not to be `%f' to `%b', but it was.

To include a literal `%' in either message, use `%%'.

If FUNCTION is passed as a lambda expression or other non-symbol, then
you must provide a keyword argument FUNCTION-NAME to be used in
the match/mismatch messages (unless neither one contains `%f').
If both are non-nil symbols, FUNCTION-NAME will be used instead
of FUNCTION in messages.

If FUNCTION (or FUNCTION-NAME) has an `ert-explainer' property,
this will be used to generate the default EXPECT-MATCH-PHRASE.

See also `buttercup-define-matcher'."
  (declare (indent 2))
  ;; Use the ERT explainer for FUNCTION if available to generate the
  ;; default expect-match phrase.
  (let ((explainer (or (when function-name
                         (get function-name 'ert-explainer))
                       (when (symbolp function)
                         (get function 'ert-explainer)))))
    (cl-assert (symbolp function-name) t)
    (cl-assert (functionp function) t)
    (unless expect-match-phrase
      (setq expect-match-phrase
            (if explainer
                ;; %x is the undocumented substitution for the
                ;; explainer's output
                "Expected `%A' to be `%f' to `%b', but instead it was `%a' which does not match because: %x."
              "Expected `%A' to be `%f' to `%b', but instead it was `%a'.")))
    (unless expect-mismatch-phrase
      (setq expect-mismatch-phrase
            "Expected `%A' not to be `%f' to `%b', but it was."))
    (when (and (null function-name)
               ;; Only need a function name if either phrase contains
               ;; an unescaped `%f'.
               (string-match-p
                "%f"
                (replace-regexp-in-string
                 "%%" ""
                 (concat expect-match-phrase " "
                         expect-mismatch-phrase))))
      (if (symbolp function)
          (setq function-name function)
        (error "The `:function-name' keyword is required if FUNCTION is not a symbol")))
    `(buttercup-define-matcher ,matcher (a b)
       (cl-destructuring-bind
           ((a-expr . a) (b-expr . b))
           (mapcar #'buttercup--expr-and-value (list a b))
         (let* ((explanation (and ',explainer (funcall ',explainer a b)))
                (spec (format-spec-make
                       ?f ',function-name
                       ?A (format "%S" a-expr)
                       ?a (format "%S" a)
                       ?B (format "%S" b-expr)
                       ?b (format "%S" b)
                       ?x (format "%S" explanation))))
           (buttercup--test-expectation (funcall #',function a b)
             :expect-match-phrase (buttercup-format-spec ,expect-match-phrase spec)
             :expect-mismatch-phrase (buttercup-format-spec ,expect-mismatch-phrase spec)))))))

;;;;;;;;;;;;;;;;;;;;;
;;; Built-in matchers

(buttercup-define-matcher-for-unary-function :to-be-truthy identity
  :expect-match-phrase "Expected `%A' to be non-nil, but instead it was nil."
  :expect-mismatch-phrase "Expected `%A' to be nil, but instead it was `%a'.")

(buttercup-define-matcher-for-binary-function :to-be eq)
(buttercup-define-matcher-for-binary-function :to-equal equal)

(buttercup-define-matcher :not (obj matcher &rest args)
  (let* ((matcher (funcall matcher))
         (result (buttercup--apply-matcher matcher (cons obj args))))
    (if (consp result)
        (cons (not (car result))
              (cdr result))
      (not result))))

(buttercup-define-matcher :to-have-same-items-as (a b)
  (cl-destructuring-bind
      ((a-expr . a) (b-expr . b))
      (mapcar #'buttercup--expr-and-value (list a b))
    (let* ((a-uniques (cl-set-difference a b :test #'equal))
           (b-uniques (cl-set-difference b a :test #'equal))
           (spec (format-spec-make
                  ?A (format "%S" a-expr)
                  ?a (format "%S" a)
                  ?B (format "%S" b-expr)
                  ?b (format "%S" b)
                  ?m (format "%S" b-uniques)
                  ?p (format "%S" a-uniques))))
      (cond
       ((and a-uniques b-uniques)
        (cons nil (buttercup-format-spec
                   "Expected `%A' to contain the same items as `%b', but `%m' are missing and `%p' are present unexpectedly."
                   spec)))
       (a-uniques
        (cons nil (buttercup-format-spec
                   "Expected `%A' to contain the same items as `%b', but `%p' are present unexpectedly."
                   spec)))
       (b-uniques
        (cons nil (buttercup-format-spec
                   "Expected `%A' to contain the same items as `%b', but `%m' are missing."
                   spec)))
       (t
        (cons t (buttercup-format-spec
                 "Expected `%A' not to have same items as `%b'"
                 spec)))))))

(buttercup-define-matcher :to-match (text regexp)
  (cl-destructuring-bind
      ((text-expr . text) (regexp-expr . regexp))
      (mapcar #'buttercup--expr-and-value (list text regexp))
    (save-match-data
      (let* (;; For string literals, juse use them normally, but for
             ;; expressions, show both the expr and its string value
             (text-is-literal (equal text-expr text))
             (regexp-is-literal (equal regexp-expr regexp))
             (text-desc
              (if text-is-literal
                  text-expr
                (format "`%S' with value %S"
                        text-expr text)))
             (regexp-desc
              (if regexp-is-literal
                  regexp-expr
                (format "`%S' with value %S"
                        regexp-expr regexp)))
             (match-p (string-match regexp text))
             ;; Get some more details about the match
             (start
              (when match-p
                (match-beginning 0)))
             (end
              (when match-p
                (match-end 0)))
             (matched-substring
              (when match-p
                (substring text start end)))
             (spec (format-spec-make
                    ?T text-desc
                    ?t (format "%S" text)
                    ?R regexp-desc
                    ?r (format "%S" regexp)
                    ?m (format "%S" matched-substring)
                    ?a start
                    ?z end)))
        (buttercup--test-expectation match-p
          :expect-match-phrase
          (buttercup-format-spec "Expected %T to match the regexp %r, but instead it was %t."
                                 spec)
          :expect-mismatch-phrase
          (buttercup-format-spec "Expected %T not to match the regexp %r, but it matched the substring %m from position %a to %z."
                                 spec))))))

(buttercup-define-matcher-for-binary-function
    :to-be-in member
  :expect-match-phrase "Expected `%A' to be an element of `%b', but it was `%a'."
  :expect-mismatch-phrase "Expected `%A' not to be an element of `%b', but it was `%a'.")

(buttercup-define-matcher-for-binary-function
    ;; Reverse the args
    :to-contain (lambda (a b) (member b a))
  :expect-match-phrase "Expected `%A' to be a list containing `%b', but instead it was `%a'."
  :expect-mismatch-phrase "Expected `%A' to be a list not containing `%b', but instead it was `%a'.")

(buttercup-define-matcher-for-binary-function
    :to-be-less-than <
  :expect-match-phrase "Expected `%A' < %b, but `%A' was %a."
  :expect-mismatch-phrase "Expected `%A' >= %b, but `%A' was %a.")
(buttercup-define-matcher-for-binary-function
    :to-be-greater-than >
  :expect-match-phrase "Expected `%A' > %b, but `%A' was %a."
  :expect-mismatch-phrase "Expected `%A' <= %b, but `%A' was %a.")
(buttercup-define-matcher-for-binary-function
    :to-be-weakly-less-than <=
  :expect-match-phrase "Expected `%A' <= %b, but `%A' was %a."
  :expect-mismatch-phrase "Expected `%A' > %b, but `%A' was %a.")
(buttercup-define-matcher-for-binary-function
    :to-be-weakly-greater-than >=
  :expect-match-phrase "Expected `%A' >= %b, but `%A' was %a."
  :expect-mismatch-phrase "Expected `%A' < %b, but `%A' was %a.")

(buttercup-define-matcher :to-be-close-to (a b precision)
  (cl-destructuring-bind
      (precision (a-expr . a) (_b-expr . b))
      (cons (funcall precision)
            (mapcar #'buttercup--expr-and-value (list a b)))
    (let ((tolerance (expt 10.0 (- precision))))
      (buttercup--test-expectation
          (< (abs (- a b)) tolerance)
        :expect-match-phrase
        (format "Expected `%S' to be within %s of %s, but instead it was %s, with a difference of %s"
                a-expr tolerance b a (abs (- a b)))
        :expect-mismatch-phrase
        (format "Expected `%S' to differ from %s by more than %s, but instead it was %s, with a difference of %s"
                a-expr b tolerance a (abs (- a b)))))))

(buttercup-define-matcher :to-throw (expr &optional signal signal-args)
  (let ((expected-signal-symbol (or (and signal (funcall signal)) 'error))
        (expected-signal-args (and signal-args (funcall signal-args)))
        (unevaluated-expr (buttercup--enclosed-expr expr))
        expr-value
        thrown-signal
        thrown-signal-symbol
        thrown-signal-args)
    (when (and (functionp unevaluated-expr)
               (member (car unevaluated-expr) '(lambda closure)))
      (display-warning
       'buttercup
       (buttercup-colorize
        (format "Probable incorrect use of `:to-throw' matcher: pass an expression instead of a function: `%S'"
                unevaluated-expr)
        'yellow)))
    ;; If no signal specificaiton, use `error' as the signal symbol
    (when (and (null expected-signal-symbol)
               (null expected-signal-args))
      (setq expected-signal-symbol 'error))
    ;; Set the above 4 variables
    (condition-case err
        (setq expr-value
              (funcall expr))
      (error
       (setq thrown-signal err
             thrown-signal-symbol (car err)
             thrown-signal-args (cdr err))
       nil))
    (let*
        ((matched
          (and thrown-signal
               (or (null expected-signal-symbol)
                   (memq expected-signal-symbol (get thrown-signal-symbol 'error-conditions)))
               (or (null expected-signal-args)
                   (equal thrown-signal-args expected-signal-args))))
         (spec (format-spec-make
                ?E (format "%S" unevaluated-expr)
                ?e (format "%S" expr-value)
                ?t (format "%S" thrown-signal)
                ?s (if expected-signal-symbol
                       (format "a child signal of `%S'" expected-signal-symbol)
                     "a signal")
                ?a (if expected-signal-args
                       (format " with args `%S'" expected-signal-args)
                     "")))
         (result-text
          (if thrown-signal
              (buttercup-format-spec "it threw %t" spec)
            (buttercup-format-spec "it evaluated successfully, returning value `%e'" spec)))

         (expect-match-text
          (concat (buttercup-format-spec "Expected `%E' to throw %s%a" spec)
                  ", but instead "
                  result-text))
         (expect-mismatch-text
          (concat (buttercup-format-spec "Expected `%E' not to throw %s%a" spec)
                  ", but "
                  result-text)))
      (buttercup--test-expectation matched
        :expect-match-phrase expect-match-text
        :expect-mismatch-phrase expect-mismatch-text))))

(buttercup-define-matcher :to-have-been-called (spy)
  (setq spy (funcall spy))
  (cl-assert (symbolp spy))
  (if (spy-calls-all (funcall spy))
      t
    nil))

(buttercup-define-matcher :to-have-been-called-with (spy &rest args)
  (setq spy (funcall spy))
  (cl-assert (symbolp spy))
  (setq args (mapcar #'funcall args))
  (let* ((calls (mapcar 'spy-context-args (spy-calls-all spy))))
    (cond
     ((not calls)
      (cons nil
            (format "Expected `%s' to have been called with %S, but it was not called at all" spy args)))
     ((not (member args calls))
      (cons nil
            (format "Expected `%s' to have been called with %S, but it was called with %s"
                    spy
                    args
                    (mapconcat (lambda (args)
                                 (format "%S" args))
                               calls
                               ", "))))
     (t
      t))))

(buttercup-define-matcher :to-have-been-called-times (spy number)
  (setq spy (funcall spy)
        number (funcall number))
  (cl-assert (symbolp spy))
  (let* ((call-count (spy-calls-count spy)))
    (cond
     ((= number call-count)
      t)
     (t
      (cons nil
            (format "Expected `%s' to have been called %s %s, but it was called %s %s"
                    spy
                    number (if (= number 1) "time" "times")
                    call-count (if (= call-count 1) "time" "times")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Suite and spec data structures

(cl-defstruct buttercup-suite-or-spec
  ;; The name of this specific suite
  description
  ;; The parent of this suite, another suite
  parent
  ;; One of: passed failed pending
  (status 'passed)
  failure-description
  failure-stack
  time-started
  time-ended)

(cl-defstruct (buttercup-suite (:include buttercup-suite-or-spec))
  ;; Any children of this suite, both suites and specs
  children
  ;; Closure to run before and after each spec in this suite and its
  ;; children
  before-each
  after-each
  ;; Likewise, but before and after all specs.
  before-all
  after-all)

(cl-defstruct (buttercup-spec (:include buttercup-suite-or-spec))
  ;; The closure to run for this spec
  function)

(defun buttercup-suite-add-child (parent child)
  "Add a CHILD suite to a PARENT suite.
Return CHILD."
  (setf (buttercup-suite-children parent)
        (append (buttercup-suite-children parent)
                (list child)))
  (setf (buttercup-suite-or-spec-parent child) parent)
  child)

(defun buttercup-suite-or-spec-parents (suite-or-spec)
  "Return a list of parents of SUITE-OR-SPEC."
  (when (buttercup-suite-or-spec-parent suite-or-spec)
    (cons (buttercup-suite-or-spec-parent suite-or-spec)
          (buttercup-suite-or-spec-parents (buttercup-suite-or-spec-parent suite-or-spec)))))

(define-obsolete-function-alias 'buttercup-suite-parents 'buttercup-suite-or-spec-parents "emacs-buttercup 1.10")
(define-obsolete-function-alias 'buttercup-spec-parents 'buttercup-suite-or-spec-parents "emacs-buttercup 1.10")

(defun buttercup-suites-total-specs-defined (suite-list)
  "Return the number of specs defined in all suites in SUITE-LIST."
  (length (buttercup--specs suite-list)))

(defun buttercup-suites-total-specs-status (suite-list status)
  "Return the number of specs in SUITE-LIST marked with STATUS."
  (cl-count status (buttercup--specs suite-list) :key #'buttercup-spec-status))

(defun buttercup-suites-total-specs-pending (suite-list)
  "Return the number of specs marked as pending in all suites in SUITE-LIST."
  (buttercup-suites-total-specs-status suite-list 'pending))

(defun buttercup-suites-total-specs-failed (suite-list)
  "Return the number of failed specs in all suites in SUITE-LIST."
  (buttercup-suites-total-specs-status suite-list 'failed))

(defun buttercup--specs (spec-or-suite-list)
  "Return a flat list of all specs in SPEC-OR-SUITE-LIST."
  (let (specs)
    (dolist (spec-or-suite spec-or-suite-list specs)
      (if (buttercup-spec-p spec-or-suite)
          (setq specs (append specs (list spec-or-suite)))
        (setq specs (append specs (buttercup--specs
                                   (buttercup-suite-children spec-or-suite))))))))

(defun buttercup--specs-and-suites (spec-or-suite-list)
  "Return a flat list of all specs and suites in SPEC-OR-SUITE-LIST."
  (let ((specs-and-suites nil))
    (dolist (spec-or-suite spec-or-suite-list specs-and-suites)
      (setq specs-and-suites (append specs-and-suites
                                     (list spec-or-suite)))
      (when (buttercup-suite-p spec-or-suite)
        (setq specs-and-suites
              (append specs-and-suites
                      (buttercup--specs-and-suites
                       (buttercup-suite-children spec-or-suite))))))))

(defun buttercup-suite-full-name (suite)
  "Return the full name of SUITE, which includes the names of the parents."
  (mapconcat #'buttercup-suite-description
             (nreverse (cons suite (buttercup-suite-or-spec-parents suite)))
             " "))

(defun buttercup-spec-full-name (spec)
  "Return the full name of SPEC, which includes the full name of its suite."
  (let ((parent (buttercup-spec-parent spec)))
    (if parent
        (concat (buttercup-suite-full-name parent)
                " "
                (buttercup-spec-description spec))
      (buttercup-spec-description spec))))

(defun buttercup--full-spec-names (spec-or-suite-list)
  "Return full names of all specs in SPEC-OR-SUITE-LIST."
  (cl-loop
   for x in (buttercup--specs spec-or-suite-list)
   collect (buttercup-spec-full-name x)))

(defun buttercup--find-duplicate-spec-names (spec-or-suite-list)
  "Return duplicate full spec names among SPEC-OR-SUITE-LIST."
  (let ((seen '())
        (duplicates '()))
    (dolist (name (buttercup--full-spec-names spec-or-suite-list)
                  (nreverse duplicates))
      (if (member name seen)
          (push name duplicates)
        (push name seen)))))

(defun buttercup--set-start-time (suite-or-spec)
  "Set time-started of SUITE-OR-SPEC to `current-time'."
  (setf (buttercup-suite-or-spec-time-started suite-or-spec) (current-time)))

(defun buttercup--set-end-time (suite-or-spec)
  "Set time-ended of SUITE-OR-SPEC to `current-time'."
  (setf (buttercup-suite-or-spec-time-ended suite-or-spec) (current-time)))

(defun buttercup-elapsed-time (suite-or-spec)
  "Get elapsed time of SUITE-OR-SPEC."
  ;; time-subtract does not handle nil arguments until Emacs 25.1
  (time-subtract
   (or (buttercup-suite-or-spec-time-ended suite-or-spec) (current-time))
   (or (buttercup-suite-or-spec-time-started suite-or-spec) (current-time))))

(defun buttercup-elapsed-time-string (suite-or-spec)
  "Convert the elapsed time for SUITE-OR-SPEC to a short string."
  (seconds-to-string (float-time (buttercup-elapsed-time suite-or-spec))))

(defun buttercup--indented-description (suite-or-spec)
  "Return the description of SUITE-OR-SPEC indented according to level.
The indentaion is two spaces per parent."
  (let ((level (length (buttercup-suite-or-spec-parents suite-or-spec))))
    (concat (make-string (* 2 level) ?\s) (buttercup-suite-or-spec-description suite-or-spec))))

(defun buttercup--spec-mark-pending (spec description &optional description-for-now)
  "Mark SPEC as pending with DESCRIPTION.
If DESCRIPTION-FOR-NOW is non-nil, set the spec
`pending-description' to that value for now, it will be reset to
DESCRIPTION when the spec is run. Return SPEC."
  (setf (buttercup-spec-function spec)
        (lambda () (signal 'buttercup-pending description))
        (buttercup-spec-status spec) 'pending)
  (when description-for-now
    (setf (buttercup-spec-failure-description spec) description-for-now))
  spec)

;;;;;;;;;;;;;;;;;;;;
;;; Suites: describe

(defvar buttercup-suites nil
  "The list of all currently defined Buttercup suites.")

(defvar buttercup--current-suite nil
  "The suite currently being defined.

Do not set this globally. It is let-bound by the `describe'
form.")

(defmacro describe (description &rest body)
  "Describe a test suite.

DESCRIPTION is a string. BODY is a sequence of instructions,
mainly calls to `describe', `it' and `before-each'."
  (declare (indent 1) (debug (&define sexp def-body)))
  (let ((new-body
         (cond
          ((eq (elt body 0) :var)
           `((let ,(elt body 1)
               ,@(cddr body))))
          ((eq (elt body 0) :var*)
           `((let* ,(elt body 1)
               ,@(cddr body))))
          (t body))))
    `(buttercup-describe ,description (lambda () ,@new-body))))

(defun buttercup-describe (description body-function)
  "Function to handle a `describe' form.

DESCRIPTION has the same meaning as in `describe'. BODY-FUNCTION
is a function containing the body instructions passed to
`describe'."
  (let* ((enclosing-suite buttercup--current-suite)
         (buttercup--current-suite (make-buttercup-suite
                                    :description description)))
    (condition-case nil
        (funcall body-function)
      (buttercup-pending
       (setf (buttercup-suite-status buttercup--current-suite)
             'pending)))
    (if enclosing-suite
        (buttercup-suite-add-child enclosing-suite
                                   buttercup--current-suite)
      ;; At top level, warn about duplicate spec names
      (let ((dups (buttercup--find-duplicate-spec-names
                   (list buttercup--current-suite))))
        (when dups
          ;; TODO: Use `buttercup--warn'
          (display-warning
           'buttercup
           (format "Found duplicate spec names in suite: %S"
                   (delete-dups dups)))))
      (setq buttercup-suites (append buttercup-suites
                                     (list buttercup--current-suite)))
      buttercup--current-suite)))

;;;;;;;;;;;;;
;;; Specs: it

(defmacro it (description &rest body)
  "Define a spec.

DESCRIPTION is a string. BODY is a sequence of instructions,
most probably including one or more calls to `expect'."
  (declare (indent 1) (debug (&define sexp def-body)))
  (if body
      `(buttercup-it ,description
         (lambda ()
           (buttercup-with-converted-ert-signals
             (buttercup--mark-stackframe)
             ,@body)))
    `(buttercup-xit ,description)))

(defun buttercup-it (description body-function)
  "Function to handle an `it' form.

DESCRIPTION has the same meaning as in `it'. BODY-FUNCTION is a
function containing the body instructions passed to `it'. Return
the created spec object."
  (declare (indent 1))
  (when (not buttercup--current-suite)
    (error "`it' has to be called from within a `describe' form"))
  (buttercup-suite-add-child buttercup--current-suite
                             (make-buttercup-spec
                              :description description
                              :function body-function)))

;;;;;;;;;;;;;;;;;;;;;;
;;; Setup and Teardown

(defmacro before-each (&rest body)
  "Run BODY before each spec in the current suite."
  (declare (indent 0) (debug (&define def-body)))
  `(buttercup-before-each (lambda () ,@body)))

(defun buttercup-before-each (function)
  "The function to handle a `before-each' form.

FUNCTION is a function containing the body instructions passed to
`before-each'."
  (setf (buttercup-suite-before-each buttercup--current-suite)
        (append (buttercup-suite-before-each buttercup--current-suite)
                (list function))))

(defmacro after-each (&rest body)
  "Run BODY after each spec in the current suite."
  (declare (indent 0) (debug (&define def-body)))
  `(buttercup-after-each (lambda () ,@body)))

(defun buttercup-after-each (function)
  "The function to handle an `after-each' form.

FUNCTION is a function containing the body instructions passed to
`after-each'."
  (setf (buttercup-suite-after-each buttercup--current-suite)
        (append (buttercup-suite-after-each buttercup--current-suite)
                (list function))))

(defmacro before-all (&rest body)
  "Run BODY before every spec in the current suite."
  (declare (indent 0) (debug (&define def-body)))
  `(buttercup-before-all (lambda () ,@body)))

(defun buttercup-before-all (function)
  "The function to handle a `before-all' form.

FUNCTION is a function containing the body instructions passed to
`before-all'."
  (setf (buttercup-suite-before-all buttercup--current-suite)
        (append (buttercup-suite-before-all buttercup--current-suite)
                (list function))))

(defmacro after-all (&rest body)
  "Run BODY after every spec in the current suite."
  (declare (indent 0) (debug (&define def-body)))
  `(buttercup-after-all (lambda () ,@body)))

(defun buttercup-after-all (function)
  "The function to handle an `after-all' form.

FUNCTION is a function containing the body instructions passed to
`after-all'."
  (setf (buttercup-suite-after-all buttercup--current-suite)
        (append (buttercup-suite-after-all buttercup--current-suite)
                (list function))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Disabled Suites: xdescribe

(defun buttercup--disable-specs (forms)
  "Process FORMS to make any suites or specs pending."
  (when (eq (car forms) :var)
    (setq forms (cddr forms)))
  (let (retained)
    (dolist (form forms (nreverse retained))
      (pcase form
        ;; Make it pending by just keeping the description
        (`(it ,description . ,_)
         (push (list 'it description) retained))
        (`(xit ,description . ,_)
         (push (list 'it description) retained))
        ;; Just make nested describes into xdescribes and handle them
        ;; in another macro invocation
        (`(describe . ,tail)
         (push (cons 'xdescribe tail) retained))
        (`(xdescribe . ,tail)
         (push (cons 'xdescribe tail) retained))
        ;; Special case to ignore before-* and after-* forms
        (`(before-each . ,_)) ; nop
        (`(after-each . ,_)) ; nop
        (`(before-all . ,_)) ; nop
        (`(after-all . ,_)) ; nop
        ;; Any list starting with a list, like a let varlist.
        ((and (pred consp)
              ls
              (guard (consp (car ls))))
         (dolist (elt (buttercup--disable-specs ls))
           (push elt retained)))
        ;; Any function call list
        (`(,_ . ,tail)
         (dolist (elt (buttercup--disable-specs tail))
           (push elt retained)))
        ;; non-cons items
        ((and elt (guard (not (consp elt))))) ; nop
        (_
         (error "Unrecognized form in `xdescribe': `%s'" (pp-to-string form)))
        ))))

(defmacro xdescribe (description &rest body)
  "Like `describe', but mark any specs as disabled.

DESCRIPTION is a string. BODY is a sequence of instructions,
mainly calls to `describe', `it' and `before-each'."
  (declare (indent 1))
  `(describe ,description
     ,@(buttercup--disable-specs body)
     ;; make sure the suite is marked as pending
     (signal 'buttercup-pending "PENDING")))

;;;;;;;;;;;;;;;;;;;;;;
;;; Pending Specs: xit

(defmacro xit (description &rest body)
  "Like `it', but mark the spec as disabled.

A disabled spec is not run.

DESCRIPTION is a string. BODY is ignored."
  (declare (indent 1))
  (ignore body)
  `(buttercup-xit ,description))

(defun buttercup-xit (description &optional function)
  "Like `buttercup-it', but mark the spec as disabled.
A disabled spec is not run.

DESCRIPTION has the same meaning as in `xit'. FUNCTION is
ignored. Return the created spec object."
  (declare (indent 1))
  (let ((spec (buttercup-it description (or function #'ignore))))
    (buttercup--spec-mark-pending spec "PENDING" "")))

;;;;;;;;;
;;; Spies

(defvar buttercup--spy-contexts (make-hash-table :test 'eq
                                                 :weakness 'key)
  "A mapping of currently-defined spies to their contexts.")

;; The base struct has no constructor so a factory function
;; `make-spy-context' masquerading as a constructor can be defined
;; later.
(cl-defstruct (spy-context (:constructor nil))
  args current-buffer)
(cl-defstruct (spy-context-return (:include spy-context)
                                  (:conc-name spy-context--return-))
  value)
(cl-defstruct (spy-context-thrown (:include spy-context)
                                  (:conc-name spy-context--thrown-))
  signal)

(cl-defun make-spy-context (&key args current-buffer
                                 (return-value nil has-return-value)
                                 (thrown-signal nil has-thrown-signal))
  "Constructor for objects of type spy-context.
ARGS is the argument list of the called function.
CURRENT-BUFFER is the buffer that was current when the spy was called.
RETURN-VALUE is the returned value, if any.
THROWN-SIGNAL is the signal raised by the function, if any.
Only one of RETURN-VALUE and THROWN-SIGNAL may be given. Giving
none of them is equivalent to `:return-value nil'.

\(fn &key ARGS CURRENT-BUFFER RETURN-VALUE THROWN-SIGNAL)"
  (cond
   ((and has-return-value has-thrown-signal)
    (error "Only one of :return-value and :thrown-signal may be given"))
   (has-thrown-signal (make-spy-context-thrown :args args
                                               :current-buffer current-buffer
                                               :signal thrown-signal))
   (t (make-spy-context-return :args args
                               :current-buffer current-buffer
                               :value return-value))))

(defun spy-context-return-value (context)
  "Access slot \"return-value\" of `spy-context' struct CONTEXT."
  (unless (spy-context-return-p context)
    (error "Not a returning context"))
  (spy-context--return-value context))

(defun spy-context-thrown-signal (context)
  "Access slot \"thrown-signal\" of `spy-context' struct CONTEXT."
  (unless (spy-context-thrown-p context)
    (error "Not a signal-raising context"))
  (spy-context--thrown-signal context))

(defun spy-on (symbol &optional keyword arg)
  "Create a spy (mock) for the function SYMBOL.

KEYWORD can have one of the following values:

  :and-call-through -- Track calls, but call the original
      function.

  :and-return-value -- Track calls, but return ARG instead of
      calling the original function.

  :and-call-fake -- Track calls, but call ARG instead of the
      original function.

  :and-throw-error -- Signal ARG as an error instead of calling
      the original function.

  nil -- Track calls, but simply return nil instead of calling
      the original function.

If the original function was a command, the generated spy will
also be a command with the same interactive form, unless
`:and-call-fake' is used, in which case it is the caller's
responsibility to ensure ARG is a command."
  ;; We need to load an autoloaded function before spying on it
  (when (autoloadp (and (fboundp symbol) (symbol-function symbol)))
    (autoload-do-load (symbol-function symbol) symbol))
  (cl-assert (not (autoloadp (and (fboundp symbol) (symbol-function symbol)))))
  (let* ((orig (and (fboundp symbol) (symbol-function symbol)))
         (orig-intform (interactive-form orig))
         (replacement
          (pcase
              keyword
            (:and-call-through
             (when arg
               (error "`spy-on' with `:and-call-through' does not take an ARG"))
             `(lambda (&rest args)
                ,orig-intform
                (apply ',orig args)))
            (:and-return-value
             `(lambda (&rest args)
                ,orig-intform
                ',arg))
            (:and-call-fake
             (let ((replacement-intform (interactive-form arg)))
               (when (and replacement-intform
                          (not (equal orig-intform replacement-intform)))
                 (display-warning
                  'buttercup
                  (format "While spying on `%S': replacement does not have the same interactive form"
                          symbol)))
               `(lambda (&rest args)
                  ,(or replacement-intform orig-intform)
                  (apply (function ,arg) args))))
            (:and-throw-error
             `(lambda (&rest args)
                ,orig-intform
                (signal ',(or arg 'error) "Stubbed error")))
            ;; No keyword: just spy
            (`nil
             (when arg
               (error "`spy-on' with no KEYWORD does not take an ARG"))
             `(lambda (&rest args)
                ,orig-intform
                nil))
            (_
             (error "Invalid `spy-on' keyword: `%S'" keyword)))))
    (unless (buttercup--spy-on-and-call-replacement symbol replacement)
      (error "Spies can only be created in `before-each'"))))


(defun buttercup--spy-on-and-call-replacement (spy fun)
  "Replace the function in symbol SPY with a spy calling FUN."
  (let ((orig-function (and (fboundp spy) (symbol-function spy))))
    (when (buttercup--add-cleanup (lambda ()
                                  (fset spy orig-function)))
      (fset spy (buttercup--make-spy fun)))))

(defun buttercup--make-spy (fun)
  "Create a new spy function wrapping FUN and tracking every call to itself."
  (let (this-spy-function)
    (setq
     this-spy-function
     (lambda (&rest args)
       (let ((returned nil)
             (return-value nil))
         (condition-case err
             (progn
               (setq return-value (apply fun args)
                     returned t)
               (buttercup--spy-calls-add
                this-spy-function
                (make-spy-context :args args
                                  :return-value return-value
                                  :current-buffer (current-buffer)))
               return-value)
           (error
            ;; If returned is non-nil, then the error we caught
            ;; didn't come from FUN, so we shouldn't record it.
            (unless returned
              (buttercup--spy-calls-add
               this-spy-function
               (make-spy-context :args args
                                 :thrown-signal err
                                 :current-buffer (current-buffer))))
            ;; Regardless, we only caught this error in order to
            ;; record it, so we need to re-throw it.
            (signal (car err) (cdr err)))))))
    ;; Add the interactive form from `fun', if any
    (when (interactive-form fun)
      (setq this-spy-function
            `(lambda (&rest args)
               ,(interactive-form fun)
               (apply ',this-spy-function args))))
    this-spy-function))

(defvar buttercup--cleanup-functions :inactive
  "Stack of cleanup operations.
Should always be set to a value that is not `listp', except while
in a `buttercup-with-cleanup' environment.")

(defmacro buttercup-with-cleanup (&rest body)
  "Execute BODY, cleaning spys and the rest afterwards."
  `(let ((buttercup--cleanup-functions nil))
     (unwind-protect (progn ,@body)
       (dolist (fun buttercup--cleanup-functions)
         (ignore-errors
           (funcall fun))))))

(defun buttercup--add-cleanup (function)
  "Register FUNCTION for cleanup in `buttercup-with-cleanup'."
  (when (listp buttercup--cleanup-functions)
    (setq buttercup--cleanup-functions
          (cons function buttercup--cleanup-functions))))

(defun spy-calls-all (spy)
  "Return all call contexts for SPY."
  (gethash (symbol-function spy)
           buttercup--spy-contexts))

(defun buttercup--spy-calls-add (spy-function context)
  "Add CONTEXT to the call records for SPY-FUNCTION."
  (puthash spy-function
           (append (gethash spy-function
                            buttercup--spy-contexts)
                   (list context))
           buttercup--spy-contexts))

(defun spy-calls-reset (spy)
  "Reset SPY, removing all stored contexts."
  (puthash (symbol-function spy)
           nil
           buttercup--spy-contexts))

(buttercup-define-matcher :to-have-been-called (spy)
  (if (spy-calls-all (funcall spy))
      t
    nil))

(defun spy-calls-any (spy)
  "Return t iff SPY has been called at all, nil otherwise."
  (if (spy-calls-all spy)
      t
    nil))

(defun spy-calls-count (spy)
  "Return the number of times SPY has been called so far."
  (length (spy-calls-all spy)))

(defun spy-calls-count-returned (spy)
  "Return the number of times SPY has been called successfully so far."
  (cl-count-if 'spy-context-return-p (spy-calls-all spy)))

(defun spy-calls-count-errors (spy)
  "Return the number of times SPY has been called and thrown errors so far."
  (cl-count-if 'spy-context-thrown-p (spy-calls-all spy)))

(defun spy-calls-args-for (spy index)
  "Return the context of the INDEXth call to SPY."
  (let ((context (elt (spy-calls-all spy)
                      index)))
    (if context
        (spy-context-args context)
      nil)))

(defun spy-calls-all-args (spy)
  "Return the arguments for every recorded call to SPY."
  (mapcar 'spy-context-args (spy-calls-all spy)))

(defun spy-calls-most-recent (spy)
  "Return the context of the most recent call to SPY."
  (car (last (spy-calls-all spy))))

(defun spy-calls-first (spy)
  "Return the context of the first call to SPY."
  (car (spy-calls-all spy)))

;;;;;;;;;;;;;;;;
;;; Test Runners

;; These variables are generally used in the test runners, but set
;; elsewhere. They must be defined here before their first use.
(defvar buttercup-reporter #'buttercup-reporter-adaptive
  "The reporter function for buttercup test runs.

During a run of buttercup, the value of this variable is called
as a function with two arguments. The first argument is a symbol
describing the event, the second depends on the event.

The following events are known:

buttercup-started -- The test run is starting. The argument is a
  list of suites this run will execute.

suite-started -- A suite is starting. The argument is the suite.
  See `make-buttercup-suite' for details on this structure.

spec-started -- A spec in is starting. The argument is the spec.
  See `make-buttercup-spec' for details on this structure.

spec-done -- A spec has finished executing. The argument is the
  spec.

suite-done -- A suite has finished. The argument is the suite.

buttercup-done -- All suites have run, the test run is over. The
  argument is the list of executed suites.")

(defvar buttercup-stack-frame-style (car '(crop full pretty))
  "Style to use when printing stack traces of tests.

`full' is roughly the same style as normal Emacs stack traces:
print each stack frame in full with no line breaks. `crop' is
like full, but truncates each line to 80 characters. `pretty'
uses `pp' to generate a multi-line indented representation of
each frame, and prefixes each stack frame with lambda or M to
indicate whether it represents a normal evaluated function call
or a macro/special form.")

(defvar buttercup-color t
  "Whether to use colors in output.")

(defconst buttercup-warning-buffer-name " *Buttercup-Warnings*"
  "Buffer name used to collect warnings issued while running a spec.

A buffer with this name should only exist while running a test
spec, and should be killed after running the spec.")

;; predeclaration
(defvar buttercup-reporter-batch-quiet-statuses)

;;;###autoload
(defun buttercup-run-at-point ()
  "Run the buttercup suite at point."
  (interactive)
  (let ((buttercup-suites nil)
        (lexical-binding t))
    (save-selected-window
      (eval-defun nil)
      (buttercup-run))
    (message "Suite executed successfully")))

;;;###autoload
(defun buttercup-run-discover ()
  "Discover and load test files, then run all defined suites.

Takes directories as command line arguments, defaulting to the
current directory."
  (setq backtrace-on-error-noninteractive nil)
  (let ((dirs nil)
        (patterns nil)
        (args command-line-args-left)
        failed-files-suite)
    (while args
      (cond
       ((equal (car args) "--")
        (setq args (cdr args)))
       ((member (car args) '("--traceback"))
        (when (not (cdr args))
          (error "Option requires argument: %s" (car args)))
        ;; Make sure it's a valid style by trying to format a dummy
        ;; frame with it
        (buttercup--format-stack-frame '(t myfun 1 2) (intern (cadr args)))
        (setq buttercup-stack-frame-style (intern (cadr args)))
        (setq args (cddr args)))
       ((member (car args) '("-p" "--pattern"))
        (when (not (cdr args))
          (error "Option requires argument: %s" (car args)))
        (push (cadr args) patterns)
        (setq args (cddr args)))
       ((member (car args) '("-c" "--no-color"))
        (setq buttercup-color nil)
        (setq args (cdr args)))
       ((equal (car args) "--no-skip")
        (push 'skipped buttercup-reporter-batch-quiet-statuses)
        (push 'disabled buttercup-reporter-batch-quiet-statuses)
        (setq args (cdr args)))
       ((equal (car args) "--only-error")
        (push 'pending buttercup-reporter-batch-quiet-statuses)
        (push 'passed buttercup-reporter-batch-quiet-statuses)
        (setq args (cdr args)))
       ((equal (car args) "--stale-file-error")
        (buttercup-error-on-stale-elc)
        (setq args (cdr args)))
       (t
        (push (car args) dirs)
        (setq args (cdr args)))))
    (setq command-line-args-left nil)
    (setq failed-files-suite (make-buttercup-suite
                              :description "File failed to load completely: "))
    (dolist (dir (or dirs '(".")))
      (dolist (file (directory-files-recursively
                     dir "\\`test-.*\\.el\\'\\|-tests?\\.el\\'"))
        ;; Exclude any hidden directory, both immediate (^.) and nested (/.) subdirs
        (when (not (string-match "\\(^\\|/\\)\\." (file-relative-name file)))
          (buttercup--load-test-file file failed-files-suite))))
    (when patterns
      (buttercup-mark-skipped patterns t))
    (when (buttercup-suite-children failed-files-suite)
      ;; At least one file has failed to load, add the
      ;; failed-files-suite to the end(?) of the suite list
      (setq buttercup-suites (append buttercup-suites
                                     (list failed-files-suite)))))
    (buttercup-run))

(defun buttercup--load-test-file (file failure-suite)
  "Load FILE keeping track of failures.
If an error is raised by `load', store the error information in a
spec and add it to FAILURE-SUITE."
  (cl-destructuring-bind (status description stack)
      (buttercup--funcall #'load file nil t)
    (when (eq status 'failed)
      ;; Skip all of stack until load is called
      (while (not (eq (nth 1 (car stack)) 'load))
        (pop stack))
      (buttercup-suite-add-child
       failure-suite
       (make-buttercup-spec
        :description file
        :status 'failed
        :failure-description (error-message-string (cadr description))
        :failure-stack stack
        :function (lambda () (ignore)))))))

(defun buttercup-mark-skipped (matcher &optional reverse)
  "Mark any spec that match MATCHER as skipped.
MATCHER can be either a regex, a list of regexes, or a function
taking a spec as the single argument. If REVERSE is non-nil,
specs will be marked as pending when MATCHER does not match."
  (cl-etypecase matcher
    (string (buttercup--mark-skipped
             buttercup-suites
             (lambda (spec)
               (string-match matcher (buttercup-spec-full-name spec)))
             reverse))
    (function (buttercup--mark-skipped buttercup-suites matcher reverse))
    (list (cond
           ((cl-every #'stringp matcher)
            (buttercup-mark-skipped (mapconcat (lambda (re)
                                                 (concat "\\(?:" re "\\)"))
                                               matcher "\\|")
                                    reverse))
           (t (error "Bad matcher list: %s, should be list of strings" matcher))))))

(defun buttercup--mark-skipped (suites predicate &optional reverse-predicate)
  "Mark all specs in SUITES as skipped if PREDICATE(spec) is true.
If REVERSE-PREDICATE is non-nil, mark spec where PREDICATE(spec)
is false."
  (dolist (spec (buttercup--specs suites))
    ;; cond implements (xor reverse-predicate (funcall predicate
    ;; spec)) as xor is introduced in Emacs 27
    (when (cond ((not reverse-predicate) (funcall predicate spec))
                ((not (funcall predicate spec)) reverse-predicate))
      (buttercup--spec-mark-pending spec "SKIPPED"))))

;;;###autoload
(defun buttercup-run-markdown-buffer (&rest markdown-buffers)
  "Run all test suites defined in MARKDOWN-BUFFERS.
A suite must be defined within a Markdown \"lisp\" code block.
If MARKDOWN-BUFFERS is empty (nil), use the current buffer."
  (interactive)
  (unless markdown-buffers
    (setq markdown-buffers (list (current-buffer))))
  (let ((lisp-buffer (generate-new-buffer "elisp"))
        (case-fold-search t)
        code
        buttercup-suites)
    (dolist (markdown-buffer markdown-buffers)
      (with-current-buffer markdown-buffer
        (save-excursion
          (save-match-data
            (goto-char (point-min))
            (while (re-search-forward
                    "```\\(?:emacs-\\|e\\)?lisp\n\\(\\(?:.\\|\n\\)*?\\)```"
                    nil t)
              (setq code (match-string 1))
              (with-current-buffer lisp-buffer
                (insert code)))))))
    (with-current-buffer lisp-buffer
      (setq lexical-binding t)
      (eval-region (point-min)
                   (point-max)))
    (buttercup-run)))

;;;###autoload
(defun buttercup-run-markdown ()
  "Run all test suites defined in Markdown files passed as arguments.
A suite must be defined within a Markdown \"lisp\" code block."
  (setq backtrace-on-error-noninteractive nil)
  (apply #'buttercup-run-markdown-buffer (mapcar #'find-file-noselect
                                                 command-line-args-left)))

;;;###autoload
(defun buttercup-run-markdown-file (file)
  "Run all test suites defined in Markdown FILE.
A suite must be defined within a Markdown \"lisp\" code block."
  (interactive "fMarkdown file: ")
  (buttercup-run-markdown-buffer (find-file-noselect file)))

(eval-when-compile
  ;; Defined below in a dedicated section
  (defvar buttercup-reporter))

(defun buttercup-run (&optional noerror)
  "Run all described suites.
Signal an error if any spec fail or if no suites have been
defined. Signal no errors if NOERROR is non-nil. Return t if all
specs pass, nil if at least one spec fail, and :no-suites if no suites
have been defined."
  (if buttercup-suites
      (buttercup--run-suites buttercup-suites noerror)
    (or (and noerror :no-suites)
        (error "No suites defined"))))

(define-error 'buttercup-run-specs-failed "buttercup-run failed" 'buttercup-error-base)

(defun buttercup--run-suites (suites &optional noerror)
  "Run a list of SUITES.
Signal an error if any spec fail. Signal no error if NOERROR is
non-nil. Return t if all specs pass, nil if at least one spec
fail."
  (funcall buttercup-reporter 'buttercup-started suites)
  (mapc #'buttercup--run-suite suites)
  (funcall buttercup-reporter 'buttercup-done suites)
  (or (zerop (buttercup-suites-total-specs-failed suites))
      (not (or noerror (signal 'buttercup-run-specs-failed '(""))))))

(defvar buttercup--before-each nil
  "A list of functions to call before each spec.

Do not change the global value.")

(defvar buttercup--after-each nil
  "A list of functions to call after each spec.

Do not change the global value.")

(defun buttercup--run-suite (suite)
  "Run SUITE. A suite is a sequence of suites and specs."
  (buttercup--set-start-time suite)
  (let* ((buttercup--before-each (append buttercup--before-each
                                         (buttercup-suite-before-each suite)))
         (buttercup--after-each (append (buttercup-suite-after-each suite)
                                        buttercup--after-each)))
    (funcall buttercup-reporter 'suite-started suite)
    (dolist (f (buttercup-suite-before-all suite))
      (buttercup--update-with-funcall suite f))
    (dolist (sub (buttercup-suite-children suite))
      (cond
       ((buttercup-suite-p sub)
        (buttercup--run-suite sub))
       ((buttercup-spec-p sub)
        (buttercup--run-spec sub))))
    (dolist (f (buttercup-suite-after-all suite))
      (buttercup--update-with-funcall suite f))
    (buttercup--set-end-time suite)
    (funcall buttercup-reporter 'suite-done suite)))

(defun buttercup--run-spec (spec)
  "Run SPEC."
  (buttercup--set-start-time spec)
  (unwind-protect
      (progn
        ;; Kill any previous warning buffer, just in case
        (when (get-buffer buttercup-warning-buffer-name)
          (kill-buffer buttercup-warning-buffer-name))
        (get-buffer-create buttercup-warning-buffer-name)

        (funcall buttercup-reporter 'spec-started spec)
        (buttercup-with-cleanup
         (dolist (f buttercup--before-each)
           (buttercup--update-with-funcall spec f))
         (buttercup--update-with-funcall spec (buttercup-spec-function spec))
         (dolist (f buttercup--after-each)
           (buttercup--update-with-funcall spec f)))
        (funcall buttercup-reporter 'spec-done spec)
        ;; Display warnings that were issued while running the the
        ;; spec, if any
        (with-current-buffer buttercup-warning-buffer-name
          (when (string-match-p "[^[:space:]\n\r]" (buffer-string))
            (buttercup--print
             "%s\n"
             (buttercup-colorize
              ;; Any terminating newline in the buffer should not be
              ;; colorized. It would mess up color handling in Emacs
              ;; compilation buffers using
              ;; `ansi-color-apply-on-region' in
              ;; `compilation-filter-hook'.
              (buffer-substring (point-min)
                                (save-excursion
                                  (goto-char (1- (point-max)))
                                  (if (looking-at-p "\n")
                                      (point) (point-max))))
              'yellow)))))
    (when (get-buffer buttercup-warning-buffer-name)
      (kill-buffer buttercup-warning-buffer-name))
    (buttercup--set-end-time spec)))

(defun buttercup--update-with-funcall (suite-or-spec function &rest args)
  "Update SUITE-OR-SPEC with the result of calling FUNCTION with ARGS.
Sets the `status', `failure-description', and `failure-stack' for
failed and pending specs."
  (let* ((result (apply 'buttercup--funcall function args))
         (status (elt result 0))
         (description (elt result 1))
         (stack (elt result 2)))
    (when (eq status 'failed)
      (pcase description
        (`(error (buttercup-failed . ,failure-description))
         (setq description failure-description))
        (`(error (buttercup-pending . ,pending-description))
         (setq status 'pending
               description pending-description))))
    ;; Only change state when the new state is 'worse' than or same as
    ;; the current state. The constant list is the prioritized list of
    ;; states. The new state is worse if it is in the tail of the
    ;; current state.
    (when (memq status
                (memq (buttercup-suite-or-spec-status suite-or-spec)
                      '(passed pending failed)))
      (setf (buttercup-suite-or-spec-status suite-or-spec) status
            (buttercup-suite-or-spec-failure-description suite-or-spec) description
            (buttercup-suite-or-spec-failure-stack suite-or-spec) stack))))

;;;;;;;;;;;;;
;;; Reporters

(defun buttercup-reporter-adaptive (event arg)
  "A reporter that handles both interactive and noninteractive sessions.

Calls either `buttercup-reporter-batch' or
`buttercup-reporter-interactive', depending.

EVENT and ARG are described in `buttercup-reporter'."
  (if noninteractive
      (buttercup-reporter-batch event arg)
    (buttercup-reporter-interactive event arg)))

(defvar buttercup-reporter-batch--start-time nil
  "The time the last batch report started.")

(defvar buttercup-reporter-batch--failures nil
  "List of failed specs of the current batch report.")

(defvar buttercup-reporter-batch-quiet-statuses nil
  "Do not print results for any spec with any of the listed statuses.")

(defvar buttercup-reporter-batch--suite-stack nil
  "Stack of unprinted suites.")

(defun buttercup-reporter-batch--quiet-spec-p (spec)
  "Return non-nil if the status of SPEC is any of the quiet statuses.
SPEC is considered quiet if its status is listed in
`buttercup-reporter-batch-quiet-statuses'.

Two special statuses can be listed in
`buttercup-reporter-batch-quiet-statuses';
 `skipped': Real spec status `pending' and failure description \"SKIPPED\".
            This matches specs filtered out with `buttercup-mark-skipped'.
 `disabled': Real spec status `pending' and failure description \"PENDING\".
             This matches specs disabled with `xit' or equivalent."
  (or (memq (buttercup-spec-status spec) buttercup-reporter-batch-quiet-statuses)
      ;; check for the virtual status `skipped'
      (and (memq 'skipped buttercup-reporter-batch-quiet-statuses)
           (eq (buttercup-spec-status spec) 'pending)
           (string= (buttercup-spec-failure-description spec) "SKIPPED"))
      ;; check for the virtual status `disabled'
      (and (memq 'disabled buttercup-reporter-batch-quiet-statuses)
           (eq (buttercup-spec-status spec) 'pending)
           (string= (buttercup-spec-failure-description spec) "PENDING"))
      ))

(defun buttercup-reporter-batch (event arg)
  "A reporter that handles batch sessions.

EVENT and ARG are described in `buttercup-reporter'."
  (let ((print-escape-newlines t)
        (print-escape-nonascii t))
    (pcase event
      (`buttercup-started
       (setq buttercup-reporter-batch--start-time (current-time)
             buttercup-reporter-batch--failures nil
             buttercup-reporter-batch--suite-stack nil)
       (let ((defined (buttercup-suites-total-specs-defined arg))
             (pending (buttercup-suites-total-specs-pending arg)))
         (if (> pending 0)
             (buttercup--print "Running %s out of %s specs.\n\n"
                               (- defined pending)
                               defined)
           (buttercup--print "Running %s specs.\n\n" defined))))

      (`suite-started
       (if buttercup-reporter-batch-quiet-statuses
           (push arg buttercup-reporter-batch--suite-stack)
         (buttercup--print "%s\n" (buttercup--indented-description arg))))
      (`spec-started
       (or buttercup-reporter-batch-quiet-statuses
           (and buttercup-color
                (string-match-p "[\n\v\f]" (buttercup-spec-description arg)))
           (buttercup--print "%s" (buttercup--indented-description arg))))
      (`spec-done
       (when (and buttercup-reporter-batch-quiet-statuses
                  (not (buttercup-reporter-batch--quiet-spec-p arg)))
         (dolist (suite (nreverse buttercup-reporter-batch--suite-stack))
           (buttercup--print "%s\n" (buttercup--indented-description suite)))
         (setq buttercup-reporter-batch--suite-stack nil)
         (buttercup--print "%s" (buttercup--indented-description arg)))

       (unless (buttercup-reporter-batch--quiet-spec-p arg)
         (buttercup-reporter-batch--print-spec-done-line arg buttercup-color))

       (when (eq (buttercup-spec-status arg) 'failed)
         (setq buttercup-reporter-batch--failures
               (append buttercup-reporter-batch--failures
                       (list arg)))))

      (`suite-done
       (when (= 0 (length (buttercup-suite-or-spec-parents arg)))
         (if buttercup-reporter-batch-quiet-statuses
             (unless buttercup-reporter-batch--suite-stack
               (buttercup--print "\n"))
           (buttercup--print "\n")))
       (pop buttercup-reporter-batch--suite-stack))

      (`buttercup-done
       (dolist (failed buttercup-reporter-batch--failures)
         (buttercup-reporter-batch--print-failed-spec-report failed buttercup-color))
       (buttercup-reporter-batch--print-summary arg buttercup-color))

      (_
       (error "Unknown event %s" event)))))

(defun buttercup-reporter-batch--print-spec-done-line (spec color)
  "Print the remainder of the SPEC report line for `spec-done'.

If COLOR is non-nil, erace the text so far on the current line
using '\r' and replace it with the same text colored according to
the SPEC status. Do not erase and replace if the text would have
been reprinted with the default color.

Then print the SPEC failure description except if the status is
`passed'. If COLOR is non-nil, print it in the aproprate color
for the spec status.

Finally print the elapsed time for SPEC."
  (let* ((status (buttercup-spec-status spec))
         (failure (buttercup-spec-failure-description spec)))
    ;; Failed specs do typically not have string filure-descriptions.
    ;; In this typical case, use the string "FAILED" for the output.
    (and (eq status 'failed)
         (not (stringp failure))
         (setq failure "FAILED"))
    (unless (memq status '(passed pending failed))
      (error "Unknown spec status %s" status))
    ;; Special status in this function;
    ;;   skipped - a pending spec with failure description "SKIPPED".
    (and (eq status 'pending)
         (equal failure "SKIPPED")
         (setq status 'skipped))
    ;; Use color both as a boolean for erase-and-reprint and the color
    ;; to use.  nil means the default color.
    (setq color (and color (pcase status
                             (`passed 'green)
                             (`pending 'yellow)
                             (`failed 'red)
                             (`skipped nil))))
    (when color
      ;; Carriage returns (\r) should not be colorized. It would mess
      ;; up color handling in Emacs compilation buffers using
      ;; `ansi-color-apply-on-region' in `compilation-filter-hook'.
      (buttercup--print "\r%s"
                        (buttercup-colorize
                         (buttercup--indented-description spec) color)))
    (unless (eq 'passed status)
      (buttercup--print "%s"
                        (buttercup-colorize (concat "  " failure) color)))
    (buttercup--print " (%s)\n" (buttercup-elapsed-time-string spec))))

(cl-defun buttercup-reporter-batch--print-failed-spec-report (failed-spec color)
  "Print a failure report for FAILED-SPEC.

Colorize parts of the output if COLOR is non-nil."
  (when (eq buttercup-stack-frame-style 'omit)
    (cl-return-from buttercup-reporter-batch--print-failed-spec-report))
  (let ((description (buttercup-spec-failure-description failed-spec))
        (stack (buttercup-spec-failure-stack failed-spec))
        (full-name (buttercup-spec-full-name failed-spec)))
    (if color
        (setq full-name (buttercup-colorize full-name 'red)))
    (buttercup--print "%s\n" (make-string 40 ?=))
    (buttercup--print "%s\n" full-name)
    (when stack
      (buttercup--print "\nTraceback (most recent call last):\n")
      (dolist (frame stack)
        (let ((frame-text (buttercup--format-stack-frame frame)))
          (buttercup--print "%s\n" frame-text))))
    (cond
     ((stringp description)
      (buttercup--print "%s: %s\n"
                        (if color
                            (buttercup-colorize "FAILED" 'red)
                          "FAILED")
                        description))
     ((and (consp description) (eq (car description) 'error))
      (buttercup--print "%S: %S\n"
                        (car description)
                        (cadr description)))
     (t
      (buttercup--print "FAILED: %S\n" description)))
    (buttercup--print "\n")))

(defun buttercup-reporter-batch--print-summary (suites color)
  "Print a summary of the reults of SUITES.

Colorize parts of the output if COLOR is non-nil."
  (let* ((defined (buttercup-suites-total-specs-defined suites))
         (pending (buttercup-suites-total-specs-pending suites))
         (failed (buttercup-suites-total-specs-failed suites))
         (duration (seconds-to-string
                    (float-time
                     (time-subtract (current-time)
                                    buttercup-reporter-batch--start-time))))
         (out-of (if (zerop pending) "" (format " out of %d" defined)))
         (failed-str (format "%d failed" failed)))
    (if color
        (setq failed-str (buttercup-colorize failed-str (if (zerop failed) 'green 'red))))
    (buttercup--print
     "Ran %d%s specs, %s, in %s.\n"
     (- defined pending) out-of failed-str duration)))

(defun buttercup--print (fmt &rest args)
  "Format a string and send it to terminal without alteration.

FMT and ARGS are passed to `format'."
  (send-string-to-terminal (apply #'format fmt args)))


(defadvice display-warning (around buttercup-defer-warnings activate)
  "Log all warnings to a special buffer while running buttercup specs.

Emacs' normal display logic for warnings doesn't mix well with
buttercup, for several reasons. So instead, while a buttercup
test is running, BUFFER-NAME defaults to a special buffer that
exists only during the test (see
`buttercup-warning-buffer-name'). When logging to this buffer,
`warning-minimum-level' is set to `:emergency' and the `message'
function is disabled to suppress display of all warning messages.
The contents of this buffer are then displayed after the test
finishes."
  (when (and (null buffer-name)
             buttercup-warning-buffer-name
             (get-buffer buttercup-warning-buffer-name))
    (setq buffer-name buttercup-warning-buffer-name))
  (if (equal buffer-name buttercup-warning-buffer-name)
      (cl-letf
          ((warning-minimum-level :emergency)
           ((symbol-function 'message) 'ignore))
        ad-do-it)
    ad-do-it))

(defconst buttercup-colors
  '((black   . 30)
    (red     . 31)
    (green   . 32)
    (yellow  . 33)
    (blue    . 34)
    (magenta . 35)
    (cyan    . 36)
    (white   . 37))
  "List of text colors.")

(defmacro buttercup-suppress-warning-capture (&rest body)
  "Suppress Buttercup's warning capturing within BODY.

Buttercup normally captures all warnings while a test is running
so it can defer displaying them until after the test is complete.
However, if you want to catch any warnings yourself as part of
the test, you need to wrap your code in this macro to suppress
the capturing behavior."
  (declare (indent 0))
  `(let ((buttercup-warning-buffer-name nil))
     ,@body))

(defun buttercup-colorize (string color)
  "Format STRING with COLOR.
Return STRING unmodified if COLOR is nil."
  (if color
      (let ((color-code (cdr (assoc color buttercup-colors))))
        (format "\e[%sm%s\e[0m" color-code string))
    string))

(defun buttercup-reporter-interactive (event arg)
  "Reporter for interactive sessions.

EVENT and ARG are described in `buttercup-reporter'."
  ;; This is a bit rudimentary ...
  (with-current-buffer (get-buffer-create "*Buttercup*")
    (let ((old-print (symbol-function 'buttercup--print))
          (buf (current-buffer))
          (inhibit-read-only t))
      (when (eq event 'buttercup-started)
        (erase-buffer)
        (special-mode)
        (display-buffer (current-buffer)))
      (fset 'buttercup--print (lambda (fmt &rest args)
                                (with-current-buffer buf
                                  (let ((inhibit-read-only t))
                                    (goto-char (point-max))
                                    (insert (apply 'format fmt args))))))
      (unwind-protect
          (buttercup-reporter-batch event arg)
        (fset 'buttercup--print old-print)))
    (let ((w (get-buffer-window (current-buffer))))
      (when w
        (with-selected-window w
          (goto-char (point-max)))))))

;;;;;;;;;;;;;
;;; Utilities

(defun buttercup--funcall (function &rest arguments)
  "Call FUNCTION with ARGUMENTS.

Returns a list of three values. The first is the state:

passed -- The second value is the return value of the function
  call, the third is nil.

failed -- The second value is the description of the expectation
  which failed or the error, the third is the backtrace or nil."
  (catch 'buttercup-debugger-continue
    (let ((debugger #'buttercup--debugger)
          (debug-on-error t)
          (debug-ignored-errors nil))
      (list 'passed
            (apply function arguments)
            nil))))

(defun buttercup--debugger (&rest args)
  "Debugger function that return error context with an exception.

ARGS according to `debugger'."
  ;; If we do not do this, Emacs will not run this handler on
  ;; subsequent calls. Thanks to ert for this.
  (setq num-nonmacro-input-events (1+ num-nonmacro-input-events))
  (throw 'buttercup-debugger-continue
         (list 'failed args
               (cl-destructuring-bind (_ (signal-type . data)) args
                 (unless (eq signal-type 'buttercup-pending)
                   (buttercup--backtrace))))))

(defalias 'buttercup--mark-stackframe 'ignore
  "Marker to find where the backtrace start.")

(defun buttercup--backtrace ()
  "Create a backtrace, a list of frames returned from `backtrace-frame'."
  ;; Read the backtrace frames from 0 (the closest) upward.
  (cl-do* ((n 0 (1+ n))
           (frame (backtrace-frame n) (backtrace-frame n))
           (frame-list nil)
           (in-program-stack nil))
      ((not frame) frame-list)
      ;; discard frames until (and including) `buttercup--debugger', they
      ;; only contain buttercup code
      (when in-program-stack
        (push frame frame-list))
      (when (eq (elt frame 1)
                'buttercup--debugger)
        (setq in-program-stack t))
      ;; keep frames until one of the known functions are found, after
      ;; this is just the buttercup framework and not interesting for
      ;; users incorrect for testing buttercup. Some frames before the
      ;; function also have to be discarded
      (cl-labels ((tree-find (key tree)
                             (cl-block tree-find
                               (while (consp tree)
                                 (let ((elem (pop tree)))
                                   (when (or (and (consp elem)
                                                  (tree-find key elem))
                                             (eql key elem))
                                     (cl-return-from tree-find t))))
                               (cl-return-from tree-find
                                 (and tree (eql tree key))))))
        (when (and in-program-stack (tree-find 'buttercup--mark-stackframe frame))
          (pop frame-list)
          (cl-return frame-list)))))

(defun buttercup--format-stack-frame (frame &optional style)
  "Format stack FRAME according to STYLE.
STYLE can be one of `full', `crop', `pretty', or `omit'.
If STYLE is nil, use `buttercup-stack-frame-style' or `crop'."
  (setq style (or style buttercup-stack-frame-style 'crop))
  (pcase style
    (`omit) ; needed to verify valid styles
    (`full
     (if (car frame)
         (format "  %S%s" (cadr frame) (if (cddr frame) (prin1-to-string (cddr frame)) "()"))
       (format "  %S" (cdr frame))))
    (`crop
     (let ((line (buttercup--format-stack-frame frame 'full)))
       ;; Note: this could be done sith `s-truncate' from the s
       ;; package
       (when (> (length line) 79)
         (setq line (concat (substring line 0 76)
                            "...")))
       line))
    (`pretty
     (let ((text (pp-to-string (cdr frame))))
       ;; Delete empty trailing line
       (setq text
             (replace-regexp-in-string
              "\n[[:space:]]*\\'" ""
              text))
       ;; Indent 2 spaces
       (setq text
             (replace-regexp-in-string
              "^" "  "
              text))
       ;; Prefix first line with lambda for function call and M for
       ;; macro/special form
       (setq text
             (replace-regexp-in-string
              "\\` " (if (car frame) "λ" "M")
              text))))
    (_ (error "Unknown stack trace style: %S" style))))

(defmacro buttercup-with-converted-ert-signals (&rest body)
  "Convert ERT signals to buttercup signals in BODY.

Specifically, `ert-test-failed' is converted to
`buttercup-failed' and `ert-test-skipped' is converted to
`buttercup-pending'."
  (declare (indent 0))
  `(condition-case err
       (progn ,@body)
     (ert-test-failed
      (buttercup-fail "%S" err))
     (ert-test-skipped
      (buttercup-skip "Skipping: %S" err))))

;;;###autoload
(define-minor-mode buttercup-minor-mode
  "Activate buttercup minor mode.

With buttercup minor mode active the following is activated:

- `describe' and `it' forms are fontified with
  `font-lock-keyword-face'.
- `describe' and `it' forms are available from `imenu' for
  quicker access."
  :lighter " ❀"
  (let ((font-lock-form '(("(\\(describe\\|buttercup-define-matcher\\|it\\) "
                           1 'font-lock-keyword-face)))
        (imenu-forms '(("Test Suites" "\\((describe\\_> +\\)\"\\(\\_<.+\\_>\\)\"" 2)
                       ("Spec" "\\((it\\_> +\\)\"\\(\\_<.+\\_>\\)\"" 2))))
    (if buttercup-minor-mode
        (progn
          (font-lock-add-keywords nil font-lock-form)
          (cl-dolist (form imenu-forms)
            (add-to-list 'imenu-generic-expression form)))
      (font-lock-remove-keywords nil font-lock-form)
      (cl-dolist (form imenu-forms)
        (setq imenu-generic-expression (delete form imenu-generic-expression))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Signal errors when files have to be recompiled

(defun buttercup-check-for-stale-elc (elc-file)
  "Raise an error when ELC-FILE is an elc-file and older than its el-file."
  (when (string= (file-name-extension elc-file) "elc")
    (let ((el-file (substring elc-file 0 -1)))
      (when (and (file-exists-p el-file)
                 (time-less-p
                  (file-attribute-modification-time (file-attributes elc-file))
                  (file-attribute-modification-time (file-attributes el-file))))
        (error "%s is newer than %s" el-file elc-file)))))

(defun buttercup-error-on-stale-elc (&optional arg)
  "Activate errors when an stale (older than .el) .elc-file is loaded.

Enable the functionality if ARG is omitted or nil, toggle it if
ARG is ‘toggle’; disable otherwise."
  (cond ((null arg)
         (add-hook 'after-load-functions #'buttercup-check-for-stale-elc))
        ((eq arg 'toggle)
         (if (memq 'buttercup-check-for-stale-elc after-load-functions)
             (remove-hook 'after-load-functions #'buttercup-check-for-stale-elc)
           (add-hook 'after-load-functions #'buttercup-check-for-stale-elc)))
        (t (remove-hook 'after-load-functions #'buttercup-check-for-stale-elc))))

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:
(provide 'buttercup)
;;; buttercup.el ends here
