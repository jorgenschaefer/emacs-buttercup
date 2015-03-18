# Buttercup — Behavior-Driven Emacs Lisp Testing

[![Build Status](https://api.travis-ci.org/jorgenschaefer/emacs-buttercup.png?branch=master)](https://travis-ci.org/jorgenschaefer/emacs-buttercup)

Buttercup is a behavior-driven development framework for testing Emacs
Lisp code. It is heavily inspired by
[Jasmine](https://jasmine.github.io/). So heavily inspired, in fact,
that half this page is more or less a verbatim copy of the
[Jasmine introduction](https://jasmine.github.io/edge/introduction.html).

All code in this file can be run by Buttercup’s built-in markdown test
runner. Just use `make test` in the project directory to see the
output.

## Suites: `describe` Your Tests

A test suite begins with a call to the Buttercup macro `describe` with
the first parameter describing the suite and the rest being the body
of code that implements the suite.

```Lisp
(describe "A suite"
  (it "contains a spec with an expectation"
    (expect t :to-be t)))
```

## Specs

Specs are defined by calling the Buttercup macro `it`, which, like
`describe` takes a string and code. The string is the title of the
spec and the code is the spec, or test. A spec contains one or more
expectations that test the state of the code. An expectation in
Buttercup is an assertion that is either true or false. A spec with
all true expectations is a passing spec. A spec with one or more false
expectations is a failing spec.

### It’s Just Functions

The code arguments to `describe` and `it` is just turned into
functions internally, so they can contain any executable code
necessary to implement the rules. Emacs Lisp scoping rules apply, so
make sure to define your spec file to be lexically scoped.

```Lisp
(describe "A suite is just a function"
  (let ((a nil))
    (it "and so is a spec"
      (setq a t)
      (expect a :to-be t))))
```

## Expectations

Expectations are expressed with the `expect` function. Its first
argument is the actual value. The second argument is a test, followed
by expected values for the test to compare the actual value against.

If there is no test, the argument is simply tested for being non-nil.
This can be used by people who dislike the matcher syntax.

### Matchers

Each matcher implements a boolean comparison between the actual value
and the expected value. It is responsible for reporting to Buttercup
if the expectation is true or false. Buttercup will then pass or fail
the spec.

Any matcher can evaluate to a negative assertion by prepending it with
the `:not` matcher.

```Lisp
(describe "The :to-be matcher compares with `eq'"
  (it "and has a positive case"
    (expect t :to-be t))
  (it "and can have a negative case"
    (expect nil :not :to-be t)))
```

### Included Matchers

Buttercup has a rich set of matchers included. Each is used here — all
expectations and specs pass. There is also the ability to write custom
matchers (see the `buttercup-define-matcher` macro for further
information) for when a project’s domain calls for specific assertions
that are not included below.

```Lisp
(describe "Included matchers:"
  (it "The :to-be matcher compares with `eq'"
    (let* ((a 12)
           (b a))
      (expect a :to-be b)
      (expect a :not :to-be nil)))

  (describe "The :to-equal matcher"
    (it "works for simple literals and variables"
      (let ((a 12))
        (expect a :to-equal 12)))

    (it "should work for compound objects"
      (let ((foo '((a . 12) (b . 34)))
            (bar '((a . 12) (b . 34))))
        (expect foo :to-equal bar))))

  (it "The :to-match matcher is for regular expressions"
    (let ((message "foo bar baz"))
      (expect message :to-match "bar")
      (expect message :to-match (rx "bar"))
      (expect message :not :to-match "quux")))

  (it "The :to-be-truthy matcher is for boolean casting testing"
    (let (a
          (foo "foo"))
      (expect foo :to-be-truthy)
      (expect a :not :to-be-truthy)))

  (it "The :to-contain matcher is for finding an item in a list"
    (let ((a '("foo" "bar" "baz")))
      (expect a :to-contain "bar")
      (expect a :not :to-contain "quux")))

  (it "The :to-be-less-than matcher is for mathematical comparisons"
    (let ((pi 3.1415926)
          (e 2.78))
      (expect e :to-be-less-than pi)
      (expect pi :not :to-be-less-than e)))

  (it "The :to-be-greater-than matcher is for mathematical comparisons"
    (let ((pi 3.1415926)
          (e 2.78))
      (expect pi :to-be-greater-than e)
      (expect e :not :to-be-greater-than pi)))

  (it "The :to-be-close-to matcher is for precision math comparison"
    (let ((pi 3.1415926)
          (e 2.78))
      (expect pi :not :to-be-close-to e 2)
      (expect pi :to-be-close-to e 0)))

  (describe "The :to-throw matcher"
    (it "is for testing if a function throws an exception"
      (let ((foo (lambda () (+ 1 2)))
            (bar (lambda () (+ a 1))))
        (expect foo :not :to-throw)
        (expect bar :to-throw)))
    (it "accepts a symbol to check for the signal thrown"
      (let ((foo (lambda () (/ 1 0)))
            (bar (lambda () (+ a 1))))
        (expect foo :not :to-throw 'void-variable)
        (expect bar :to-throw 'void-variable)))
    (it "optionally matches arguments to signals"
      (let ((foo (lambda () (+ a 1)))
            (bar (lambda () (+ a 1))))
        (expect foo :not :to-throw 'void-variable '(b))
        (expect bar :to-throw 'void-variable '(a))))))
```

## Spies

Buttercup provides a way of _spying_ on a function, something usually
called mocking, but Jasmine calls it _spies_, and so do we. Did I
mention Buttercup is heavily inspired by Jasmine?

## Test Runners

Evaluating `describe` forms just stores the suites. You need to use a
test runner to actually evaluate them. Buttercup comes with two test
runners by default:

- `buttercup-run-suite-at-point` — Evaluate the topmost `describe`
  form at point and run the suite it creates directly. Useful for
  interactive development. But be careful, this uses your current
  environment, which might not be clean (due to said interactive
  development).
- `buttercup-discover` — Find files in directories specified on the
  command line, load them, and then run all suites defined therein.
  Useful for being run in batch mode.
- `buttercup-markdown-runner` — Run code in markdown files. Used to
  run this file’s code.
