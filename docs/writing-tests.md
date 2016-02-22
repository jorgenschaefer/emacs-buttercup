# Introduction

Buttercup is a behavior-driven development framework for testing Emacs
Lisp code. It does not depend on any other Emacs Lisp libraries. It
has a clean, obvious syntax so that you can easily write tests.

It is heavily inspired by [Jasmine](https://jasmine.github.io/). So
heavily inspired, in fact, that most of this page is more or less a
verbatim copy of the
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

## Grouping Related Specs with `describe`

The `describe` macro is for grouping related specs. The string
parameter is for naming the collection of specs, and will be
concatenated with specs to make a spec’s full name. This aids in
finding specs in a large suite. If you name them well, your specs read
as full sentences in traditional
[BDD](http://en.wikipedia.org/wiki/Behavior-driven_development) style.

```Lisp
(describe "A spec"
  (it "is just a function, so it can contain any code"
    (let ((foo 0))
      (setq foo (1+ foo))

      (expect foo :to-equal 1)))

  (it "can have more than one expectation"
    (let ((foo 0))
      (setq foo (1+ foo))

      (expect foo :to-equal 1)
      (expect t :to-equal t))))
```

### Setup and Teardown

To help a test suite DRY up any duplicated setup and teardown code,
Buttercup provides the `before-each`, `after-each`, `before-all` and
`after-all` special forms.

As the name implies, code blocks defined with `before-each` are called
once before each spec in the `describe` is run, and the `after-each`
code blocks are called once after each spec.

Here is the same set of specs written a little differently. The
variable under test is defined at the top-level scope — the `describe`
block — and initialization code is moved into a `before-each` block.
The `after-each` block resets the variable before continuing.

```Lisp
(describe "A spec using `before-each' and `after-each'"
  (let ((foo 0))
    (before-each
     (setq foo (1+ foo)))

    (after-each
     (setq foo 0))

    (it "is just a function, so it can contain any code"
      (expect foo :to-equal 1))

    (it "can have more than one expectation"
      (expect foo :to-equal 1)
      (expect t :to-equal t))))
```

The `before-all` form is called only once before all the specs in
`describe` are run, and the `after-all` form is called after all specs
finish. These functions can be used to speed up test suites with
expensive setup and teardown.

However, be careful using `before-all` and `after-all`! Since they are
not reset between specs, it is easy to accidentally leak state between
your specs so that they erroneously pass or fail.

```Lisp
(describe "A spec using `before-all' and `after-all'"
  (let (foo)
    (before-all
     (setq foo 1))

    (after-all
     (setq foo 0))

    (it "sets the initial value of foo before specs run"
      (expect foo :to-equal 1)
      (setq foo (1+ foo)))

    (it "does not reset foo between specs"
      (expect foo :to-equal 2))))
```

### Nesting `describe` Blocks

Calls to `describe` can be nested, with specs defined at any level.
This allows a suite to be composed as a tree of functions. Before a
spec is executed, Buttercup walks down the tree executing each
`before-each` function in order. After the spec is executed, Buttercup
walks through the `after-each` functions similarly.

```Lisp
(describe "A spec"
  (let (foo)
    (before-each
     (setq foo 0)
     (setq foo (1+ foo)))

    (after-each
     (setq foo 0))

    (it "is just a function, so it can contain any code"
      (expect foo :to-equal 1))

    (it "can have more than one expectation"
      (expect foo :to-equal 1)
      (expect t :to-equal t))

    (describe "nested inside a second describe"
      (let (bar)
        (before-each
         (setq bar 1))

        (it "can reference both scopes as needed"
          (expect foo :to-equal bar))))))
```

## Disabling Suites

Suites and specs can be disabled with the `xdescribe` and `xit`
macros, respectively. These suites and any specs inside them are
skipped when run and thus their results will not appear in the
results.

```Lisp
(xdescribe "A spec"
  (let (foo)
    (before-each
      (setq foo 0)
      (setq foo (1+ foo)))

    (it "is just a function, so it can contain any code"
      (expect foo :to-equal 1))))
```

## Pending Specs

Pending specs do not run.

Any spec declared with `xit` is marked as pending.

Any spec declared without a function body will also be marked as
pending in results.

```Lisp
(describe "Pending specs"
  (xit "can be declared using `xit'"
    (expect t :to-be nil))

  (it "can be declared with `it' but without a body"))
```

## Spies

Buttercup has test double functions called spies. While other
frameworks call these mocks and similar, we call them spies, because
their main job is to spy in on function calls. Also, Jasmine calls
them spies, and so do we. A spy can stub any function and tracks calls
to it and all arguments. A spy only exists in the `describe` or `it`
block it is defined in, and will be removed after each spec. There are
special matchers for interacting with spies. The
`:to-have-been-called` matcher will return true if the spy was called
at all. The `:to-have-been-called-with` matcher will return true if
the argument list matches any of the recorded calls to the spy.

```Lisp
(describe "A spy"
  (let (foo bar)
    (before-each
     (setf (symbol-function 'foo)
           (lambda (value)
             (setq bar value)))

     (spy-on 'foo)

     (foo 123)
     (foo 456 "another param"))

    (it "tracks that the spy was called"
      (expect 'foo :to-have-been-called))

    (it "tracks all arguments of its calls"
      (expect 'foo :to-have-been-called-with 123)
      (expect 'foo :to-have-been-called-with 456 "another param"))

    (it "stops all execution on a function"
      (expect bar :to-be nil))))
```

### Spies: `:and-call-through`

The keyword argument `:and-call-through` to `spy-on` will make the spy
call the original function instead of returning `nil`.

```Lisp
(describe "A spy, when configured to call through"
  (let (bar set-bar get-bar fetched-bar)
    (before-each
      (fset 'set-bar (lambda (val)
                       (setq bar val)))
      (fset 'get-bar (lambda ()
                       bar))

      (spy-on 'get-bar :and-call-through)

      (set-bar 123)
      (setq fetched-bar (get-bar)))

    (it "tracks that the spy was called"
      (expect 'get-bar :to-have-been-called))

    (it "should not affect other functions"
      (expect bar :to-equal 123))

    (it "when called returns the requested value"
      (expect fetched-bar :to-equal 123))))
```

### Spies: `:and-return-value`

The keyword argument `:and-return-value` specifies the value the
spied-on function should return.

```Lisp
(describe "A spy, when configured to fake a return value"
  (let (bar set-bar get-bar fetched-bar)
    (before-each
      (fset 'set-bar (lambda (val)
                       (setq bar val)))
      (fset 'get-bar (lambda ()
                       bar))

      (spy-on 'get-bar :and-return-value 745)

      (set-bar 123)
      (setq fetched-bar (get-bar)))

    (it "tracks that the spy was called"
      (expect 'get-bar :to-have-been-called))

    (it "should not affect other functions"
      (expect bar :to-equal 123))

    (it "when called returns the requested value"
      (expect fetched-bar :to-equal 745))))
```

### Spies: `:and-call-fake`

The keyword argument `:and-call-fake` delegates calls to a supplied
function.

```Lisp
(describe "A spy, when configured with an alternate implementation"
  (let (bar set-bar get-bar fetched-bar)
    (before-each
      (fset 'set-bar (lambda (val)
                       (setq bar val)))
      (fset 'get-bar (lambda ()
                       bar))

      (spy-on 'get-bar :and-call-fake (lambda () 1001))

      (set-bar 123)
      (setq fetched-bar (get-bar)))

    (it "tracks that the spy was called"
      (expect 'get-bar :to-have-been-called))

    (it "should not affect other functions"
      (expect bar :to-equal 123))

    (it "when called returns the requested value"
      (expect fetched-bar :to-equal 1001))))
```

### Spies: `:and-throw-error`

With the keyword argument `:and-throw-error`, all calls to the spy
will `signal` the specified value as an error.

```Lisp
(describe "A spy, when configured to throw an error"
  (let (bar set-bar get-bar fetched-bar)
    (before-each
      (fset 'set-bar (lambda (val)
                       (setq bar val)))
      (fset 'get-bar (lambda ()
                       bar))

      (spy-on 'get-bar :and-throw-error 'error))

    (it "throws the error"
      (expect (lambda () (get-bar))
              :to-throw 'error))))
```

### Other tracking properties

Every call to a spy is tracked and exposed using the `spy-calls`
accessor.

`spy-calls-any` returns `nil` if the spy has not been called at all,
and then `t` once at least one call happens. `spy-calls-count` returns
the number of times the spy was called. `spy-calls-args-for` returns
the arguments passed to a given call (by index). `spy-calls-all-args`
returns the arguments to all calls. `spy-calls-all` returns the
current buffer and arguments passed to all calls.
`spy-calls-most-recent` returns the current buffer and arguments for
the most recent call. `spy-calls-first` returns the current buffer and
arguments for the first call.

Finally, `spy-calls-reset` clears all tracking for a spy.

```Lisp
(describe "A spy"
  (let (set-foo foo)
    (before-each
      (fset 'set-foo (lambda (val &rest ignored)
                       (setq foo val)))
      (spy-on 'set-foo))

    (it "tracks if it was called at all"
      (expect (spy-calls-any 'set-foo)
              :to-equal
              nil)

      (set-foo 5)

      (expect (spy-calls-any 'set-foo)
              :to-equal
              t))

    (it "tracks the number of times it was called"
      (expect (spy-calls-count 'set-foo)
              :to-equal
              0)

      (set-foo 2)
      (set-foo 3)

      (expect (spy-calls-count 'set-foo)
              :to-equal
              2))

    (it "tracks the arguments of each call"
      (set-foo 123)
      (set-foo 456 "baz")

      (expect (spy-calls-args-for 'set-foo 0)
              :to-equal
              '(123))

      (expect (spy-calls-args-for 'set-foo 1)
              :to-equal
              '(456 "baz")))

    (it "tracks the arguments of all calls"
      (set-foo 123)
      (set-foo 456 "baz")

      (expect (spy-calls-all-args 'set-foo)
              :to-equal
              '((123)
                (456 "baz"))))

    (it "can provide the context and arguments to all calls"
      (set-foo 123)

      (expect (spy-calls-all 'set-foo)
              :to-equal
              `(,(make-spy-context :current-buffer (current-buffer)
                                   :args '(123)
                                   :return-value nil))))

    (it "has a shortcut to the most recent call"
      (set-foo 123)
      (set-foo 456 "baz")

      (expect (spy-calls-most-recent 'set-foo)
              :to-equal
              (make-spy-context :current-buffer (current-buffer)
                                :args '(456 "baz")
                                :return-value nil)))

    (it "has a shortcut to the first call"
      (set-foo 123)
      (set-foo 456 "baz")

      (expect (spy-calls-first 'set-foo)
              :to-equal
              (make-spy-context :current-buffer (current-buffer)
                                :args '(123)
                                :return-value nil)))

    (it "can be reset"
      (set-foo 123)
      (set-foo 456 "baz")

      (expect (spy-calls-any 'set-foo)
              :to-be
              t)

      (spy-calls-reset 'set-foo)

      (expect (spy-calls-any 'set-foo)
              :to-be
              nil))))
```
