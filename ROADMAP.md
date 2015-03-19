# Version 1.0: Jasmine’s introduction.html

I will declare buttercup ready to be used once it implements most of
the stuff in
[Jasmine’s introduction](https://jasmine.github.io/edge/introduction.html).
At this time, this is missing:

## Setup and Teardown

The forms `(before-each body ...)` and `(after-each body ...)`
(likewise, `before-all` and `after-all`) to add functions to the
current suite. Test runners will then call these in turn for each spec
being run.

## Test Runners

This would also be a great time to write useful test runners. For the
first release, there should be `buttercup-run-discover`,
`buttercup-run-markdown`, and `buttercup-run-at-point`.

## Suite Execution

All of those can use the same `buttercup-run` function, which should
run a list of suites and call a reporter with results. All execution
should happen with `debug-on-error` set. We’ll deal with backtraces
later.

## Spies

It’s only sensible for Emacs Lisp to spy on functions, so that’s what
we should support. The best way of tracking the calls for a function
is likely a key-weak hash table of function objects to a call list.

Of the following, `:and-call-fake` is probably going to be the
primitive version. The basic stub would record any arguments in the
hash table.

Example code:

```Lisp
(spy-on 'function-name)
(spy-on 'function-name :and-call-through)
(spy-on 'function-name :and-return-value 23)
(spy-on 'function-name :and-call-fake function)
(spy-on 'function-name :and-throw-error 'arith-error)

(expect 'spied-function :to-have-been-called)
(expect 'spied-function :to-have-been-called-with 1 2 3)
(spy-calls-any 'spied-function)
(spy-calls-count 'spied-function)
(spy-calls-args-for 'spied-function index)
(spy-calls-all-args 'spied-function)
(spy-calls-most-recent 'spied-function)
(spy-calls-first 'spied-function)
(spy-calls-reset 'spied-function)
```

# Version 1.1: The Missing Features

## Return of the Backtrace

Suite execution should catch errors and include a backtrace in the
result.

## Reporter

A reporter is a function that is called with various events during the
execution of a test run. See
[the Jasmine documentation](https://jasmine.github.io/edge/custom_reporter.html)
for a list of useful events.

By default, I’d like to have a `buttercup-reporter-batch` and
`buttercup-reporter-interactive`.

## Disabling Suites and Pending Specs

The `xdescribe`/`xit` macros and associated functions. A possible
initial implementation would be just noops. We’ll deal with them fully
in 1.2.

# Version 1.2: Nice to Have

## Disabled Suites and Pending Specs, again

The `xdescribe`/`xit` macros should get their respective entries
reported as actually disabled/pending, not just ignored.

## defspy

Syntactic sugar for `:and-call-fake`:

```Lisp
(defspy function-name (args ...)
  body...
  (spy-original 1 2)
  ...)
```
