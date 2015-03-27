# Version 1.1: The Missing Features

## Pending Specs

The following phrases were left out from `introduction.js`, and should
be implemented still:

> Pending specs do not run, _but their names will show up in the results as pending._

> And if you call the function `pending` anywhere in the spec body, no matter the expectations, the spec will be marked pending. A string passed to pending will be treated as a reason and displayed when the suite finishes.

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
