# Buttercup — Behavior-Driven Emacs Lisp Testing

[![Build and test](https://github.com/jorgenschaefer/emacs-buttercup/workflows/Build%20and%20test/badge.svg)](https://github.com/jorgenschaefer/emacs-buttercup/actions?query=workflow%3A%22Build+and+test%22+branch%3Amaster)
[![MELPA Stable](http://stable.melpa.org/packages/buttercup-badge.svg)](http://stable.melpa.org/#/buttercup)

![Ranculus repens, photo by sannse](docs/images/buttercup.jpg)

Buttercup is a behavior-driven development framework for testing Emacs
Lisp code. It allows to group related tests so they can share common
set-up and tear-down code, and allows the programmer to “spy” on
functions to ensure they are called with the right arguments during
testing.

The framework is heavily inspired by
[Jasmine](https://jasmine.github.io/edge/introduction.html).

## Example

*Full article: [Writing Tests](docs/writing-tests.md)*

A simple test looks like this.

```Lisp
(describe "A suite"
  (it "contains a spec with an expectation"
    (expect t :to-be t)))
```

## Installation and Usage

*Full article: [Running Tests](docs/running-tests.md)*

You can install buttercup from
[MELPA Stable](http://stable.melpa.org/). Add the following to your
`init.el` or `.emacs` file:

```
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
```

This should allow you to `M-x package-install RET buttercup RET`.

Alternatively, users of Debian 9 or later or Ubuntu 16.10 or later may
simply `apt-get install elpa-buttercup`.

Now create a file called `test-feature.el` with these contents:

```Lisp
(describe "A suite"
  (it "contains a spec with an expectation"
    (expect t :to-be t)))
```

You can now use buttercup to run this test:

```
$ emacs -batch -f package-initialize -L . -f buttercup-run-discover
Running 1 specs.

A suite
  contains a spec with an expectation

Ran 1 specs, 0 failed, in 0.0 seconds.
```

Congratulations, you ran your first test!

## Feature List

- Shared set-up and tear-down sections to reduce code repetition and
  share a common environment among tests.
- Easy to read and extensible `expect` macro to describe expected
  behavior.
- Powerful mocking framework, called “spies,” to both cause them to
  return expected values or throw errors as needed by the test, as
  well as to ensure functions are called with expected arguments
  during tests.
- Built to be run within a Continuous Integration environment,
  including test runners to discover and execute tests in a directory
  tree.

### Why not ERT?

Emacs comes with a testing framework,
[ERT](https://www.gnu.org/software/emacs/manual/html_mono/ert.html).
Buttercup was written to address some shortcomings of that framework.

- ERT
  [deliberately leaves it up to the programmer to define set-up and tear-down code](https://www.gnu.org/software/emacs/manual/html_mono/ert.html#Fixtures-and-Test-Suites),
  which requires a lot of boiler-plate code for every set-up function.
  Buttercup makes this easy and seamless.
- ERT has no good way of being run in a continuous integration
  environment. There are
  [external projects to make this less of a pain](https://github.com/rejeep/ert-runner.el)
  instead. Once all is said and done, you installed six external
  packages your project does not need just to run your own tests. And
  that does not include a mocking library.
- ERT has no way of grouping related tests, requiring every test name
  to share the same prefix, making names long and hard to read.

Nonetheless, ERT is a great project. It introduced testing to Emacs,
and Buttercup learned a lot from its code to record a stack trace for
error display. Even though Buttercup tries to be a better testing
framework than ERT, we do wish ERT and the ERT maintainers all the
best and hope both frameworks can continue to benefit from each other.
