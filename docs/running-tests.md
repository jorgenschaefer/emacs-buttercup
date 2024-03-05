# Usage

Buttercup is primarily meant to be used non-interactively, to
automatically test a project independent of a user’s setup, before a
commit and on a continuous integration platform. Because of this, the
recommended workflow for buttercup does not use interactive commands
but instead the command line.

## Cask

[Cask](https://github.com/cask/cask) is a project to create an
environment separate from your usual interactive Emacs environment.
This allows you to install the packages your project depends on, and
only those, and test your project in a well-defined environment.

Buttercup works best in such environments, so the following best
practices rely on Cask to be installed.

### Project Directory Layout

A basic project layout requires a project file, called `feature.el`
here, a `Cask` file to define dependencies, and a `tests/` directory
for tests. It should look roughly like this:

```
feature/feature.el
        Cask
        tests/test-feature.el
```

**feature.el**

```elisp
(defun featurize (bug feature)
  (format "It's not a %s, it's a %s" bug feature))

(provide 'feature)
```

**Cask**

```elisp
(source gnu)
(source melpa-stable)

(development
 (depends-on "buttercup"))
```

**tests/test-feature.el**

```elisp
;;; -*- lexical-binding: t; -*-
(require 'feature)

(describe "The feature"
  (it "can use bug and feature"
    (expect (featurize "bug" "feature")
            :to-equal
            "It's not a bug, it's a feature")))
```

### Running Tests

You can now use Cask to run your tests.

First, you have to install the dependencies. You only have to do this
once, or when the dependencies change:

```console
$ cask
Extracting buttercup-1.1/
Extracting buttercup-1.1/buttercup-compat.el
Extracting buttercup-1.1/buttercup.el
Extracting buttercup-1.1/bin/
Extracting buttercup-1.1/bin/buttercup
Extracting buttercup-1.1/buttercup-pkg.el
Generating autoloads for buttercup-compat.el...
Generating autoloads for buttercup-compat.el...done
Generating autoloads for buttercup-pkg.el...
Generating autoloads for buttercup-pkg.el...done
Generating autoloads for buttercup.el...
Generating autoloads for buttercup.el...done
```

Now, you can run your tests:

```console
$ cask exec buttercup -L .
Running 1 specs.

The feature
  can use bug and feature

Ran 1 specs, 0 failed, in 0.0 seconds.
```

That’s it. Buttercup’s built-in discover test runner looks for files
named `test-*.el`, `*-test.el` or `*-tests.el`.

Use the `--pattern PATTERN` option to only Only run tests with names
matching PATTERN. The `--pattern` option can be used multiple times,
in which case tests will be run if they match any of the given
patterns. Combine with the `--no-skip` option to filter out the
skipped tests.

You can run this command whichever way you like. Common choices
include a makefile or shell scripts.

## Eldev

[Eldev](https://github.com/doublep/eldev) is another Elisp build tool
and has built-in supports for Buttercup.  Like Cask, it installs
dependencies of your project automatically, ensuring consistent
environment.

### Project Directory Layout

Typical project layout used for Cask also works for Eldev.  However,
the tool is very liberal and with a couple of lines you can configure
it to handle almost any layout a project could have (see [its
documentation](https://github.com/doublep/eldev#testing) if needed).

Unlike Cask, however, Eldev needs that your test files include form
`(require 'buttercup)`.  Otherwise you will get Elisp errors like
“Symbol’s function definition is void: describe”.  Basically, Eldev
requires that the `.el` files are *self-contained* and do not depend
on certain external tool.

### Running Tests

The tool installs project dependencies automatically when needed, so
you don’t have to bother about that.  All you need to do is to ensure
the main file of the project correctly declares them.

You also don’t need to declare that the project uses Buttercup for
testing: Eldev will determine that on-the-fly.

To run your test, just execute:

```console
$ eldev test
[1/1] Installing package ‘buttercup’ (1.23) from ‘melpa-stable’...
Running 1 specs.

The feature
  can use bug and feature (2.58ms)

Ran 1 specs, 0 failed, in 2.68ms.
```

You can also specify patterns on the command line, to avoid running
all the tests at once, i.e.:

```console
$ eldev test foo
```

If you have several test files, you can avoid running all the tests by
specifying filename after `-f` (`--file`) option:

```console
$ eldev test -f main.el
```

For more information, please see tool’s own documentation, especially
[the section about testing](https://github.com/doublep/eldev#testing).

## Eask
> 📢 Eask is very similar to Cask; anything that applies to Cask will apply to Eask

[Eask](https://github.com/emacs-eask/cli) is a replacement for Cask which is
actively maintained.

### Project Directory Layout

> 📢 The project directory layout is similar to the Cask's one. But instead of
> `Cask`-file, you should have `Eask`-file as a replacement.

A basic project layout requires a project file, called `feature.el`
here, an `Eask`-file to define dependencies, and a `tests/` directory
for tests. It should look roughly like this:

```
feature/feature.el
        Eask
        tests/test-feature.el
```

**feature.el**

```elisp
(defun featurize (bug feature)
  (format "It's not a %s, it's a %s" bug feature))

(provide 'feature)
```

**Eask-file**

```elisp
(source 'gnu)
(source 'melpa-stable)

(development
 (depends-on "buttercup"))
```

**tests/test-feature.el**

```elisp
;;; -*- lexical-binding: t; -*-
(require 'feature)

(describe "The feature"
  (it "can use bug and feature"
    (expect (featurize "bug" "feature")
            :to-equal
            "It's not a bug, it's a feature")))
```

### Running Tests

You can now use Eask to run your tests.

First, you have to install the dependencies. You only have to do this once, or
when the dependencies change:

```console
$ eask install-deps --dev
...

Installing 1 development dependency...
Loading package information... done
  - Installing buttercup (20230119.2337)... done
(Total of 1 dependency installed, 0 skipped)
```

Now, you can run your tests:

```console
$ eask exec buttercup -L .
...

Running 1 specs.

The feature
  can use bug and feature

Ran 1 specs, 0 failed, in 0.0 seconds.
```

That’s it. Buttercup’s built-in discover test runner looks for files
named `test-*.el`, `*-test.el` or `*-tests.el`.

Use the `--pattern PATTERN` option to only Only run tests with names
matching PATTERN. The `--pattern` option can be used multiple times,
in which case tests will be run if they match any of the given
patterns. Combine with the `--no-skip` option to filter out the
skipped tests.

You can run this command whichever way you like. Common choices
include a Makefile or shell scripts.

See [relevant documentation](https://emacs-eask.github.io/) on Eask's own page
for more information.

## Projectile

If you use [Projectile](https://github.com/bbatsov/projectile) for interacting
with your projects you can set the "default" project test command to be
available when you invoke `projectile-test-project`.  Create a `.dir-locals.el`
file in the the root of your project tree (next to your Cask file).  An example:

**.dir-locals.el**

```elisp
((nil . ((eval . (progn
                   (require 'projectile)
                   (puthash (projectile-project-root)
                            "cask exec buttercup -L ."
                            projectile-test-cmd-map))))))
```

If you are using Eldev as build tool, Projectile should provide
testing command on its own, so you don’t need any special steps.

## Travis

### Cask

If your project is hosted on github, you can use
[Travis CI](https://travis-ci.org/) as your continuous integration
environment. Buttercup can easily be used in such a setup. Simply add
the following `.travis.yml` file:

```yaml
language: emacs-lisp
sudo: false
cache: apt
env:
  - EVM_EMACS=emacs-24.5-travis
  - EVM_EMACS=emacs-25.1-travis
before_install:
  - curl -fsSkL https://gist.github.com/rejeep/ebcd57c3af83b049833b/raw > travis.sh && source ./travis.sh
  - evm install $EVM_EMACS --use --skip
  - cask
install:
  - cask install
script:
  - emacs --version
  - cask exec buttercup -L .
```

Most of the complexity here is from installing
[EVM](https://github.com/rejeep/evm) and Cask to be able to test your
project using different Emacs versions.

### Eldev

For Eldev, use the following `.travis.yml` file:

```yaml
language: emacs-lisp
dist: trusty

env:
  # Add more lines like this if you want to test on different Emacs versions.
  - EVM_EMACS=emacs-26.3-travis

install:
  - curl -fsSL https://raw.github.com/doublep/eldev/master/webinstall/travis-eldev-and-evm > x.sh && source ./x.sh
  - evm install $EVM_EMACS --use

script:
  - eldev -p -dtT test
```

For details, see [tool’s own documentation](https://github.com/doublep/eldev#continuous-integration).

### Eask

For Eask, please see [tool's documentation](https://emacs-eask.github.io/Continuous-Integration/Travis-CI/).
