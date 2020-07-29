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

## Project Directory Layout

A basic project layout requires a project file, called `feature.el`
here, a `Cask` file to define dependencies, and a `tests/` directory
for tests. It should look roughly like this:

```
feature/feature.el
        Cask
        tests/test-feature.el
```

**feature.el**

```Emacs-Lisp
(defun featurize (bug feature)
  (format "It's not a %s, it's a %s" bug feature))

(provide 'feature)
```

**Cask**

```
(source gnu)
(source melpa-stable)

(development
 (depends-on "buttercup"))
```

**tests/test-feature.el**

```Lisp
(require 'feature)

(describe "The feature"
  (it "can use bug and feature"
    (expect (featurize "bug" "feature")
            :to-equal
            "It's not a bug, it's a feature")))
```

## Running Tests

You can now use Cask to run your tests.

First, you have to install the dependencies. You only have to do this
once, or when the dependencies change:

```
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

```
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

## Projectile

If you use [Projectile](https://github.com/bbatsov/projectile) for interacting with your projects you can set the "default" project test command to be available when you invoke `projectile-test-project`.  Create a `.dir-locals.el` file in the the root of your project tree (next to your Cask file).  An example:

**.dir-locals.el**

```
((nil . ((eval . (progn
                   (require 'projectile)
                   (puthash (projectile-project-root)
                            "cask exec buttercup -L ."
                            projectile-test-cmd-map))))))
```

## Travis

If your project is hosted on github, you can use
[Travis CI](https://travis-ci.org/) as your continuous integration
environment. Buttercup can easily be used in such a setup. Simply add
the following `.travis.yml` file:

```
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
