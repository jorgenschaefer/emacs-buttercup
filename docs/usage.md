# Usage

Buttercup is primarily meant to be used non-interactively, to
automatically test a project independent of a user’s setup, before a
commit and on a continuous integration platform. Because of this, the
workflow for buttercup does not use interactive commands but instead
the command line.

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
        tests/feature.el
```

**feature.el**

```Lisp
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

That’s it.

You can run this command whichever way you like. Common choices
include a makefile or shell scripts.

## Travis

If your project is hosted on github, you can use
[Travis CI](https://travis-ci.org/) as your continuous integration
environment. Buttercup can easily be used in such a setup. Simply add
the following `.travis.yml` file:

```
language: emacs-lisp
env:
  - EVM_EMACS=emacs-24.4-bin
  - EVM_EMACS=emacs-24.5-bin
before_install:
  - sudo mkdir /usr/local/evm
  - sudo chown $(id -u):$(id -g) /usr/local/evm
  - curl -fsSkL https://raw.github.com/rejeep/evm/master/go | bash
  - export PATH="$HOME/.evm/bin:$PATH"
  - evm install $EVM_EMACS --use
  - curl -fsSkL https://raw.github.com/cask/cask/master/go | python
  - export PATH="$HOME/.cask/bin:$PATH"
  - cask
script:
  - cask exec buttercup -L .
```

Most of the complexity here is from installing
[EVM](https://github.com/rejeep/evm) and Cask to be able to test your
project using different Emacs versions.
