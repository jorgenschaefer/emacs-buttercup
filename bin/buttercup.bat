@echo off

setlocal enabledelayedexpansion

if not [%inside_emacs%]==[] (
  set emacs_bin=emacs
) else if not [%emacs%]==[] (
  set emacs_bin=%emacs%
) else (
  set emacs_bin=emacs
)

goto parse_args

:usage
echo %0 [OPTIONS] [DIRS]
echo.
echo Buttercup will search recursively in each of DIRS for test files (any
echo elisp file starting with "test-" or ending with "-test.el" or
echo ^"-tests.el"). It will load all of those files and then run the tests
echo defined in those files. If no directories are specified, buttercup
echo will search in the current directory.
echo.
echo Options can include the options described below, as well as the
echo standard Emacs options "--directory", "--funcall", "--load", "--eval",
echo and "--execute". These Emacs options should be used to ensure that any
echo Elisp files required for the tests can be found in Emacs' load path.
echo For package testing, "-L ." is commonly used. See "emacs --help" for
echo more information on Emacs options.
echo.
echo Buttercup options:
echo.
echo --pattern, -p PATTERN   Only run tests with names matching PATTERN.
echo                           This option can be used multiple times, in
echo                           which case tests will be run if they match
echo                           any of the given patterns. PATTERN should be
echo                           an Emacs regex that will be matched against
echo                           the full test description (the concatenation
echo                           of the test and all parent suites
echo                           descriptions).
echo.
echo --no-skip               Do not print the descriptions for tests that
echo                           are filtered out with "--pattern" or disabled
echo                           with "xit". Tests skipped wiath "assume" will
echo                           still be priuted,
echo.
echo --only-error            Only print failed tests and their containing suites.
echo                           Implies "--no-skip".
echo.
echo --no-color, -c          Do not colorize test output.
echo.
echo --traceback STYLE       When printing backtraces for errors that occur
echo                           during tests, print them in the chosen
echo                           STYLE. Available styles are
echo                          "full", which shows the full function call for
echo                           each stack frame on a single line,
echo                          "crop", which truncates each stack frame to 80
echo                           characters (the default),
echo                          "pretty", which uses Emacs' pretty-printing
echo                           facilities to print each stack frame, and also
echo                           annotates each frame with a lambda or M to
echo                           indicate whether it is a normal function call
echo                           or a macro/special form and
echo                          "omit", which omits the backtraces alltogether.
echo.
echo --stale-file-error      Fail the test run if stale .elc files are loaded.
exit /b

:parse_args

set emacs_args=
set buttercup_args=

:next_args
set current_arg=%1
set next_arg=%2
if not [%current_arg%]==[] (
  if !current_arg!==-h (
    goto usage
  ) else if !current_arg!==--help (
    goto usage
  ) else if !current_arg!==-L (
    set emacs_args=!emacs_args! !current_arg! !next_arg!
    shift /1
    shift /1
  ) else if !current_arg!==--directory (
    set emacs_args=!emacs_args! !current_arg! !next_arg!
    shift /1
    shift /1
  ) else if !current_arg!==-f (
    set emacs_args=!emacs_args! !current_arg! !next_arg!
    shift /1
    shift /1
  ) else if !current_arg!==--funcall (
    set emacs_args=!emacs_args! !current_arg! !next_arg!
    shift /1
    shift /1
  ) else if !current_arg!==--l (
    set emacs_args=!emacs_args! !current_arg! !next_arg!
    shift /1
    shift /1
  ) else if !current_arg!==--load (
    set emacs_args=!emacs_args! !current_arg! !next_arg!
    shift /1
    shift /1
  ) else if !current_arg!==--eval (
    set emacs_args=!emacs_args! !current_arg! !next_arg!
    shift /1
    shift /1
  ) else if !current_arg!==--execute (
    set emacs_args=!emacs_args! !current_arg! !next_arg!
    shift /1
    shift /1
  ) else if !current_arg!==-p (
    set buttercup_args=!buttercup_args! !current_arg! !next_arg!
    shift /1
    shift /1
  ) else if !current_arg!==--pattern (
    set buttercup_args=!buttercup_args! !current_arg! !next_arg!
    shift /1
    shift /1
  ) else if !current_arg!==-c (
    set buttercup_args=!buttercup_args! !current_arg!
    shift /1
  ) else if !current_arg!==--no-color (
    set buttercup_args=!buttercup_args! !current_arg!
    shift /1
  ) else if !current_arg!==--no-skip (
    set buttercup_args=!buttercup_args! !current_arg!
    shift /1
  ) else if !current_arg!==--only-error (
    set buttercup_args=!buttercup_args! !current_arg!
    shift /1
  ) else if !current_arg!==--stale-file-error (
    set buttercup_args=!buttercup_args! !current_arg!
    shift /1
  ) else if !current_arg!==--traceback (
    set buttercup_args=!buttercup_args! !current_arg! !next_arg!
    shift /1
    shift /1
  ) else (
    set buttercup_args=!buttercup_args! !current_arg!
    shift /1
  )
  goto next_args
)

%emacs_bin% -batch %emacs_args% -l buttercup -f buttercup-run-discover %buttercup_args%
