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
echo                           any of the given patterns.
echo.
echo --no-color, -c          Do not colorize test output.
echo.
echo --traceback STYLE       When printing backtraces for errors that occur
echo                           during tests, print them in the chosen
echo                           STYLE. Available styles are "full", which
echo                           shows the full function call for each stack
echo                           frame on a single line, "crop", which
echo                           truncates each stack frame to 80 characters
echo                           ^(the default^), and "pretty", which uses
echo                           Emacs' pretty-printing facilities to print
echo                           each stack frame, and also annotates each
echo                           frame with a lambda or M to indicate whether
echo                           it is a normal function call or a
echo                           macro/special form.
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
