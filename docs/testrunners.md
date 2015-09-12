# Test Runners

Evaluating `describe` forms just stores the suites. You need to use a
test runner to actually evaluate them. Buttercup comes with three test
runners by default:

- `buttercup-run-at-point` — Evaluate the topmost `describe` form at
  point and run the suite it creates directly. Useful for interactive
  development. But be careful, this uses your current environment,
  which might not be clean (due to said interactive development).
- `buttercup-run-discover` — Find files in directories specified on
  the command line, load them, and then run all suites defined
  therein. Useful for being run in batch mode.
- `buttercup-run-markdown` — Run code in markdown files. Used to
  run this file’s code.
