#!/bin/bash
set -eu

# Print all ok_* and warning_* tests that should pass
# without errors with default command-line.
# E.g. I don't pass ok_macros_off.pas because it results in error
# if --no-macro was not used.

/usr/bin/find . '(' -iname 'ok_*.pas' -or -iname 'warning_*.pas' ')' \
  -and -not '(' -iname 'ok_macros_off.pas' ')' 