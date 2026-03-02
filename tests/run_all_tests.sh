#!/bin/bash
set -euo pipefail

# ----------------------------------------------------------------------------
# Run all PasDoc tests.
#
# Uses (subset of) http://redsymbol.net/articles/unofficial-bash-strict-mode/
# ----------------------------------------------------------------------------

# Set $MAKE, unless already set.
# This allows parent process to pass MAKE value to this script.
# We expect GNU make (this option allows to set it,
# e.g. on Windows or FreeBSD where other "make" may be on $PATH).
if [ "${MAKE:-}" = '' ]; then
  MAKE='make'
fi

$MAKE clean

# fpcunit tests --------------------------------------------------------------

pushd fpcunit
$MAKE
popd

# build pasdoc --------------------------------------------------------------

pushd ../
$MAKE
popd

# ----------------------------------------------------------------------------

./run_all_tests_no_build.sh
