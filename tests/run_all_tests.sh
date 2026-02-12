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
# Find pasdoc binary, setting PASDOC_BIN to absolute exe path.
# If not found, fail.

export PASDOC_BIN
if [ -f ../bin/pasdoc ]; then
  PASDOC_BIN=$(pwd)/../bin/pasdoc
elif [ -f ../bin/pasdoc.exe ]; then
  PASDOC_BIN=$(pwd)/../bin/pasdoc.exe
else
  if ! which pasdoc > /dev/null; then
    # shellcheck disable=SC2016
    echo 'pasdoc binary not found on $PATH'
    exit 1
  fi
  PASDOC_BIN=$(which pasdoc)
fi
echo "Detected pasdoc binary as ${PASDOC_BIN}"
"${PASDOC_BIN}" --version

# run all testcases, compare with correct output ------------------------------

ALL_OUTPUT_FORMATS='html htmlhelp latex latex2rtf simplexml php'

# Set environment variable USE_DIFF_TO_COMPARE to compare using plain `diff`.
# Otherwise we will assume we are inside a code repository,
# and will compare using `git diff`.

if [ "${USE_DIFF_TO_COMPARE:-false}" = 'true' ]; then
  rm -Rf current_output/
  cp -R testcases_output/ current_output/
fi

echo 'Regenerating "testcases_output".'
rm -Rf testcases_output/

pushd testcases
# shellcheck disable=SC2086
../scripts/mk_tests.sh $ALL_OUTPUT_FORMATS
popd

if [ "${USE_DIFF_TO_COMPARE:-false}" = 'true' ]; then
  echo 'Comparing "testcases_output" with "current_output".'
  diff -wur testcases_output/ current_output/
  rm -Rf current_output/
else
  echo 'Comparing "testcases_output" with their state in GIT repository.'
  git diff -w --exit-code testcases_output/
fi

# validation -----------------------------------------------------------------

# Validate testcases_output/html, requires vnu installed
#scripts/validate_html.sh

# Validate testcases_output/simplexml, requires xmllint installed
scripts/validate_simplexml.sh

# Validate testcases_output/php, requires php cli installed
scripts/validate_php.sh

# cache tests ----------------------------------------------------------------

cd scripts/
for OUTPUT_FORMAT in $ALL_OUTPUT_FORMATS; do
  # shellcheck disable=SC2086
  ./check_cache.sh $OUTPUT_FORMAT
done

./check_cache_format_independent.sh html latex
./check_cache_format_independent.sh latex2rtf htmlhelp
