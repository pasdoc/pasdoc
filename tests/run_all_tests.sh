#!/bin/bash
set -eu

# Run all PasDoc tests.

make clean

# fpcunit tests --------------------------------------------------------------

cd fpcunit/
make
cd ../

# run all testcases, compare with correct output ------------------------------

ALL_OUTPUT_FORMATS='html htmlhelp latex latex2rtf simplexml'

# Set environment variable USE_DIFF_TO_COMPARE to compare using plain `diff`.
# Otherwise we will assume we are inside a code repository,
# and will compare using `git diff`.

if [ "${USE_DIFF_TO_COMPARE:-false}" = 'true' ]; then
  rm -Rf current_output/
  cp -R testcases_output/ current_output/
fi

echo 'Regenerating "testcases_output".'
rm -Rf testcases_output/

cd testcases/
../scripts/mk_tests.sh $ALL_OUTPUT_FORMATS
cd ../

if [ "${USE_DIFF_TO_COMPARE:-false}" = 'true' ]; then
  echo 'Comparing "testcases_output" with "current_output".'
  diff -wur testcases_output/ current_output/
  rm -Rf current_output/
else
  echo 'Comparing "testcases_output" with their state in GIT repository.'
  git diff -w --exit-code testcases_output/
fi

# validation -----------------------------------------------------------------

# Validate testcases_output/html, requires onsgmls installed
# This is unfortunately not working for HTML 5 now.
# scripts/validate_html.sh

# Validate testcases_output/simplexml, requires xmllint installed
scripts/validate_simplexml.sh

# cache tests ----------------------------------------------------------------

cd scripts/
for OUTPUT_FORMAT in $ALL_OUTPUT_FORMATS; do
  ./check_cache.sh $OUTPUT_FORMAT
done

./check_cache_format_independent.sh html latex
./check_cache_format_independent.sh latex2rtf htmlhelp
