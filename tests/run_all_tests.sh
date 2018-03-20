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

cd testcases/
../scripts/mk_tests.sh $ALL_OUTPUT_FORMATS
cd ../

git diff --exit-code testcases_output/

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
