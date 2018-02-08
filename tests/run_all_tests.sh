#!/bin/bash
set -eu

# Run all PasDoc tests.

make clean

# fpcunit tests --------------------------------------------------------------

cd fpcunit/
make
cd ../

# run on testcases, compare with correct output ------------------------------

ALL_OUTPUT_FORMATS='html htmlhelp latex latex2rtf simplexml'

cd scripts/
./download_correct_tests_output.sh $ALL_OUTPUT_FORMATS
cd ../

for OUTPUT_FORMAT in $ALL_OUTPUT_FORMATS; do
  cd testcases/
  ../scripts/mk_tests.sh $OUTPUT_FORMAT
  cd ../
  diff -wur correct_output/$OUTPUT_FORMAT current_output/$OUTPUT_FORMAT
done

# If you detect any *valid* differences, i.e. the new version is more correct,
# run this:
# cd scripts/ && ./upload_correct_tests_output.sh $SOURCEFORGE_USERNAME $ALL_OUTPUT_FORMATS && cd ../

# validation -----------------------------------------------------------------

# Validate current_output/html, requires onsgmls installed
# This is unfortunately not working for HTML 5 now.
# scripts/validate_html.sh

# Validate current_output/simplexml, requires xmllint installed
scripts/validate_simplexml.sh

# cache tests ----------------------------------------------------------------

cd scripts/
for OUTPUT_FORMAT in $ALL_OUTPUT_FORMATS; do
  ./check_cache.sh $OUTPUT_FORMAT
done

./check_cache_format_independent.sh html latex
./check_cache_format_independent.sh latex2rtf htmlhelp
