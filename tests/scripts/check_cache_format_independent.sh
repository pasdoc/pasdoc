#!/bin/bash
set -eu

# When running this script, the current directory
# must be the directory of the script, i.e. tests/scripts/
# in pasdoc's sources.
#
# See ../README file for docs for this script.

# functions ------------------------------------------------------------

run_echo ()
{
  echo "$@"
  scripts/find_all_tests_for_check_cache.sh | "$@"
}

pasdoc_call ()
{
  echo 'Running pasdoc:'
  run_echo pasdoc -S - --exclude-generator "$@"
}

# ------------------------------------------------------------

OUTPUT_FORMAT_1="$1"
OUTPUT_FORMAT_2="$2"
shift 2

rm -Rf check_cache_format_independent_tmp/
mkdir -p \
  check_cache_format_independent_tmp/cache/ \
  check_cache_format_independent_tmp/1/ \
  check_cache_format_independent_tmp/2/ \
  check_cache_format_independent_tmp/3/

cd ..

# No cache, format 1
pasdoc_call \
  --output=scripts/check_cache_format_independent_tmp/1/ \
  --format="$OUTPUT_FORMAT_1"

# Make cache while making format 2
pasdoc_call \
  --output=scripts/check_cache_format_independent_tmp/2/ \
  --format="$OUTPUT_FORMAT_2" \
  --cache-dir=scripts/check_cache_format_independent_tmp/cache/

# Use cache with format 1
pasdoc_call \
  --output=scripts/check_cache_format_independent_tmp/3/ \
  --format="$OUTPUT_FORMAT_1" \
  --cache-dir=scripts/check_cache_format_independent_tmp/cache/

echo 'Comparing two outputs:'
diff -ur \
  scripts/check_cache_format_independent_tmp/1/ \
  scripts/check_cache_format_independent_tmp/3/
echo 'OK, test passed.'

rm -Rf scripts/check_cache_format_independent_tmp/
