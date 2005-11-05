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
  run_echo pasdoc \
    --format="$OUTPUT_FORMAT" -S - \
    --exclude-generator \
    --cache-dir=scripts/check_cache_tmp/cache/ \
    "$@"
}

# ------------------------------------------------------------

OUTPUT_FORMAT="$1"
shift 1

rm -Rf check_cache_tmp/
mkdir -p check_cache_tmp/cache/ check_cache_tmp/1/ check_cache_tmp/2/

cd ..

pasdoc_call --output=scripts/check_cache_tmp/1/
pasdoc_call --output=scripts/check_cache_tmp/2/

echo 'Comparing two outputs:'
diff -u scripts/check_cache_tmp/1/ scripts/check_cache_tmp/2/
echo 'OK, test passed.'

rm -Rf scripts/check_cache_tmp/
