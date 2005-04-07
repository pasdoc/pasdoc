#!/bin/bash
set -eu

# When running this script, the current directory
# must be the directory of the script, i.e. tests/scripts/
# in pasdoc's sources.
#
# See ../README file for docs for this script.

OUTPUT_FORMAT="$1"
shift 1

rm -Rf check_cache_tmp/
mkdir -p check_cache_tmp/cache/ check_cache_tmp/1/ check_cache_tmp/2/

cd ..

pasdoc_call ()
{
  pasdoc "$@" --format="$OUTPUT_FORMAT" ok_*.pas warning_*.pas \
    --cache-dir=scripts/check_cache_tmp/cache/
}

pasdoc_call --output=scripts/check_cache_tmp/1/
pasdoc_call --output=scripts/check_cache_tmp/2/
diff -u scripts/check_cache_tmp/1/ scripts/check_cache_tmp/2/

rm -Rf check_cache_tmp/
