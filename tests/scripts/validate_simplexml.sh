#!/bin/bash
set -eu

# This scripts runs xmllint to check every *.xml file in simplexml/
# subdirectory, recursively.
# It's meant to be run using `make validate_simplexml' in parent directory.
#
# See ../README for comments.

# check if xmllint is available and fail otherwise
which xmllint > /dev/null

echo 'Validating simplexml output using xmllint.'

find testcases_output/simplexml/ -iname '*.xml' \
  -exec sh -c 'echo ---- Validating {}' ';' \
  -exec xmllint --noout '{}' ';'
