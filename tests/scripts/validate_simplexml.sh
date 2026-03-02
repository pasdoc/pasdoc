#!/bin/bash
set -eu

# This scripts runs xmllint to check every *.xml file in simplexml/
# subdirectory, recursively.
# It's meant to be run using `make validate_simplexml' in parent directory.
#
# See ../README for comments.

# check if xmllint is available and fail otherwise
if ! command -v xmllint > /dev/null 2>&1; then
  echo 'xmllint missing'
  exit 1
fi

echo 'Validating simplexml output using xmllint.'

find testcases_output/simplexml/ -iname '*.xml' -print0  \
    | while read -rd $'\0' FILE; do
  echo "---- Validating ${FILE}"
  xmllint --noout "${FILE}"
done
