#!/bin/bash
set -eu

# This scripts runs xmllint to check every *.xml file in simplexml/
# subdirectory, recursively.
# It's meant to be run using `make validate_simplexml' in parent directory.
#
# See ../README for comments.

find current_output/simplexml/ -iname '*.xml' \
  -exec sh -c 'echo ---- Validating {}' ';' \
  -exec xmllint --noout '{}' ';'
