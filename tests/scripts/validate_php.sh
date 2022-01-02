#!/bin/bash
set -eu

# This scripts runs php to check every *.php file in php/
# subdirectory, recursively.
# It's meant to be run using `make validate_php' in parent directory.

# check if php is available and fail otherwise
which php > /dev/null

echo 'Validating PHP output using command-line php.'

find testcases_output/php/ -iname '*.php' \
  -exec sh -c 'echo ---- Validating {}' ';' \
  -exec php -f '{}' ';'
