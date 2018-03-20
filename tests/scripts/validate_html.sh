#!/bin/bash
set -eu

# This scripts runs onsgmls to check every *.html file in html/ subdirectory,
# recursively.
# It's meant to be run using `make validate_html' in parent directory.
#
# See ../README for comments.
#
# TODO: This is not really useful for now, as it doesn't validate HTML 5.
# Fixes (to make onsgmls validate HTML 5, or use different validator
# than onsgmls) welcome.

# check if onsgmls is available and fail otherwise
which onsgmls

find testcases_output/html/ -iname '*.html' \
  -exec sh -c 'echo ---- Validating {}' ';' \
  -exec onsgmls -s -e -g '{}' ';'
