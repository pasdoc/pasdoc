#!/bin/bash
set -eu

# This scripts runs v.Nu to check every *.html file in html/ subdirectory,
# recursively.
# It's meant to be run using `make validate_html' in parent directory.
#
# See ../README for comments.

# check if vnu is available and fail otherwise
command -v vnu >/dev/null 2>&1 || { printf "%s\n" \
    "v.Nu has to be installed for HTML validation!" \
    "See the README.md inside the tests directory to learn how..." 1>&2; exit 1; }

vnu --skip-non-html --format text testcases_output/html/
