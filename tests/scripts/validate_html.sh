#!/bin/bash
set -eu

# This scripts runs onsgmls to check every *.html file in html/ subdirectory,
# recursively.
# It's meant to be run using `make validate_html' in parent directory.
#
# See ../README for comments.

# Why do I avoid validating index.html here ?
#
# Because I can't force onsgmls installed from Cygwin to properly 
# validate <frameset> document. (While version installed 
# from Debian package works OK). This is probably not really 
# related to Cygwin "core" functionality, but to some specific 
# problem of Cygwin installation of onsgmls. So I decided to simply 
# disable validating index.html, so that Cygwin users will not 
# have problems.
#
# It's not a big deal because index.html is a small file, 
# and you can always check it manually with `onsgmls -s -e -g index.html'.
#
# Earlier I printed here a warning:
#   echo "---- Not validating $FFF due to problems with Cygwin's onsgmls"
# but it seems that this warning is too terse (people see it and
# think that it's some validation problem that they should report), 
# so now index.html is just "silently" ignored.

find html/ -iname '*.html' \
  '(' -not -iname 'index.html' ')' \
  -exec sh -c 'echo ---- Validating {}' ';' \
  -exec onsgmls -s -e -g '{}' ';'
