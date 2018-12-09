#!/bin/bash
set -eu

find ../../ '(' \
  -iname '*.pas' -or \
  -iname '*.inc' -or \
  -iname '*.css' -or \
  -iname '*.dpr' ')' \
  -execdir sed --in-place -e 's|Copyright 1998-2016 PasDoc developers|Copyright 1998-2018 PasDoc developers|' '{}' ';'
