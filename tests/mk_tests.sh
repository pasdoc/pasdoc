#!/bin/bash
set -eu

# This script creates tests output for particular pasdoc output format.
# $1 is this format name. It's also the name of subdirectory where
# tests output will be placed.
#
# This script is meant to be called only from Makefile in this directory.

# functions ----------------------------------------

# Simply runs pasdoc with "$@" and common command-line options.
# And echoes this is output.
run_pasdoc ()
{
  echo "Running pasdoc:"
  echo pasdoc --format "$FORMAT" --exclude-generator "$@"
  pasdoc --format "$FORMAT" --exclude-generator "$@"
}

# $1 is name of subdir inside "$FORMAT"/ where to put test output.
# Rest of params are pasdoc's command-line:
# special command-line paramaters and unit files to process.
mk_special_test ()
{
  OUTPUT_PATH="$FORMAT"/"$1"/
  shift 1

  mkdir -p "$OUTPUT_PATH"
  run_pasdoc --output="$OUTPUT_PATH" "$@"
}

# parse params ----------------------------------------

FORMAT="$1"
shift 1

# Run tests ----------------------------------------

mkdir -p "$FORMAT"

# Try to place each entry below on separate line,
# so that it's easy to temporary switch some test on/off by
# simply commenting / uncommenting that line.

# Make a general test of everything with normal pasdoc command-line
run_pasdoc --output="$FORMAT"/ \
`find . -iname '*.pas' -maxdepth 1 -not '(' \
  -iname 'ok_link_1_char.pas' -or \
  -iname 'ok_const_1st_comment_missing.pas' -or \
  -iname 'ok_auto_abstract.pas' \
  ')' `

# Make a specialized test of some units that need special
# command-line
mk_special_test ok_const_1st_comment_missing --marker=: ok_const_1st_comment_missing.pas
mk_special_test ok_link_1_char --visible-members 'private,public,published' ok_link_1_char.pas
mk_special_test ok_auto_abstract --auto-abstract ok_auto_abstract.pas
