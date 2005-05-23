#!/bin/bash
set -eu

# This script creates tests output for particular pasdoc output format.
# $1 is this format name. It's also the name of subdirectory where
# tests output will be placed.
#
# This script is meant to be called only from Makefile in this directory.

# functions ----------------------------------------

# $1 is name of subdir (must end with /) where to put test output
# (including PASDOC-OUTPUT file).
# Rest of params are pasdoc's command-line:
# special command-line parameters and unit files to process.
mk_test ()
{
  OUTPUT_PATH="$1"
  shift 1

  mkdir -p "$OUTPUT_PATH"

  PASDOC_OUTPUT_FILENAME="$OUTPUT_PATH"PASDOC-OUTPUT

  echo "Running pasdoc:"
  echo pasdoc --format "$FORMAT" --exclude-generator \
    --output="$OUTPUT_PATH" "$@" '>' "$PASDOC_OUTPUT_FILENAME"
       pasdoc --format "$FORMAT" --exclude-generator \
    --output="$OUTPUT_PATH" "$@" >   "$PASDOC_OUTPUT_FILENAME"
}

# Like mk_test but $1 is name of subdir inside "$FORMAT" subdir.
mk_special_test ()
{
  OUTPUT_PATH="$FORMAT"/"$1"/
  shift 1

  mk_test "$OUTPUT_PATH" "$@"
}

# parse params ----------------------------------------

FORMAT="$1"
shift 1

# Run tests ----------------------------------------

# Try to place each entry below on separate line,
# so that it's easy to temporary switch some test on/off by
# simply commenting / uncommenting that line.

# Make a general test of everything with normal pasdoc command-line.
#
# Note that sorting output of find below is needed to get "stable" order,
# otherwise find outputs files in pretty much random (as they happened to
# returned by OS calls), and this can accidentaly change order of lines in
# PASDOC-OUTPUT files.
mk_test "$FORMAT"/ \
`find . -iname '*.pas' -maxdepth 1 -not '(' \
  -iname 'ok_link_1_char.pas' -or \
  -iname 'ok_const_1st_comment_missing.pas' -or \
  -iname 'ok_auto_abstract.pas' -or \
  -iname 'warning_incorrect_tag_nesting.pas' -or \
  -iname 'ok_param_raises_returns_proctype.pas' \
  ')' | sort`

# Make a specialized test of some units that need special
# command-line. This is also useful if you want to just make
# some units in a separate subdirectories, to separate them
# from the rest of tests (e.g. because you want to test AllXxx.html pages).
mk_special_test ok_const_1st_comment_missing --marker=: ok_const_1st_comment_missing.pas
mk_special_test ok_link_1_char --visible-members 'private,public,published' ok_link_1_char.pas
mk_special_test ok_auto_abstract --auto-abstract ok_auto_abstract.pas
mk_special_test warning_incorrect_tag_nesting warning_incorrect_tag_nesting.pas
mk_special_test ok_param_raises_returns_proctype ok_param_raises_returns_proctype.pas