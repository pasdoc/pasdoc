#!/bin/bash
set -eu

# This script creates tests output for particular pasdoc output format.
# $1 is this format name. It's also the name of subdirectory where
# tests output will be placed.
#
# This script is meant to be called only from Makefile in this directory.

SORT_ALL='--sort=structures,constants,functions,types,variables,uses-clauses,record-fields,non-record-fields,methods,properties'
SORT_OLD='--sort=functions,record-fields,non-record-fields,methods,properties'

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
mk_test "$FORMAT"/ "$SORT_OLD" \
`find . -iname '*.pas' -maxdepth 1 -not '(' \
  -iname 'ok_link_1_char.pas' -or \
  -iname 'ok_const_1st_comment_missing.pas' -or \
  -iname 'ok_auto_abstract.pas' -or \
  -iname 'warning_incorrect_tag_nesting.pas' -or \
  -iname 'ok_param_raises_returns_proctype.pas' -or \
  -iname 'ok_no_sort.pas' -or \
  -iname 'ok_sorting.pas' -or \
  -iname 'ok_introduction_conclusion.pas' -or \
  -iname 'ok_property_decl.pas' -or \
  -iname 'ok_multiple_vars.pas' -or \
  -iname 'ok_class_function.pas' -or \
  -iname 'ok_latex_head.pas' -or \
  -iname 'ok_longcode_underscores.pas' \
  ')' | sort`

# Make a specialized test of some units that need special
# command-line. This is also useful if you want to just make
# some units in a separate subdirectories, to separate them
# from the rest of tests (e.g. because you want to test AllXxx.html pages).
# This is also useful if you want to test the same unit more than once,
# with different command-line options (e.g. like ok_sorting.pas).

mk_special_test ok_const_1st_comment_missing --marker=: "$SORT_OLD" ok_const_1st_comment_missing.pas
mk_special_test ok_link_1_char --visible-members 'private,public,published' "$SORT_OLD" ok_link_1_char.pas
mk_special_test ok_auto_abstract --auto-abstract "$SORT_OLD" ok_auto_abstract.pas
mk_special_test warning_incorrect_tag_nesting "$SORT_OLD"  warning_incorrect_tag_nesting.pas
mk_special_test ok_param_raises_returns_proctype "$SORT_OLD" ok_param_raises_returns_proctype.pas
mk_special_test ok_no_sort '--sort=functions,non-record-fields,methods,properties' ok_no_sort.pas
mk_special_test ok_sorting_all "$SORT_ALL" ok_sorting.pas
mk_special_test ok_sorting_none --sort= ok_sorting.pas
mk_special_test ok_introduction_conclusion ok_introduction_conclusion.pas --introduction=ok_introduction.txt --conclusion=ok_conclusion.txt
mk_special_test ok_property_decl ok_property_decl.pas
mk_special_test ok_multiple_vars ok_multiple_vars.pas
mk_special_test ok_class_function ok_class_function.pas
mk_special_test ok_latex_head --latex-head=ok_latex_head.tex ok_latex_head.pas
mk_special_test ok_longcode_underscores ok_longcode_underscores.pas