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

run_echo ()
{
  OUTPUT_FILENAME="$1"
  shift 1

  echo 'Running:' "$@" '>' "$OUTPUT_FILENAME"

  # Temporary set +e, to ignore exit status from pasdoc
  set +e
  "$@" > "$OUTPUT_FILENAME"
  set -e
}

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

  run_echo "$PASDOC_OUTPUT_FILENAME" \
    pasdoc --format "$FORMAT" --exclude-generator \
    --output="$OUTPUT_PATH" "$@"
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

# Make a test of many units with normal pasdoc command-line.
#
# Note: most new tests should not be added here.
# It was the 1st approach to just call something like `pasdoc *.pas' here
# to test everything. But this was not good, because
# 1. obviously you can't do tests with specialized pasdoc command-line options
# 2. in case of output formats that create some "index" pages
#    (like AllClasses.html in HTML / HtmlHelp output) small
#    changes and additions have too global impact on many parts of
#    documentation, so output from `diff -wur correct_output/html html/'
#    is harder to grok for humans.
#
# Instead, usually you should add new test units as new calls to
# `mk_special_test ...' lower in this script.
#
mk_test "$FORMAT"/ "$SORT_OLD" \
  error_line_number.pas \
  ok_cdecl_external.pas \
  ok_complicated_record.pas \
  ok_deprecated_tag.pas \
  ok_directive_as_identifier.pas \
  ok_expanding_descriptions.pas \
  ok_hint_directives.pas \
  ok_line_break.pas \
  ok_link_class_unit_level.pas \
  ok_link_explicite_name.pas \
  ok_links_2.pas \
  ok_links.pas \
  ok_nodescription_printing.pas \
  ok_paragraph_in_single_line_comment.pas \
  ok_tag_name_case.pas \
  ok_tag_params_no_parens.pas \
  ok_value_member_tags.pas \
  warning_abstract_termination.pas \
  warning_abstract_twice.pas \
  warning_not_existing_tags.pas \
  warning_tags_no_parameters.pas \
  warning_value_member_tags.pas

# Make a specialized test of some units that need special
# command-line. This is also useful if you want to just make
# some units in a separate subdirectories, to separate them
# from the rest of tests (e.g. because you want to test AllXxx.html pages).
# This is also useful if you want to test the same unit more than once,
# with different command-line options (e.g. like ok_sorting.pas).
#
# Try to place each entry below on separate line,
# so that it's easy to temporary switch some test on/off by
# simply commenting / uncommenting that line.
#
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
mk_special_test ok_longcode_comment ok_longcode_comment.pas
mk_special_test ok_longcode_dash ok_longcode_dash.pas
mk_special_test ok_longcode_special_chars ok_longcode_special_chars.pas
mk_special_test error_introduction_twice_anchors_unit --introduction=error_introduction_twice_anchors.txt error_introduction_twice_anchors_unit.pas
mk_special_test ok_longcode_float_hex ok_longcode_float_hex.pas
mk_special_test ok_see_also ok_see_also.pas
mk_special_test ok_implicit_visibility_public ok_implicit_visibility.pas
mk_special_test ok_implicit_visibility_published --implicit-visibility=published ok_implicit_visibility.pas
mk_special_test ok_implicit_visibility_implicit_yes --implicit-visibility=implicit --visible-members=public,implicit ok_implicit_visibility.pas
mk_special_test ok_implicit_visibility_implicit_no  --implicit-visibility=implicit --visible-members=public          ok_implicit_visibility.pas
mk_special_test ok_bold_italic ok_bold_italic.pas
mk_special_test warning_inherited_test warning_inherited_test.pas
mk_special_test ok_preformatted_test ok_preformatted_test.pas
mk_special_test ok_dashes ok_dashes.pas
mk_special_test ok_lists ok_lists.pas
mk_special_test warning_lists warning_lists.pas
mk_special_test ok_table ok_table.pas
mk_special_test ok_table_nonlatex ok_table_nonlatex.pas
mk_special_test warning_table warning_table.pas
mk_special_test ok_macros ok_macros.pas
mk_special_test error_macros error_macros.pas
mk_special_test error_macros_recursive error_macros_recursive.pas
mk_special_test ok_macros_off --no-macro ok_macros_off.pas
mk_special_test ok_item_set_number ok_item_set_number.pas
mk_special_test error_unexpected_eof error_unexpected_eof.pas
mk_special_test error_unexpected_eof_2 error_unexpected_eof_2.pas
mk_special_test error_expected_semicolon error_expected_semicolon.pas
mk_special_test ok_record_case_parsing ok_record_case_parsing.pas
mk_special_test ok_record_with_case ok_record_with_case.pas
mk_special_test ok_multiple_fields ok_multiple_fields.pas
mk_special_test ok_back_comment ok_back_comment.pas
mk_special_test warning_back_comment warning_back_comment.pas
