#!/bin/bash
set -eu

# This script creates tests output for particular pasdoc output format.
# $1 is the output format name. It's also the name of subdirectory where
# tests output will be placed.
#
# This script is meant to be called only from ../Makefile .

SORT_ALL='--sort=structures,constants,functions,types,variables,uses-clauses,record-fields,non-record-fields,methods,properties'
SORT_OLD='--sort=functions,record-fields,non-record-fields,methods,properties'

# functions ----------------------------------------

run_echo ()
{
  OUTPUT_FILENAME="$1"
  shift 1

  # too verbose now
  # echo 'Running:' "$@" '>' "$OUTPUT_FILENAME"

  # Temporary set +e, to ignore exit status from pasdoc
  set +e
  "$@" > "$OUTPUT_FILENAME"
  set -e
}

# Run pasdoc with given command-line parameters.
# First parameter is the name of subdirectory where we place test results
# (including PASDOC-OUTPUT file), this is always inside "$FORMAT" subdirectory.
mk_test ()
{
  OUTPUT_PATH=../testcases_output/"$FORMAT"/"$1"/
  shift 1

  mkdir -p "$OUTPUT_PATH"

  PASDOC_OUTPUT_FILENAME="$OUTPUT_PATH"PASDOC-OUTPUT

  run_echo "$PASDOC_OUTPUT_FILENAME" \
    "${PASDOC_BIN}" --format "$FORMAT" --exclude-generator \
    --output="$OUTPUT_PATH" "$@"
}

# Run all the test for current $FORMAT.
all_tests_for_current_format ()
{
  # Make a test of many units with normal pasdoc command-line.
  #
  # Note: most new tests should not be added here.
  # It was the 1st approach to just call something like `pasdoc *.pas' here
  # to test everything. But this was not good, because
  # 1. obviously you can't do tests with specialized pasdoc command-line options
  # 2. in case of output formats that create some "index" pages
  #    (like AllClasses.html in HTML / HtmlHelp output) small
  #    changes and additions have too global impact on many parts of
  #    documentation, so the differences between currect/new versions were
  #    harder to analyze by humans.
  #
  # Instead, usually you should add new test units as new calls to
  # `mk_test ...' lower in this script.
  #
  mk_test large_test "$SORT_OLD" \
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
  mk_test ok_const_1st_comment_missing --marker=: "$SORT_OLD" ok_const_1st_comment_missing.pas
  mk_test ok_link_1_char --visible-members 'private,public,published' "$SORT_OLD" ok_link_1_char.pas
  mk_test ok_auto_abstract --auto-abstract "$SORT_OLD" ok_auto_abstract.pas
  mk_test warning_incorrect_tag_nesting "$SORT_OLD"  warning_incorrect_tag_nesting.pas
  mk_test ok_param_raises_returns_proctype "$SORT_OLD" ok_param_raises_returns_proctype.pas
  mk_test ok_no_sort '--sort=functions,non-record-fields,methods,properties' ok_no_sort.pas
  mk_test ok_sorting_all "$SORT_ALL" ok_sorting.pas
  mk_test ok_sorting_none --sort= ok_sorting.pas
  mk_test ok_introduction_conclusion ok_introduction_conclusion.pas --introduction=ok_introduction.txt --conclusion=ok_conclusion.txt
  mk_test ok_introduction_conclusion_additional ok_introduction_conclusion.pas --introduction=ok_introduction.txt --conclusion=ok_conclusion.txt --additional ok_additionalfile1.txt -A ok_additionalfile2.txt
  mk_test ok_property_decl ok_property_decl.pas
  mk_test ok_multiple_vars ok_multiple_vars.pas
  mk_test ok_class_function ok_class_function.pas
  mk_test ok_latex_head --latex-head=ok_latex_head.tex ok_latex_head.pas
  mk_test ok_longcode_underscores ok_longcode_underscores.pas
  mk_test ok_longcode_comment ok_longcode_comment.pas
  mk_test ok_longcode_dash ok_longcode_dash.pas
  mk_test ok_longcode_special_chars ok_longcode_special_chars.pas
  mk_test error_introduction_twice_anchors_unit --introduction=error_introduction_twice_anchors.txt error_introduction_twice_anchors_unit.pas
  mk_test ok_longcode_float_hex ok_longcode_float_hex.pas
  mk_test ok_see_also ok_see_also.pas
  mk_test ok_implicit_visibility_public ok_implicit_visibility.pas
  mk_test ok_implicit_visibility_published --implicit-visibility=published ok_implicit_visibility.pas
  mk_test ok_implicit_visibility_implicit_yes --implicit-visibility=implicit --visible-members=public,implicit ok_implicit_visibility.pas
  mk_test ok_implicit_visibility_implicit_no  --implicit-visibility=implicit --visible-members=public          ok_implicit_visibility.pas
  mk_test ok_bold_italic ok_bold_italic.pas
  mk_test warning_inherited_test warning_inherited_test.pas
  mk_test ok_preformatted_test ok_preformatted_test.pas
  mk_test ok_dashes ok_dashes.pas
  mk_test ok_lists ok_lists.pas
  mk_test warning_lists warning_lists.pas
  mk_test ok_table ok_table.pas
  mk_test ok_table_nonlatex ok_table_nonlatex.pas
  mk_test warning_table warning_table.pas
  mk_test ok_macros ok_macros.pas
  mk_test error_macros error_macros.pas
  mk_test error_macros_recursive error_macros_recursive.pas
  mk_test ok_macros_off --no-macro ok_macros_off.pas
  mk_test ok_item_set_number ok_item_set_number.pas
  mk_test error_unexpected_eof error_unexpected_eof.pas
  mk_test error_unexpected_eof_2 error_unexpected_eof_2.pas
  mk_test error_expected_semicolon error_expected_semicolon.pas
  mk_test ok_record_case_parsing ok_record_case_parsing.pas
  mk_test ok_record_with_case ok_record_with_case.pas
  mk_test ok_multiple_fields ok_multiple_fields.pas
  mk_test ok_back_comment ok_back_comment.pas
  mk_test warning_back_comment warning_back_comment.pas
  mk_test ok_auto_link --auto-link ok_auto_link.pas
  mk_test ok_introduction_pre_link ok_introduction_pre_link_unit.pas --introduction=ok_introduction_pre_link.txt
  mk_test ok_table_of_contents ok_table_of_contents_unit.pas --introduction=ok_table_of_contents.txt
  mk_test warning_link_in_seealso warning_link_in_seealso.pas
  mk_test ok_caret_character ok_caret_character.pas
  mk_test ok_unit_uses_filename --write-uses-list ok_unit_uses_filename.pas
  mk_test ok_enum_explicit_assign ok_enum_explicit_assign.pas
  mk_test ok_if_directive ok_if_directive.pas
  mk_test ok_include_environment ok_include_environment.pas
  mk_test ok_enum_explicit_values ok_enum_explicit_values.pas
  mk_test ok_description_test --description ok_description_test.txt ok_description_test.pas
  mk_test ok_program --write-uses-list ok_program.pas
  mk_test ok_operator_test ok_operator_test.pas
  mk_test error_line_number_2 error_line_number_2.pas
  mk_test ok_dispid_method ok_dispid_method.pas
  mk_test ok_longcode_highlight ok_longcode_highlight.pas
  mk_test ok_non_matching_paren ok_non_matching_paren.pas
  mk_test ok_image ok_image.pas
  mk_test ok_include --introduction=ok_include_intro.txt ok_include.pas
  mk_test ok_class_var ok_class_var.pas
  mk_test error_line_number_3 error_line_number_3.pas
  mk_test ok_not_defined_omit ok_not_defined_omit.pas
  mk_test ok_abstract_sealed ok_abstract_sealed.pas
  mk_test ok_library ok_library.dpr
  mk_test ok_static_member ok_static_member.pas
  mk_test ok_strict_visibilities --visible-members protected,public,strictprotected,strictprivate ok_strict_visibilities.pas
  mk_test ok_weird_record ok_weird_record.dpr
  mk_test ok_include_quoted ok_include_quoted.pas
  mk_test ok_relative_include test_subdir/ok_relative_include_1.pas test_subdir/another_test_subdir/ok_relative_include_2.pas
  mk_test ok_list_item_set_number ok_list_item_set_number.pas
  mk_test ok_enum_field_var ok_enum_field_var.pas
  mk_test ok_back_comment_private ok_back_comment_private.pas
  mk_test warning_back_comment_class warning_back_comment_class.pas
  mk_test ok_excluded_unit ok_excluded_unit.pas
  mk_test ok_comment_over_uses_clause ok_comment_over_uses_clause.pas warning_back_comment_over_uses_clause.pas
  mk_test ok_interface_implicit ok_interface_implicit.pas --implicit-visibility=implicit
  mk_test ok_dot_unitname ok_dot_unitname.pas --write-uses-list
  mk_test ok_longcode_end_semicolon ok_longcode_end_semicolon.pas
  mk_test warning_desc_end warning_desc_end.pas
  mk_test ok_different_image_same_filename ok_different_image_same_filename_dir1/unit1.pas ok_different_image_same_filename_dir2/unit2.pas
  mk_test ok_enumeration_auto_abstract ok_enumeration_auto_abstract.pas --auto-abstract
  mk_test ok_ignore_leading ok_ignore_leading.pas --ignore-leading=*
  mk_test ok_ignore_leading_star ok_ignore_leading_star.pas --ignore-leading=* --staronly
  mk_test ok_ignore_leading_hash ok_ignore_leading_hash.pas --ignore-leading=#
  mk_test ok_ignore_leading_length2 ok_ignore_leading_length2.pas --ignore-leading=#?
  mk_test ok_packed_class_object ok_packed_class_object.pas
  mk_test ok_vorbisfile ok_vorbisfile.pas
  mk_test ok_procedural_const ok_procedural_const.pas
  mk_test ok_deprecated_const_string ok_deprecated_const_string.pas
  mk_test ok_external_class_hierarchy ok_external_class_hierarchy.pas --external-class-hierarchy=ok_external_class_hierarchy.txt
  mk_test ok_anonymous_methods ok_anonymous_methods.pas
  mk_test ok_class_record_helpers ok_class_record_helpers.pas
  mk_test utf_bom_test ok_bom.pas error_bom_utf16_be.pas error_bom_utf16_le.pas error_bom_utf32_be.pas
  mk_test ok_cvar ok_cvar.pas
  mk_test ok_nested_types ok_nested_types.pas
  mk_test ok_generic ok_generic.pas
  mk_test ok_helpinsight_comments ok_helpinsight_comments.pas
  mk_test ok_attributes ok_attributes.pas
  mk_test ok_final ok_final.pas
  mk_test ok_at_character_in_verbatim ok_at_character_in_verbatim.pas
  mk_test ok_deprecated_directive_note ok_deprecated_directive_note.pas
  mk_test ok_enum_links ok_enum_links.pas
  mk_test ok_no_link_inside_class ok_no_link_inside_class.pas
  mk_test ok_param_raises_at_property ok_param_raises_at_property.pas
  mk_test ok_date ok_date.pas
  mk_test ok_if_expressions ok_if_expressions.pas
  mk_test ok_prefix_identifier ok_prefix_identifier.pas
}

# parse params ----------------------------------------

# Assume pasdoc is on $PATH, if PASDOC_BIN not set.
PASDOC_BIN="${PASDOC_BIN:-pasdoc}"

for FORMAT; do
  all_tests_for_current_format
done
