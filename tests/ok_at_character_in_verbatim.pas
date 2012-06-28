(* Test that @@( and @@) and @@@@ (see
  http://pasdoc.sipsolutions.net/SupportedTags and
  http://pasdoc.sipsolutions.net/TagsParametersMatching) work Ok
  inside all @@-tags. Including tags that don't interpret other @@-tags
  inside, like @@latex or @@html.

  @italic(This is a closing paren @) and open @( and "at" char @@ using italic font.)

  @latex({\bf This is a closing paren @) and open @( and "at" char @@ inside LaTeX.})

  @html(<b>This is a closing paren @) and open @( and "at" char @@ inside HTML.</b>)

  @longCode(#
    // This is a closing paren @) and open @( and "at" char @@ inside longcode.
  #)
*)
unit ok_at_character_in_verbatim;
interface
implementation
end.