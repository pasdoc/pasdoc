(*
  @longcode(#
    { Some multiline
      comment }
  #)

  @longcode(
    { Some multiline
      comment, again }
  )

  Should be empty (without error):

  @longcode()

  Should be empty (without error):

  @longcode
*)

unit ok_longcode_comment;

interface

implementation

end.
