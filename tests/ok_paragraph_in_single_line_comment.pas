// This is the 1st paragraph.
//
// This is the 2nd paragraph.
//
// This is the 3rd paragraph.
//
// pasdoc should create paragraphs when glueing single-line comments to
// a description, but it doesn't for now. Update: now it does.
unit ok_paragraph_in_single_line_comment;

interface

{ This is the 1st paragraph.

  This is the 2nd paragraph.

  This is the 3rd paragraph.

  Here paragraphs are correct. }
procedure Foo;

implementation

end.