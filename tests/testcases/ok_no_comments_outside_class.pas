// Test that orthan comments do not break away from the class definition
unit ok_no_comments_outside_class;

interface

type
  // Empty class with a comment inside
  TEmptyCl = class
    // Must be ignored (not descend to TNoDescr1)
  end;

  TNoDescr1 = byte;

  // Empty record with a comment inside
  TEmptyRec = record
    // Must be ignored (not descend to TNoDescr2)
  end;

  TNoDescr2 = byte;

  // Empty class with nested empty class with a comment inside
  TEmptyClWithNested = class
  type
    TNestedEmptyCl = class
      // Must be ignored (not descend to TNoDescr3)
    end;
  end;

  TNoDescr3 = byte;

implementation

end.
 