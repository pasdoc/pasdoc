{ @abstract(This is test that comments "stick" to the items, even if the items
  are hidden because of @--visible-members value.)

  In the example below, note that @italic(MyField description)
  and @italic(MyField2 description) should @bold(not) be shown anywhere.
  In other words, @italic(MyField description) should not be accidentaly
  assigned to TBlah or TMyClass items. It should be assigned
  to MyField item --- and so, because MyField is private and thus hidden,
  @italic(MyField description) should also be hidden.
  
  As of 2005-12-25: This test doesn't pass.
  @italic(MyField2 description) is correctly just skipped,
  but @italic(MyField description) is incorrectly assigned to TBlah.
}

unit ok_comment_for_hidden_field;

interface

type
  TBlah = Integer;

  TMyClass = class
  private
    MyField: Integer; //< MyField description.
  public
    MyNextField: Integer;
  end;

  TBlah2 = Integer;

  TMyClass2 = class
  private
    // MyField2 description.
    MyField2: Integer;
  public
    MyNextField2: Integer;
  end;

implementation
