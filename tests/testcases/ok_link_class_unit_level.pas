{ This is a link from unit's description to an identifier
  inside the same unit: some procedure @link(Foo),
  some class @link(TBar). }
unit ok_link_class_unit_level;

interface

procedure Foo;

procedure Xyz;

type
  { These are links from class' description to an identifiers
    inside the same class. Note that @@links here first check
    *inside* the class, then outside (i.e. in the whole unit).
    That's why link to Foo below is a link to a method Foo
    inside this class, not to a global procedure Foo.
    Links inside the class: @link(Foo), @link(Sthg).

    Links outside of the class: @link(ok_link_class_unit_level.Foo),
    @link(Xyz). Note that I has to qualify Foo with unit's name
    and write "ok_link_class_unit_level.Foo" to get a link to
    procedure in the unit. Just like I would do in a Pascal code. }
  TBar = class
    procedure Foo;
    procedure Sthg;
  end;

implementation

end.