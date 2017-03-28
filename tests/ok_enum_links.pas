unit ok_enum_links;

interface

type
  { My enumerated type description. }
  TMyEnum = (
    { My enumerated value 1 description. }
    me1,
    { My enumerated value 2 description. }
    me2,
    { My enumerated value 3 description. }
    me3
  );

{$scopedenums on}

  { My enumerated type description. }
  TMyScopedEnum = (
    { My enumerated value 1 description. }
    mse1,
    { My enumerated value 2 description. }
    mse2,
    { My enumerated value 3 description. }
    mse3
  );

{ Test of links.

  @link(TMyEnum), @link(me1), @link(me2), @link(me3).

  @link(TMyScopedEnum),
  @link(TMyScopedEnum.mse1),
  @link(TMyScopedEnum.mse2),
  @link(TMyScopedEnum.mse3).

  @link(TMyScopedEnum),
  TODO: these should NOT work (scoped enum members namespace is tighter),
  but for now they are linked too:
  @link(mse1),
  @link(mse2),
  @link(mse3). }
procedure Foo;

implementation

end.