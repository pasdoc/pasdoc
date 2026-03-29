{ @abstract(Test that "Out" can be used as an enum member name, Delphi allows this.) }
unit ok_out_enum;

interface

type
  TAnimationType = (&In, Out, InOut);

  TDirection = (Left, Right, Up, Down, &In, Out);

implementation

end.
