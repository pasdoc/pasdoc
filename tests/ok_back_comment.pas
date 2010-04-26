{$ifdef FPC} {$mode objfpc} {$endif}

unit ok_back_comment;

{< Description of unit ok_back_comment. }

interface

uses SysUtils;

type
  TMyType = Integer; {< Description of TMyType }

  TMyClass = class
    MyField: Integer; //< Description of MyField
    procedure MyProc; //< Description of MyProc
    property MyProp: Integer read MyField write MyField; //< Description of MyProp
  end; {< Description of TMyClass }

  TMyException1 = class(Exception); //< Description of TMyException1
  TMyException2 = class(Exception); //< Description of TMyException2

  TMyEnum = (
  {< Description of TMyEnum }
    meOne, //< Description of meOne
    { Description of meTwo }
    meTwo,
    meThree (*< Description of meThree*)
  );

procedure Foo;
{< Description of Foo, (@name) }

var
  V: Integer; //< Description of V
  V1, V2: Integer; //< Description of V1 and V2

const
  MyConst = 2; //< Description of MyConst

implementation

procedure TMyClass.MyProc; begin end;
procedure Foo; begin end;

end.
