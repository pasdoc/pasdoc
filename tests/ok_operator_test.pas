unit ok_operator_test;

interface

type
  TMyClass = class
  private
    FOperator: string;
  public
    { In this case, "Operator" is used as a normal Delphi identifier,
      not as an ObjFpc keyword. PasDoc should tolerate this,
      for compatibility with Delphi and with FPC in $mode delphi. }
    property Operator: string read FOperator write FOperator;
  end;

{ In cases below, "operator" indicates FPC operator overloading
  feature. PasDoc should handle it correctly. }

{ Assignment operators,
  see [http://www.freepascal.org/docs-html/ref/refse57.html] }
{ }
Operator := (C : Complex) z : complex;

{ Arithmetic operators,
  see [http://www.freepascal.org/docs-html/ref/refse58.html] }
{ }
Operator + (c: IMoney; c1: IMoney) c2: IMoney;
Operator - (c: IMoney; c1: IMoney) c2: IMoney;
Operator * (c: IMoney; i: integer) c2: IMoney;
Operator / (A, B: TMyType): TMyType;
Operator ** (A, B: TMyType): TMyType;

{ Comparison operators,
  see [http://www.freepascal.org/docs-html/ref/refse59.html] }
{ }
operator = (const c, d: TFPColor) : boolean;
operator < (const c, d: TMyType) : boolean;
operator > (const c, d: TMyType) : boolean;
operator <= (const c, d: TMyType) : boolean;
operator >= (const c, d: TMyType) : boolean;

{ Boolean operators overloading. Seem to be undocumented,
  but they are used e.g. by FPimage unit in FPC sources. }
{ }
operator or (const c,d:TFPColor) : TFPColor;
operator and (const c,d:TFPColor) : TFPColor;
operator xor (const c,d:TFPColor) : TFPColor;

implementation

end.