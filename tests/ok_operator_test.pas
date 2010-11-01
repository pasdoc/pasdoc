{$mode objfpc}
// Operator overloads Delphi and FPC
unit ok_operator_test;

interface

type
  { @abstract(Operator overloads declared within a record (Delphi 2006+)) } 
  TDelphiRec = record
    { Addition of two operands of type TDelphiRec }
    class operator Add(a, b: TDelphiRec): TDelphiRec;
    { Subtraction of type TDelphiRec }
    class operator Subtract(a, b: TDelphiRec): TDelphiRec;
    { Implicit conversion of an Integer to type TDelphiRec }
    class operator Implicit(a: Integer): TDelphiRec;
    { Implicit conversion of TDelphiRec to Integer }
    class operator Implicit(a: TDelphiRec): Integer;
    { Explicit conversion of a Double to TDelphiRec }
    class operator Explicit(a: Double): TDelphiRec;
  end;

  { @abstract(In this case, "Operator" is used as a normal Delphi identifier) }
  TMyClass = class
  private
    FOperator: string;
  public
    { In this case, "Operator" is used as a normal Delphi identifier,
      not as an ObjFpc keyword. PasDoc should tolerate this,
      for compatibility with Delphi and with FPC in $mode delphi. }
    property Operator: string read FOperator write FOperator;
  end;

  TMyType = record end;
  TMyType2 = record end;

{ In cases below, "operator" indicates FPC operator overloading
  feature. PasDoc should handle it correctly. }

{ Assignment operators,
  see [http://www.freepascal.org/docs-html/ref/refse57.html] }
{ }
Operator := (C : TMyType2) z : TMyType;

{ Arithmetic operators,
  see [http://www.freepascal.org/docs-html/ref/refse58.html] }
{ }
Operator + (c: TMyType; c1: TMyType) c2: TMyType;
Operator - (c: TMyType; c1: TMyType) c2: TMyType;
Operator * (c: TMyType; i: integer) c2: TMyType;
Operator / (A, B: TMyType): TMyType;
Operator ** (A, B: TMyType): TMyType;

{ Comparison operators,
  see [http://www.freepascal.org/docs-html/ref/refse59.html] }
{ }
operator = (const c, d: TMyType) : boolean;
operator < (const c, d: TMyType) : boolean;
operator > (const c, d: TMyType) : boolean;
operator <= (const c, d: TMyType) : boolean;
operator >= (const c, d: TMyType) : boolean;

{ Boolean operators overloading. Seem to be undocumented,
  but they are used e.g. by FPimage unit in FPC sources. }
{ }
operator or (const c,d:TMyType) : TMyType;
operator and (const c,d:TMyType) : TMyType;
operator xor (const c,d:TMyType) : TMyType;

implementation

class operator TDelphiRec.Add(a, b: TDelphiRec): TDelphiRec; begin end;
class operator TDelphiRec.Explicit(a: Double): TDelphiRec; begin end;
class operator TDelphiRec.Implicit(a: Integer): TDelphiRec; begin end;
class operator TDelphiRec.Implicit(a: TDelphiRec): Integer; begin end;
class operator TDelphiRec.Subtract(a, b: TDelphiRec): TDelphiRec; begin end;

Operator := (C : TMyType2) z : TMyType; begin end;
Operator + (c: TMyType; c1: TMyType) c2: TMyType; begin end;
Operator - (c: TMyType; c1: TMyType) c2: TMyType; begin end;
Operator * (c: TMyType; i: integer) c2: TMyType; begin end;
Operator / (A, B: TMyType): TMyType; begin end;
Operator ** (A, B: TMyType): TMyType; begin end;
operator = (const c, d: TMyType) : boolean; begin end;
operator < (const c, d: TMyType) : boolean; begin end;
operator > (const c, d: TMyType) : boolean; begin end;
operator <= (const c, d: TMyType) : boolean; begin end;
operator >= (const c, d: TMyType) : boolean; begin end;
operator or (const c,d:TMyType) : TMyType; begin end;
operator and (const c,d:TMyType) : TMyType; begin end;
operator xor (const c,d:TMyType) : TMyType; begin end;

end.
