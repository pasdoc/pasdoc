{$mode objfpc}

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
