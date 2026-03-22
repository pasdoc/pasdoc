{ @abstract(Test --inherited-members=... behavior when some ancestor's members
  are hidden by descendants.) }
unit ok_inherited_members_hide_ancestor;

{$ifdef FPC}
  // ObjFpc prevents duplicating field names in descendants,
  // but we want to deliberaetly test it
  //{$mode objfpc}{$H+}
  {$mode delphi}
{$endif}

interface

type
  TBase = class
    { Description of BaseField, MyField from TBase }
    BaseField, MyField: Integer;
    const
      { Description of MyConst from TBase }
      MyConst = 42;
    { Description of MyMethod from TBase }
    procedure MyMethod; virtual;
    { Description of BaseMethod from TBase }
    procedure BaseMethod; virtual;
    { Description of OverloadedMethod from TBase that takes String }
    procedure OverloadedMethod(const A: String); overload;
  end;

  TDerived = class(TBase)
    { Description of DerivedField, MyField from TDerived }
    DerivedField, MyField: String; // Hides TBase.MyField
    const
      { Description of MyConst from TDerived }
      MyConst = 123;
    { Description of MyMethod from TDerived }
    procedure MyMethod; override; // Overrides TBase.MyMethod
    { Description of DerivedMethod from TDerived }
    procedure DerivedMethod; virtual;
    { Description of OverloadedMethod from TDerived that takes Integer }
    procedure OverloadedMethod(const A: Integer); overload;
  end;

  TNewDerived = class(TDerived)
    { Description of NewDerivedField, MyField from TNewDerived }
    NewDerivedField, MyField: Cardinal; // Hides TBase.MyField
    const
      { Description of MyConst from TNewDerived }
      MyConst = 456;
    { Description of MyMethod from TNewDerived }
    procedure MyMethod; override; // Overrides TBase.MyMethod
    { Description of NewDerivedMethod from TNewDerived }
    procedure NewDerivedMethod; virtual;
    { Description of OverloadedMethod from TNewDerived that takes Boolean }
    procedure OverloadedMethod(const A: Boolean); overload;
  end;

implementation
end.