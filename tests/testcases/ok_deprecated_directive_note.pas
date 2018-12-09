{ -*- compile-command: "fpc -vw ok_deprecated_directive_note.pas" -*- }
{$ifdef FPC}{$mode objfpc}{$H+}{$endif}

{ @abstract(Test deprecated directive with and without note.) }
unit ok_deprecated_directive_note deprecated 'Deprecation note for unit';

interface

procedure MyProc1; deprecated 'Deprecation note for procedure with some apostrophe: '' here you go:)';
procedure MyProc2; deprecated;
procedure MyProc3; deprecated #72#$65'llo'; //< Deprecated note should say 'Hello'. Handled Ok, we convert and sum string tokens correctly.

type
  TTestClass = class
    TestFieldDeprecated1: Integer deprecated 'Deprecation note for field';
    TestFieldDeprecated2: Integer deprecated;
    procedure MyMethod1; deprecated 'Deprecation note for procedure';
    procedure MyMethod2; deprecated;
    property TestProperty1: Integer; deprecated 'Deprecation note for property';
    property TestProperty2: Integer; deprecated;
  end;

  TTestClassDeprecated1 = class
  end deprecated 'Deprecation note for class';
  TTestClassDeprecated2 = class
  end deprecated;

const
  TestConstDeprecated1 = 1 deprecated 'Deprecation note for constant';
  TestConstDeprecated2 = 1 deprecated;

implementation

procedure MyProc1; begin end;
procedure MyProc2; begin MyProc3; end;
procedure MyProc3; begin end;

procedure TTestClass.MyMethod1; begin end;
procedure TTestClass.MyMethod2; begin end;

end.
