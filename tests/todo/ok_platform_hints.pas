{ @abstract(This unit tests parsing platform, library and deprecated
  directives by pasdoc)

  I'm doing this testcase in order to fix bug submitted to tracker:
  [ 1196073 ] some modifiers are not parsed

  I want to implement at once handling these directives everywhere
  where Delphi/Kylix allows them. FPC doesn't support them (yet).

  Quoting Delphi help (from Kylix 3):
  "Hint directives can be applied to type declarations, variable declarations,
  class and structure declarations, field declarations within classes or
  records, procedure, function and method declarations, and unit declarations."

  But I'm unable to figure out how to specify them for procedures,
  functions, normal types and methods. Anyone can improve this testcase,
  please ? Or point me to some precide documentation how such directives
  are parsed by Delphi ?
}

{$ifdef FPC}
  {$mode DELPHI}
{$endif}

unit ok_platform_hints platform library deprecated;

interface

procedure TestProcPlatform {platform};

procedure TestProcLibrary {library};

procedure TestProcDeprecated {deprecated};

procedure TestProcCombined(SomeParams: Integer)
  {library deprecated platform};

function TestFuncPlatform: Integer {platform};

function TestFuncLibrary: Integer {library};

function TestFuncDeprecated: Integer {deprecated};

function TestFuncCombined(SomeParams: Integer): Integer
  {library deprecated platform};

type
  TTestTypePlatform = Integer {platform};
  TTestTypeLibrary = Integer {library};
  TTestTypeDeprecated = Integer {deprecated};
  TTestTypeCombined = Integer {platform deprecated library};

  TTestClassDeprecated = class
    TestFieldPlatform: Integer platform;
    procedure TestMethodLibrary {library};
  end deprecated;

  TTestRecordDeprecated = record
    TestFieldPlatform: Integer platform;
  end deprecated;

var
  TestVarPlatform: Integer platform;
  TestVarLibrary: Integer library;
  TestVarDeprecated: Integer deprecated;
  TestVarCombined: Integer library deprecated platform;

const
  TestConstPlatform = 1 platform;
  TestConstLibrary = 2 library;
  TestConstDeprecated = 3 deprecated;
  TestConstCombined = 4 deprecated library platform;

implementation

procedure TestProcPlatform;
begin
end;

procedure TestProcLibrary;
begin
end;

procedure TestProcDeprecated;
begin
end;

procedure TestProcCombined(SomeParams: Integer);
begin
end;

function TestFuncPlatform: Integer;
begin
end;

function TestFuncLibrary: Integer;
begin
end;

function TestFuncDeprecated: Integer;
begin
end;

function TestFuncCombined(SomeParams: Integer): Integer;
begin
end;

procedure TTestClassDeprecated.TestMethodLibrary;
begin
end;

end.