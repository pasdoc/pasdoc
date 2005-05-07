{ @abstract(This unit tests parsing platform, library and deprecated
  directives (called collectively "hint directives") by pasdoc.)

  I'm doing this testcase in order to fix bug submitted to tracker:
  [ 1196073 ] "some modifiers are not parsed"

  I want to implement at once handling these directives everywhere
  where Delphi/Kylix allows them. FPC doesn't support them (yet).

  Quoting Delphi help (from Kylix 3):
  "Hint directives can be applied to type declarations, variable declarations,
  class and structure declarations, field declarations within classes or
  records, procedure, function and method declarations, and unit declarations."

  Looking below you can see that the way how these hints are parsed
  by Delphi is pretty much stupid and non-consequent (sorry all Delphi
  lovers, but this is really a mess) :

  1. Between "unit UnitName" and hints you *mustn't* put any semicolon,
     and you *mustn't* put any semicolons between hints. @br
     Same thing for CIOs (Classes / Interfaces / Objects / Records). @br
     Same thing for CIOs fields. @br
     Same thing for variables. @br
     Same thing for constants.

  2. Between "procedure/function Name (...)" and hints you *must*
     put a semicolon, and semicolons between hints are allowed but
     not required. It seems that you can't specify "library" directive
     for procedures/functions -- why ? Probably because "library"
     is a keyword and Borland was unable to correctly modify it's compiler
     to parse such thing. But pasdoc parses library directive correctly.

  3. Between method and hints you *must* put a semicolon,
     and semicolon between hints is *required*.
     You can specify "library" directive for methods.

  I'm unable to figure out how to specify these hints for normal
  (non-structural) types. If anyone can
  - tell me how to specify hint directives for non-structural types or
  - explain why parsing these directives is so non-consequent in Delphi or
  - point me to some precise documentation by Borland specifying grammar
    rules with these directives
  ... then please send email about this to pasdoc-main mailing list
  (or directly to me, Michalis Kamburelis, <kambi@@users.sourceforge.net>,
  if your comments about this do not really concern pasdoc).
  I will be grateful.

  Contrary to most units in tests/, this unit *is* kept at compileable
  by Delphi/Kylix. That's because this unit is also a test whether we
  really specify here hint directives in the way parseable by Delphi/Kylix.
}

{$ifdef FPC}
  {$mode DELPHI}
{$endif}

unit ok_platform_hints platform library deprecated;

interface

{ }
procedure TestProcPlatform; platform;

{procedure TestProcLibrary; library;}

{ }
procedure TestProcDeprecated; deprecated;

{ }
procedure TestProcCombined(SomeParams: Integer);
  {library } deprecated platform;

{ }
function TestFuncPlatform: Integer; platform;

{function TestFuncLibrary: Integer; library;}

{ }
function TestFuncDeprecated: Integer; deprecated;

{ }
function TestFuncCombined(SomeParams: Integer): Integer;
  {library }
  deprecated; { <- this semicolon is allowed but is optional }
  platform;

type
  {TTestTypePlatform = Integer platform;}
  {TTestTypeLibrary = Integer library;}
  {TTestTypeDeprecated = Integer deprecated;}
  {TTestTypeCombined = Integer platform deprecated library;}

  { }
  TTestClassDeprecated = class
    TestFieldPlatform: Integer platform;
    TestFieldLibrary: Integer library;
    TestFieldDeprecated: Integer deprecated;
    TestFieldCombined: Integer library deprecated platform;

    procedure TestMethodLibrary; library;
    procedure TestMethodPlatform; platform;
    procedure TestMethodDeprecated; deprecated;
    procedure TestMethodCombined; library; deprecated; platform;
  end deprecated library;

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

procedure TTestClassDeprecated.TestMethodPlatform;
begin
end;

procedure TTestClassDeprecated.TestMethodDeprecated;
begin
end;

procedure TTestClassDeprecated.TestMethodCombined;
begin
end;

end.