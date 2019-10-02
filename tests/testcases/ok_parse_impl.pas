{ Parse implementation section.
  Must be tested with --implementation-comments=join --define PASDOC }
unit ok_parse_impl;

interface

uses IntfUnit;

{$ifdef PASDOC}
  {$define FPC}
  // {$define DCC}
{$endif}

// This is Foo (intf)
procedure Foo;
procedure Bar;
// This is Laz
procedure Laz;
procedure NoDescr;
// @exclude
procedure Ignored;

procedure Overloaded; overload;
// @exclude
procedure Overloaded(b: Char); overload;
procedure Overloaded(a: Byte); overload;

type
  TClass = class
  type
    TInnerClass = class
      procedure Method;
    private
      procedure Hidden;
    end;
  public
    constructor Create; overload;
    class function Create(const S: string): TClass; overload; static; inline;
    destructor Destroy;
    procedure Method;
    class operator Implicit(const S: string): TClass;
  end;

  TMyType = record end;

{$ifdef FPC}
Operator := (C : TMyType) z : TMyType;
{$endif}

// bugfix: this comment must not descend to "NoDescr"

implementation

{$ifdef PASDOC}
procedure NoDescr; // place it before "uses" because ParseUses clears comment too
begin end;
{$endif}

uses ImplUnit;

// type/var/const sections inside implementation section - they all must be ignored
{ }
type
  TInnerRec = record
    // must be ignored
  end;

  TInnerClass = class
    // must be ignored
  type
    TInnerSubClass = class
      // must be ignored
    end;
  end;

  {$ifdef DCC}
  TProc = reference to procedure ; //< lambda type with intentional whitespace after keyword
  TProc2 = reference to procedure (par: byte); //< lambda type with parameter
  {$endif}

var
  bla: byte;
  Rec: record
    // must be ignored
  end;
  proc: procedure;  //< proc keyword in var section

// This is Foo (impl)
procedure Foo;
// must be ignored

// lots of stuff inside proc...
{ }

type
  TRec = record
    // must be ignored
  end;

  TClass2 = class
    // must be ignored
  end;

  {$ifdef DCC}
  TProc = reference to procedure;
  TProc1 = reference to procedure ;
  TProc2 = reference to procedure (par: byte);
  {$endif}

  // Nested FooBar with some directives - must be ignored
  procedure FooBar; inline; deprecated 'foobar';
  // some stuff in nested proc
  var a: byte;
  type foo = byte;
  begin
  	writeln(0);
  	// check nested begin - end
  	begin
  	end;
  end;

var
  bla: byte;
  Rec: record
    // must be ignored
  end;
  lambda: TProc;
  lambda2: TProc2;
  proc: procedure;  
begin
  // must be ignored
  writeln(0);
  // many nested blocks...
  begin
    // must be ignored
  end;
  asm
    // must be ignored
  end;
  case bla of
    1:;// must be ignored
  end;
  try try
    // must be ignored
  except end;  
  finally end;
  
  {$ifdef DCC}
  lambda := procedure
    begin
      // must be ignored
    end;
  
  lambda2 := procedure(par: byte)
    begin
      // must be ignored
    end;
  
  lambda2 := procedure (par: byte)
    begin
      // must be ignored
    end;
  {$endif}
end;

// This is Bar (impl)
procedure Bar;
begin
end;

// This is Laz
// And it must not be doubled
procedure Laz;
begin
end;

// This is internal routine and must be hidden
procedure Hidden;
begin
end;

// This routine is ignored in intf section
procedure Ignored;
begin
end;

// This is overloaded proc #1
procedure Overloaded;
begin
end;

// This is overloaded proc #3 that must be ignored
procedure Overloaded(b: Char);
begin
end;

// This is overloaded proc #2
procedure Overloaded(a: Byte);
begin
end;

{ TClass }

// Creates instance of TClass
constructor TClass . Create; // whitespace is intentional
begin
end;

// Static factory
class function TClass.Create(const S: string): TClass;
begin
end;

// Destroys instance of TClass
destructor TClass.Destroy;
// some stuff inside
var
  bla: byte;
type
  TRec = record
    // must be ignored
  end;
begin
end;

// Does something
procedure TClass.Method;
begin
end;

// Assignment operator
class operator TClass.Implicit(const S: string): TClass;
begin
end;

{ TClass.TInnerClass }

// Method of an inner class
procedure TClass.TInnerClass.Method;
begin
end;

// Hidden method of an inner class
procedure TClass.TInnerClass.Hidden;
begin
end;

{$ifdef FPC}
// This is assignment operator
Operator := (C : TMyType2) z : TMyType;
begin
end;
{$endif}

end     . // whitespace is intentional

// Text after END must be ignored
procedure Boo;
