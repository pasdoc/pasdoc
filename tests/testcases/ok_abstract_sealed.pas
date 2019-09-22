{$ifdef FPC} {$mode objfpc} {$endif}

// description of @name
unit ok_abstract_sealed;

interface

type
  // description of @name
  TOrdinaryClass = class (TObject)
  // description of @name
    var AVariable: integer;
  // description of @name
    procedure AProcedure;
  end;

  // description of @name
  TAbstractClass = class abstract (TObject)
  // description of @name
    var AVariable: integer;
  // description of @name
    procedure AProcedure;
  end;

  // description of @name
  TSealedClass = class sealed (TObject)
  // description of @name
    var AVariable: integer;
  // description of @name
    procedure AProcedure;
  end;

implementation

{ TAbstractClass }

// Override comment above so that "parse implementation section" mode wouldn't change a thing
{ }

procedure TAbstractClass.AProcedure;
begin

end;

{ TOrdinaryClass }

// Override comment above so that "parse implementation section" mode wouldn't change a thing
{ }

procedure TOrdinaryClass.AProcedure;
begin

end;

{ TSealedClass }

// Override comment above so that "parse implementation section" mode wouldn't change a thing
{ }

procedure TSealedClass.AProcedure;
begin

end;

end.