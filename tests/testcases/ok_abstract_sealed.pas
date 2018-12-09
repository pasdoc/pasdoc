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

procedure TAbstractClass.AProcedure;
begin

end;

{ TOrdinaryClass }

procedure TOrdinaryClass.AProcedure;
begin

end;

{ TSealedClass }

procedure TSealedClass.AProcedure;
begin

end;

end.