{ Test of using ancestor name qualified with unit name. }
unit ok_ancestor_qualified;

interface

type
  TCastleObjectStack = class(Contnrs.TObjectStack)
  private
    function GetCapacity: TListSize;
    procedure SetCapacity(const Value: TListSize);
  public
    property Capacity: TListSize read GetCapacity write SetCapacity;
  end;

  TSomethingElse = class(ok_ancestor_qualified.TCastleObjectStack)
  end;

implementation
end.