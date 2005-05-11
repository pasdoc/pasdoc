unit ok_link_1_char;

interface

uses Classes;

type
  {@abstract(@name is used to specify the
   real-world coordinates of a pixel in a
   bitmap.)}
  TMeasurementPointItem = class(TCollectionItem)
  private
    // @name: double;
    // See @link(X).
    FX: double;
    // @name: double;
    // See @link(Y).
    FY: double;
    // See @link(X).
    procedure SetX(const Value: double);
    // See @link(Y).
    procedure SetY(const Value: double);
  published
    // @name is the X real-world coordinate.
    property X: double read FX write SetX;
    // @name is the Y real-world coordinate.
    property Y: double read FY write SetY;
  end;

implementation

end.
