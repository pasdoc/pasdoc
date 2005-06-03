unit PasDoc_StringPairVector;

interface

uses
  Classes,
  ObjectVector;

type
  TStringPair = class
    Name: string;
    Value: string;
    Data: Pointer;

    { Init Name and Value by @link(ExtractFirstWord) from S. }
    constructor CreateExtractFirstWord(const S: string);
  end;
  
  { This is a list of string pairs.
    This class contains only non-nil objects of class TStringPair.

    Using this class instead of TStringList (with it's Name and Value
    properties) is often better, because this allows both Name and Value
    of each pair to safely contain any special characters (including '=' 
    and newline markers). It's also faster, since it doesn't try to
    encode Name and Value into one string. }
  TStringPairVector = class(TObjectVector)
  private
    function GetItems(i: Integer): TStringPair;
    procedure SetItems(i: Integer; Item: TStringPair);
  public
    property Items[i: Integer]: TStringPair read GetItems write SetItems; default;
  
    { Returns all items Names and Values glued together.
      For every item, string Name + NameValueSepapator + Value is
      constructed. Then all such strings for every items all
      concatenated with ItemSeparator.
      
      Remember that the very idea of @link(TStringPair) and 
      @link(TStringPairVector) is that Name and Value strings
      may contain any special characters, including things you
      give here as NameValueSepapator and ItemSeparator.
      So it's practically impossible to later convert such Text
      back to items and Names/Value pairs. }
    function Text(const NameValueSepapator, ItemSeparator: string): string;
  end;

implementation

uses Utils;

{ TStringPair ---------------------------------------------------------------- }

constructor TStringPair.CreateExtractFirstWord(const S: string);
var 
  FirstWord, Rest: string;
begin
  ExtractFirstWord(S, FirstWord, Rest);
  Name := FirstWord;
  Value := Rest;
end;

{ TStringPairVector ---------------------------------------------------------- }

function TStringPairVector.GetItems(i: Integer): TStringPair;
begin
  Result := TStringPair(inherited Items[i]);
end;

procedure TStringPairVector.SetItems(i: Integer; Item: TStringPair);
begin
  inherited Items[i] := Item;
end;

function TStringPairVector.Text(
  const NameValueSepapator, ItemSeparator: string): string;
var 
  i: Integer;
begin
  if Count > 0 then
  begin
    Result := Items[0].Name + NameValueSepapator + Items[0].Value;
    for i := 1 to Count - 1 do
      Result := Result + ItemSeparator +
        Items[i].Name + NameValueSepapator + Items[i].Value;
  end;
end;

end.
