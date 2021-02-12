unit ok_array_property;

interface

type
  TAbstractNode = class
  strict private
    function GetMetadataBooleanArray(const Key: String; const Index: Cardinal): Boolean;
    procedure SetMetadataBooleanArray(const Key: String; const Index: Cardinal; const Value: Boolean);
    function GetMetadataBoolean(const Key: String): Boolean;
    procedure SetMetadataBoolean(const Key: String; const Value: Boolean);
  public
    property MetadataBoolean[const Key: String]: Boolean read GetMetadataBoolean write SetMetadataBoolean;
    property MetadataBooleanArray[const Key: String; const Index: Cardinal]: Boolean read GetMetadataBooleanArray write SetMetadataBooleanArray;
  end;

implementation

end.
