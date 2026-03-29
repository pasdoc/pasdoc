unit ok_alias_string;
interface
type
  TRoot = type string;
  TFileName = type string;
  // TRecord = type record A, B: Integer end; // not allowed by Delphi

  { }
  TRoot2 = string;
  TFileName2 = string;
  TRecord2 = record A, B: Integer end;

implementation
end.