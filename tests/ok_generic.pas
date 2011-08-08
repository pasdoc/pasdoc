{ Tests of generics, both FPC and Delphi style. }
unit ok_generic;

interface

{ Units to include for Generics }
uses Generics.Collections, FGL;

type
  { FPC generics tests ------------------------------------------------------- }

  generic TMyList<T> = class(TFPSList)
  private
    type PT = ^T;
  public
    function Blah: PT;
  end;

  { }
  TGeometryAttrib = class
    Name: string;
    AType: TGeometryAttribType;
    Offset: Integer;
  end;
  TGeometryAttribsList2 = specialize TMyList<TALBuffersCache>;

  { You can also specialize and make a descendant at the same time. }
  TGeometryAttribsList = class(specialize TMyList<TGeometryAttrib>)
  public
    function Find(const Name: string): TGeometryAttrib;
  end;

  { Delphi generics tests ---------------------------------------------------- }

  { A simple Test-Object }
  TMyObject = class(TObject);

  TMyGenericList = class(TObjectList<TMyObject>)
   public
    // To Something here
   end;

  TMyNewGeneric<T1,T2> = class
  private
    type PT = ^T1;
  public
    function Blah: PT;
  end;

  { Sample for a generic with more than one type. TPair is a Key-Value-Relation }
  TAnotherGenericType = class(TDictionary<TObject,TObject>);

  { All standard Generics: }
  TArray = class end;
  TEnumerator<T> = class abstract end;
  TEnumerable<T> = class abstract end;
  TList<T> = class(TEnumerable<T>) end;
  TQueue<T> = class(TEnumerable<T>) end;
  TStack<T> = class(TEnumerable<T>) end;
  TPair<TKey,TValue> = record end;
  TDictionary<TKey,TValue> = class(TEnumerable<TPair<TKey,TValue>>) end;
  TObjectList<T: class> = class(TList<T>) end;
  TObjectQueue<T: class> = class(TQueue<T>) end;
  TObjectStack<T: class> = class(TStack<T>) end;
  TObjectDictionary<TKey,TValue> = class(TDictionary<TKey,TValue>) end;

implementation

uses Classes;

procedure SampleProc();
var
 TestList: TMyGenericList;
 TestList2: TObjectList<TStringList>; // also allowed for vars
begin
 TestList:=TMyGenericList.Create();
 TestList2:=TObjectList<TStringList>.Create();

 TestList.Free;
 TestList2.Free;
end;

end.
