unit GenericExample;

interface

{ Unit to include for Generics }
uses Generics.Collections;

type
 { A simple Test-Object }
 TMyObject = class(TObject);

 TMyGenericList = class(TObjectList<TMyObject>)
  public
   // To Something here
  end;

 { Sample for a generic with more than one type. TPair is a Key-Value-Relation }
 TAnotherGenericType = class(TDictionary<TObject,TObject>);

 { All standard Generics:
   TArray = class
   TEnumerator<T> = class abstract
   TEnumerable<T> = class abstract
   TList<T> = class(TEnumerable<T>)
   TQueue<T> = class(TEnumerable<T>)
   TStack<T> = class(TEnumerable<T>)
   TPair<TKey,TValue> = record
   TDictionary<TKey,TValue> = class(TEnumerable<TPair<TKey,TValue>>)
   TObjectList<T: class> = class(TList<T>)
   TObjectQueue<T: class> = class(TQueue<T>)
   TObjectStack<T: class> = class(TStack<T>)
   TObjectDictionary<TKey,TValue> = class(TDictionary<TKey,TValue>)

   The placeholder <T> is only necessary when designing own generics. Specializing existing
   types replace <T> with <DesiredType>.

   Quick and usable solution would be: Let the class name of TMyGenericList be
   "TObjectList<TMyObject>" and just reference to TObjectList.
  }




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
