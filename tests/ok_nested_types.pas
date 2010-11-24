{ @abstract(Test case for Delphi nested types)
  @author(Arno Garrels <first name.name@nospamgmx.de>)

  @HTML(<a href="http://docwiki.embarcadero.com/RADStudio/en/Nested_Type_Declarations">
     http://docwiki.embarcadero.com/RADStudio/en/Nested_Type_Declarations</a><br>
     <a href="http://wiki.freepascal.org/class_extensions_examples">
     http://wiki.freepascal.org/class_extensions_examples</a><br><br>) }

unit ok_nested_types;

interface

type
  { @abstract(@name contains nested classes, records, types and constants.) }
  TOuterClass = class(TObject)
  private
    { Description of @name }
    FOuterPrivateField: Integer;
    type
      { @abstract(@name contains one nested class, type and constant.) }
      TInnerPrivateClass = class(TObject)
      public
        type
          { Description of @name }
          TInnerPublicInteger = type Integer;
          { @abstract(Description of @name) }
          TInnerInnerPublicClass = class(TObject)
          private
            { Description of @name }
            FInnerInnerPrivateField: string;
            { Description of @name }
            function InnerInnerPrivateFunc(AValue: Integer): string;
          public
            { Description of @name }
            property InnerInnerPublicProp: string read  FInnerInnerPrivateField
                                                  write FInnerInnerPrivateField;
          end;
      private
        const
          { Description of @name }
          InnerPrivateConst = 1;
        var
          { Description of @name }
          FInnerPrivateField: Integer;
          { Description of @name }
          class procedure InnerPrivateClassProc(const AValue: Integer); static;
      public
        { Description of @name }
        FInnerPublicField: Integer;
        { Description of @name }
        procedure InnerPublicProc;
      end;
      { Description of @name }
      TOuterPrivateInteger = Integer;
      { @abstract(Description of @name) }
      TInnerPrivateClassDescendant = class(TInnerPrivateClass)
      private
        FField: Integer;
      end;
  public
    const
      { Description of @name } 
      OuterConst1 = 'Blah1';
      { Description of @name }
      OuterConst2 = 'Blah2';
    { Description of @name }
    procedure OuterPublicProc;
    type
      { @name is a nested record }
      TInnerImplicitRecord = record
      public
        { Description of @name }  
        FInnerPublicRecField: Integer;
        { Description of @name } 
        procedure InnerPublicRecProc;
      end;  
  end;
  
const
  { Description of @name }
  GlobalConst = 123456;

implementation

{ TOuterClass }

procedure TOuterClass.OuterPublicProc;
begin

end;

{ TOuterClass.TInnerPrivateClass }

class procedure TOuterClass.TInnerPrivateClass.InnerPrivateClassProc(
  const AValue: Integer);
begin

end;

procedure TOuterClass.TInnerPrivateClass.InnerPublicProc;
begin

end;

{ TOuterClass.TInnerPrivateClass.TInnerInnerPublicClass }

function TOuterClass.TInnerPrivateClass.TInnerInnerPublicClass.InnerInnerPrivateFunc(
  AValue: Integer): string;
begin

end;

{ TOuterClass.TInnerImplicitRecord }

procedure TOuterClass.TInnerImplicitRecord.InnerPublicRecProc;
begin

end;

end.
