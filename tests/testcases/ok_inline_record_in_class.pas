{ @abstract(Test that anonymous (declared right at variable declaration)
  record types inside nested classes work.) }
unit ok_inline_record_in_class;

interface

type
  TOuterClass = class abstract
  protected type
    TInnerClass = class

      FStatus:
        record
          A, B: Integer;
        end;

      FStatusWithCase:
        // just to make test harder, try also a record with case
        record
          A, B: Integer;
          case Boolean of
            False: (FiredEvent: Integer);
            True: (EventCount: Integer);
        end;

      constructor Create;
      destructor Destroy; override;
    end;

    // just for test, place also record in TOuterClass
    TInfo = record
      FInner: TInnerClass;
      FIndex: Integer;
    end;
  private
    FData: Integer;
  end;

implementation

end.
