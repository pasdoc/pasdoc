unit ok_attributes;

interface

// Delphi attributes test from http://www.malcolmgroves.com/blog/?p=530

type
  // Attribute class, without Attribute suffix, PasDoc should link to it
  NonEmptyString = class(TCustomAttribute)
    constructor Create(const AMessage: String);
  end;

  // Attribute class, with Attribute suffix, PasDoc should link to it too
  MinimumIntegerAttribute = class(TCustomAttribute)
    constructor Create(const MinInt: Integer; const AMessage: String);
  end;

  // Attribute class, with Attribute suffix, PasDoc should link to it too
  MaximumIntegerAttribute = class(TCustomAttribute)
    constructor Create(const MaxInt: Integer; const AMessage: String);
  end;

  TPerson = class
  private
    FName: String;
    FAge: Integer;
  public
    [NonEmptyString('Must provide a Name')]
    property Name : String read FName write FName;
    [MinimumInteger(18, 'Must be at least 18 years old')]
    [MaximumInteger(65, 'Must be no older than 65 years')]
    property Age : Integer read FAge write FAge;
  end;

  // Test that GUIDs are handled gracefully
  IUIContainer = interface
  ['{0F0BA87D-95C3-4520-B9F9-CDF30015FDB3}']
  end;

// Tests from https://github.com/pasdoc/pasdoc/issues/179

  TMyClass1 = class
    [Volatile] FLockCount: Integer;
  end;

  TMyClass2 = class
  var
    [Volatile] FLockCount: Integer;
  end;

  IEnumerator<T> = interface(IEnumerator)
    [HPPGEN('virtual T __fastcall GetCurrentT(void) = 0')]
    function GetCurrent: T;
    [HPPGEN('__property T Current = {read=GetCurrentT}')]
    property Current: T read GetCurrent;
  end;

  { Class with nested classes with attributes.
    This testcase is based on TParallel from Delphi RTL declaration. }
  TClassWithNested = class sealed
  public type
    [Align(8)]
    TNestedClass = class sealed(TAncestor)
    public type
      [Align(8)]
      TNestedClass2 = class(TAncestor.TAncestorFlag)
      public
        [Align(8)]
        FLowestBreakIter: Int64;
        function InlineMethod: Int64; inline;
      protected
        constructor Create;
        function ShouldExit(ThisIter: Int64): Boolean; overload;
        function ShouldExit: Boolean; overload;
        property LowestBreakIter: Int64 read GetLowestBreakIter;
      end;
    end;

    [Align(8)]
    TAnotherNestedClass = class sealed(TAncestor)
    public type
      [Align(8)]
      TNestedClass2 = class(TAncestor.TAncestorFlag)
      public
        [Align(8)]
        FLowestBreakIter: Int64;
        [Align(8)]
        FAnotherLowestBreakIter: Int64;
      end;
      [Align(8)]
      TAnotherNestedClass2 = class(TAncestor.TAncestorFlag)
      public
        [Align(8)]
        FLowestBreakIter: Int64;
        [Align(8)]
        FAnotherLowestBreakIter: Int64;
      end;
    end;
  end;

implementation

end.
