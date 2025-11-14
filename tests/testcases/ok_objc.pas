{ Objective-C/Objective-Pascal FPC syntax tests.
  See
  https://github.com/pasdoc/pasdoc/issues/220
  https://wiki.freepascal.org/FPC_PasCocoa
}
{$mode objfpc}
{$modeswitch objectivec2}
unit ok_objc;

interface

type
  TSomeCocoaClassObject = objcclass(NSObject)
    procedure SomeFunction(someparameter: SomeType); message 'SomeFunction:';
  end;

  NSSomeObject = objcclass(NSObject)
    procedure method(params: Integer); message 'method:';
    class procedure classmethod(para: char); override; // "message 'classmethod:'" not required, compiler will get this from the parent class
  end;

  NSView = objcclass external (NSResponder)
  private
    _subview  : id; // private field
  public
    function initWithFrame(rect : NSRect): id; message 'initWithFrame:';
    procedure addSubview(aview: NSView); message 'addSubview:';
    procedure setAutoresizingMask(mask: NSUInteger); message 'setAutoresizingMask:';
    procedure setAutoresizesSubviews(flag: LongBool); message 'setAutoresizesSubviews:';
    procedure drawRect(dirtyRect: NSRect); message 'drawRect:';
  end;

  MyView = objcclass(MSView)
  public
    data : Integer;
    procedure customMessage(dirtyRect: NSRect); message 'customMessage';
    procedure drawRect(dirtyRect: NSRect); override;
  end;

implementation

procedure MyView.customMessage(dirtyRect: NSRect);
begin
end;

procedure MyView.drawRect(dirtyRect: NSRect);
begin
end;

end.