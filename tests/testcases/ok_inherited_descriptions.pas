unit ok_inherited_descriptions;

interface

type
  { This description is only on @link(TClassA), @link(TClassB), @link(TClassC), and @link(TClassD). }
  TClassA = class
  private
    FValue: string;
  public
    { This description is only on @link(TClassA.Proc). }
    procedure Proc; virtual;
    { This description is only on @link(TClassA.Prop). }
    property Prop: string read FValue;
  end;

  { This description is only on @link(IInterfaceB). }
  IInterfaceB = interface
    { This description is only on @link(IInterfaceB.Proc). }
    procedure Proc; override;
    { This description is only on @link(IInterfaceB.Prop). }
    property Prop read FValue;
  end;

  TClassB = class(TClassA, IInterfaceB)
  public
    { This description is only on @link(TClassB.Proc), @link(TClassC.Proc) and @link(TClassD.Proc). }
    procedure Proc; override;
    { This description is only on @link(TClassB.Prop), @link(TClassC.Proc) and @link(TClassD.Proc). }
    property Prop read FValue;
  end;

  IInterfaceC = interface
    { This description is only on @link(TClassC.Proc) and @link(TClassD.Proc). }
    procedure Proc; override;
    { This description is only on @link(TClassC.Prop) and @link(TClassD.Prop). }
    property Prop read FValue;
  end;

  TClassC = class(TClassB, IInterfaceC)
  protected
    FOtherValue: string;
  public
    procedure Proc; override;
    { This description is only on @link(TClassC.NotInheritedProc). }
    procedure NotInheritedProc;
    property Prop read FValue;
    { This description is only on @link(TClassC.NotInheritedProp). }
    property NotInheritedProp: string read FOtherValue;
  end;

  TClassD = class(TClassC)
  public
    procedure Proc; override;
    procedure NotInheritedProc;
    property NotInheritedProp: string read FOtherValue;
  end;

implementation

end.