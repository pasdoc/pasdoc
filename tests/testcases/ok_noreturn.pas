{ @abstract(Test that the noreturn directive is accepted on procedures.)
  Delphi uses the noreturn directive to indicate that a procedure
  never returns to the caller.
  See @url(https://docwiki.embarcadero.com/RADStudio/Florence/en/Declarations_and_Statements_(Delphi)#The_noreturn_Directive
  Delphi docs). }
unit ok_noreturn;

interface

type
  TMyClass = class
  public
    procedure RaiseAlways; noreturn;

    procedure RaiseAlwaysOverloaded(const Msg: string); overload; noreturn;
    procedure RaiseAlwaysOverloaded(const Code: Integer); overload; noreturn;
  end;

procedure GlobalNoReturn; noreturn;

procedure ExternalNoReturn; stdcall; noreturn;

implementation

end.
