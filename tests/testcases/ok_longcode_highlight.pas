{ @abstract(Test of highlighting in @@longCode some things.)

  See
  [https://sourceforge.net/tracker/?func=detail&atid=354213&aid=1422011&group_id=4213]

  @longCode(#
  TOptions = class(TForm)
  private
    procedure WMSysCmd(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
  ...
  end;
  #)

  See https://github.com/pasdoc/pasdoc/issues/123 :

  @longcode(#
  Somethins.Name:=1;
  I:=123;
  R:=0.001;
  R:=1E10;
  R:=R*0.99 - 9E-4;
  #)
}
unit ok_longcode_highlight;

interface

implementation

end.
