{ @abstract(Test of highlighting in @@longCode some things.)

  See
  [https://sourceforge.net/tracker/?func=detail&atid=354213&aid=1422011&group_id=4213]

  @longCode(#
  TOptions = class(TForm)
  private
    procedure WMSysCmd(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
  ...
  end;
  #) }
unit ok_longcode_highlight;

interface

implementation

end.