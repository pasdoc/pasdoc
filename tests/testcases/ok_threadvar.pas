{ Check parsing of threadvar sections }

unit ok_threadvar;

interface

threadvar
  A: Byte;

type
  TClass = class
  private
    threadvar fPriv: Byte;
  public
    threadvar fPub: Byte;
  end;

implementation

end.