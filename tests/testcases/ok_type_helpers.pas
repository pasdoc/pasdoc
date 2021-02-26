unit ok_type_helpers;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils;

type

  TMyType=(TM_1, TM2);

  TMyTypeHelper=type helper for TMyType

  end;

implementation

end.

