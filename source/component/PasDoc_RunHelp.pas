unit PasDoc_RunHelp;

interface
uses
{$IFDEF LINUX}
  {$IFNDEF FPC}
  Libc;
  {$ELSE}
  linux;
  {$ENDIF}
{$ELSE}
  Windows;
{$ENDIF}

type
  TRunRecord = record // opaque record (platform dependent)
{$IFDEF LINUX}
{$ENDIF}
  end;

function RunProgram(const AName: string; args: string): TRunRecord;
procedure WriteLine(const ALine: string; const ARR: TRunRecord);
procedure CloseProgram(var ARR: TRunRecord);
function ReadLine(const ARR: TRunRecord): string;

implementation
uses
  SysUtils;

{$IFDEF FPC}
type
  TPipeDescriptors = record
    ReadDes, WriteDes: Integer;
  end;
{$ENDIF}

{$IFDEF LINUX}
function RunProgram(const AName: string; args: string): TRunRecord;
begin
end;


procedure WriteLine(const ALine: string; const ARR: TRunRecord);
begin
end;

procedure CloseProgram(var ARR: TRunRecord);
begin
end;

function ReadLine(const ARR: TRunRecord): string;
begin
end;
{$ELSE}
function RunProgram(const AName: string; args: string): TRunRecord;
begin
  raise Exception.Create('not implemented');
end;

procedure WriteLine(const ALine: string; const ARR: TRunRecord);
begin
  raise Exception.Create('not implemented');
end;

procedure CloseProgram(var ARR: TRunRecord);
begin
  raise Exception.Create('not implemented');
end;

function ReadLine(const ARR: TRunRecord): string;
begin
  raise Exception.Create('not implemented');
end;

{$ENDIF}
end.
