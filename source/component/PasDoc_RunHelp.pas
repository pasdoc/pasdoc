unit PasDoc_RunHelp;

interface
uses
{$IFDEF LINUX}
  Libc;
{$ELSE}
  Windows;
{$ENDIF}

type
  TRunRecord = record // opaque record (platform dependent)
{$IFDEF LINUX}
    read, write: TFileDescriptor;
    pid: Integer;
{$ENDIF}
  end;

function RunProgram(const AName: string; args: array of string): TRunRecord;
procedure WriteLine(const ALine: string; const ARR: TRunRecord);
procedure CloseProgram(var ARR: TRunRecord);
function ReadLine(const ARR: TRunRecord): string;

implementation
uses
  SysUtils;

{$IFDEF LINUX}
function RunProgram(const AName: string; args: array of string): TRunRecord;
var
  p1, p2: TPipeDescriptors;
  pid: Integer;
  i: Integer;
  argarray: array of PChar;
begin
  pipe(p1);
  pipe(p2);
  
  pid := fork;

  case pid of
     0: begin
          setlength(argarray, length(args)+2);
          argarray[0] := pchar(AName);
          for i := 0 to high(args) do begin
            argarray[i+1] := PChar(args[i]);
          end;
          argarray[high(argarray)] := nil;

          for i := stderr_fileno+1 to sysconf(_SC_OPEN_MAX) do
            fcntl(i, F_SETFD, FD_CLOEXEC);

          __close(p1.ReadDes);
          __close(p2.WriteDes);
          __close(0);
          __close(1);
          __close(2);
          dup2(p1.WriteDes, 1);
          dup2(p2.ReadDes, 0);
          dup2(p1.WriteDes, 2);
          execv(PChar(AName), @argarray[0]);
          writeln('error: '+inttostr(errno));
          _Exit(1);
        end;
    -1: begin // error
          raise Exception.Create('Could not run program '+AName);
        end;
    else begin
      __close(p1.WriteDes);
      __close(p2.ReadDes);
      Result.read := p1.ReadDes;
      Result.write := p2.WriteDes;
      Result.pid := pid;
    end;
  end;
end;


procedure WriteLine(const ALine: string; const ARR: TRunRecord);
var
  c: char;
begin
  if ARR.Write <> -1 then begin
    c := #10;
    __write(ARR.write, ALine[1], Length(ALine));
    __write(ARR.write, c, 1);
  end;
end;

procedure CloseProgram(var ARR: TRunRecord);
var
  status: Integer;
begin
  __close(ARR.read);
  __close(ARR.write);
  waitpid(ARR.pid, @status, 0);
  ARR.read := -1;
  ARR.write := -1;
  ARR.pid := 0;
end;

function ReadLine(const ARR: TRunRecord): string;
var
  c: char;
begin
  result := '';
  if ARR.read <> -1 then begin
    c := #0;
    while (c <> #10) do begin
      __read(ARR.read, c, 1);
      Result := Result + c;
    end;
    SetLength(Result, Length(Result)-1);
  end;
end;
{$ELSE}
function RunProgram(const AName: string; args: array of string): TRunRecord;
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
