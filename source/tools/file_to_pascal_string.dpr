{ Reads text file $1 and writes Pascal source file $2,
  such that $2 file contains Pascal string that has the contents
  of file $1. 
  
  Useful if you want to "embed" some text file inside compiled
  Pascal program.

  Note it treats $1 as text file, so difference between various line endings
  is lost in $2 (line endings are always encoded using LineEnding constant). }

program file_to_pascal_string;

{$apptype CONSOLE}
{$ifdef FPC} {$mode objfpc} {$endif}
{$H+}

uses SysUtils;

var
  Src, Dest: TextFile;
  SrcFileName, DestFileName, S: string;
begin
  SrcFileName := ParamStr(1);
  DestFileName := ParamStr(2);

  Assign(Src, SrcFileName);
  Reset(Src);
  try
    Assign(Dest, DestFileName);
    Rewrite(Dest);
    try
      Writeln(Dest, '{ -*- buffer-read-only: t -*- }');
      Writeln(Dest, '{ DON''T EDIT -- this file was automatically generated from "'
        + SrcFileName + '" }');

      while not Eof(Src) do
      begin
        Readln(Src, S);
        S := StringReplace(S, '''', '''''', [rfReplaceAll]);
        Writeln(Dest, '''' + S + ''' + LineEnding + ');
      end;
      
      Writeln(Dest, '''''');
    finally CloseFile(Dest) end;
  finally CloseFile(Src) end;
end.