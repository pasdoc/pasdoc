{
@author(Richard B. Winston <rbwinst@usgs.gov>)

The main purpose of @name is to define GetHelpControl which is used to find
a control that has help for a control.

Contributions to this unit by Richard B. Winston are in the public domain.
As of Nov. 2, 2005, this entire unit is by Richard B. Winston.
}
unit HelpProcessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms;
  
{ @abstract(@name returns @true if it can find a control that has help starting
 its search with HelpRequestControl.  The control that it finds is
 returned in HasHelpControl.)

 In its search, the function checks the FocusControl property of
 TCustomStaticText and TCustomLabel, the Parent property of TControl,
 and finally Application.MainForm.

 @name works for both HelpType = htKeyword and HelpType = htContext.}
function GetHelpControl(const HelpRequestControl: TControl;
  out HasHelpControl: TControl): boolean;

implementation

type
  TCustomLabelCracker = class(TCustomLabel);

function GetHelpControl(const HelpRequestControl: TControl;
  out HasHelpControl: TControl): boolean;
var
  AControl: TControl;
  AlreadyTried: TList;
begin
  AControl := HelpRequestControl;
  HasHelpControl := nil;
  AlreadyTried := TList.Create;
  try
    while AControl <> nil do begin
      // Test if AControl has help.  If it does, set HasHelpControl
      // and exit;
      case AControl.HelpType of
        htKeyword:
          begin
            if AControl.HelpKeyword <> '' then
            begin
              HasHelpControl := AControl;
              Exit;
            end;
          end;
        htContext:
          begin
            if AControl.HelpContext <> 0 then
            begin
              HasHelpControl := AControl;
              Exit;
            end;
          end;
      else
        Assert(False);
      end;
      // AControl does not have help, find the next one to test.
      if (AControl is TCustomStaticText)
        and (TCustomStaticText(AControl).FocusControl <> nil) then
      begin
        AControl := TCustomStaticText(AControl).FocusControl;
      end
      else if (AControl is TCustomLabel)
        and (TCustomLabelCracker(AControl).FocusControl <> nil) then
      begin
        AControl := TCustomLabelCracker(AControl).FocusControl;
      end
      else if AControl.Parent <> nil then
      begin
        AControl := AControl.Parent;
      end
      else if (Application <> nil)
        and (AControl <> Application.MainForm)
        and (Application.MainForm <> nil) then
      begin
        AControl := Application.MainForm;
      end
      else
      begin
        // nothing left to test so quit.
        Exit;
      end;
      
      // If the FocusControl of a TCustomStaticText or TCustomLabel
      // refers back to itself either directly or indirectly the
      // while loop might never exit.  The following prevents that
      // from happening.
      if AlreadyTried.IndexOf(AControl) >= 0 then
      begin
        Exit;
      end
      else
      begin
        AlreadyTried.Add(AControl);
      end;
    end;
  finally
    AlreadyTried.Free;
    result := HasHelpControl <> nil;
  end;
end;

end.

