{ @abstract(Helper unit for integrating tipue [http://www.tipue.com/]
  with pasdoc HTML output.) }

unit PasDoc_Tipue;

interface

{ Put this in <head> of page with search button. }
function TipueSearchButtonHead: string;

{ Put this in content of some page --
  this will place a form with search button. }
function TipueSearchButton: string;

{ Adds some additional files to html documentation, needed for tipue engine.

  OutputPath is our output path, where html output must be placed.
  Must end with PathDelim. }
procedure TipueAddFiles(const OutputPath: string);

implementation

uses Classes, SysUtils, Utils;

function TipueSearchButtonHead: string;
begin
  Result := '<script language="JavaScript1.3" ' +
    'type="text/javascript" src="tip_form.js"></script>';
end;

function TipueSearchButton: string;
begin
  { Based on tipue docs.
    I only changed "Go" to "Search" (this is more standard name) }
  Result :=
    '<form name="tip_Form" onsubmit="search_form(tip_Form);return false">' + LineEnding +
    '<input type="text" name="d">' + LineEnding +
    '<input type="submit" value="Search">' + LineEnding +
    '</form>' + LineEnding;
end;

procedure TipueAddFiles(const OutputPath: string);
const
  TipFormScript = {$I tip_form.js.inc};
  TipSearchScript = {$I tip_search.js.inc};
  TipResultsPage = {$I _tipue_results.html.inc};
  TipLogoImage : {$I tipue_b1.png.inc};
begin
  StringToFile(OutputPath + 'tip_search.js', TipSearchScript);
  StringToFile(OutputPath + 'tip_form.js', TipFormScript);
  StringToFile(OutputPath + '_tipue_results.html', TipResultsPage);
  DataToFile(OutputPath + 'tipue_b1.png', TipLogoImage);
end;

end.