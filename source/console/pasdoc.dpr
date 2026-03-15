{
  Copyright 1998-2026 PasDoc developers.

  This file is part of "PasDoc".

  "PasDoc" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "PasDoc" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "PasDoc"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ PasDoc is a documentation generator for Pascal code.
  See https://pasdoc.github.io/ for more information.

  This is the main console program file,
  which simply calls the Main procedure defined in the PasDoc_Main unit.
  This allows the PasDoc_Main unit to be processed by PasDoc itself,
  generating documentation for the main logic of the application.
}

program pasdoc;

{$ifdef MSWINDOWS}
  {$apptype CONSOLE}
{$endif}

uses
  PasDoc_Main in 'PasDoc_Main.pas',
  PasDoc_Aspell in '..\component\PasDoc_Aspell.pas',
  PasDoc_Base in '..\component\PasDoc_Base.pas',
  PasDoc_Gen in '..\component\PasDoc_Gen.pas',
  PasDoc_GenHtml in '..\component\PasDoc_GenHtml.pas',
  PasDoc_GenHtmlHelp in '..\component\PasDoc_GenHtmlHelp.pas',
  PasDoc_GenLatex in '..\component\PasDoc_GenLatex.pas',
  PasDoc_GenPHP in '..\component\PasDoc_GenPHP.pas',
  PasDoc_GenSimpleXML in '..\component\PasDoc_GenSimpleXML.pas',
  PasDoc_Hashes in '..\component\PasDoc_Hashes.pas',
  PasDoc_HierarchyTree in '..\component\PasDoc_HierarchyTree.pas',
  PasDoc_Items in '..\component\PasDoc_Items.pas',
  PasDoc_Languages in '..\component\PasDoc_Languages.pas',
  PasDoc_OptionParser in '..\component\PasDoc_OptionParser.pas',
  PasDoc_Parser in '..\component\PasDoc_Parser.pas',
  PasDoc_ProcessLineTalk in '..\component\PasDoc_ProcessLineTalk.pas',
  PasDoc_Reg in '..\component\PasDoc_Reg.pas',
  PasDoc_Scanner in '..\component\PasDoc_Scanner.pas',
  PasDoc_Serialize in '..\component\PasDoc_Serialize.pas',
  PasDoc_SortSettings in '..\component\PasDoc_SortSettings.pas',
  PasDoc_StreamUtils in '..\component\PasDoc_StreamUtils.pas',
  PasDoc_StringPairVector in '..\component\PasDoc_StringPairVector.pas',
  PasDoc_StringVector in '..\component\PasDoc_StringVector.pas',
  PasDoc_TagManager in '..\component\PasDoc_TagManager.pas',
  PasDoc_Tokenizer in '..\component\PasDoc_Tokenizer.pas',
  PasDoc_Types in '..\component\PasDoc_Types.pas',
  PasDoc_Utils in '..\component\PasDoc_Utils.pas',
  PasDoc_Versions in '..\component\PasDoc_Versions.pas',
  PasDoc_Tipue in '..\component\tipue\PasDoc_Tipue.pas';

begin
  Main;
end.
