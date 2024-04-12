{
  Copyright 1998-2022 PasDoc developers.

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

{ @abstract(Parse ObjectPascal code.)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Michalis Kamburelis)
  @author(Arno Garrels <first name.name@nospamgmx.de>)

  Contains the @link(TParser) object, which can parse an ObjectPascal
  code, and put the collected information into the TPasUnit instance. }

unit PasDoc_Parser;

{$I pasdoc_defines.inc}

interface

uses SysUtils, Classes, Contnrs, StrUtils,
  PasDoc_Types,
  PasDoc_Items,
  PasDoc_Scanner,
  PasDoc_Tokenizer,
  PasDoc_StringPairVector,
  PasDoc_StringVector;

type
  { Raised when an impossible situation (indicating bug in
    pasdoc) occurs. }
  EInternalParserError = class(Exception);

  TItemParseMode = (pmUndefined, pmConst, pmVar, pmType);

  { @name stores a CIO reference and current state. }
  TPasCioHelper = class(TObject)
  private
    FCio: TPasCio;
    FCurVisibility: TVisibility;
    FMode: TItemParseMode;
    FSkipCioDecl: Boolean;
  public
    { Frees included objects and calls its own destructor. Objects are not
      owned by default. }
    procedure FreeAll;
    property Cio: TPasCio read FCio write FCio;
    property CurVisibility: TVisibility read FCurVisibility write FCurVisibility;
    property Mode: TItemParseMode read FMode write FMode;
    property SkipCioDecl: Boolean read FSkipCioDecl write FSkipCioDecl;
  end;

  { A stack of @link(TPasCioHelper) objects currently used to parse nested
    classes and records }
  TPasCioHelperStack = class(TObjectStack)
  public
    { Frees all items including their CIOs and clears the stack }
    procedure Clear;
    function Push(AHelper: TPasCioHelper): TPasCioHelper;
      {$IFDEF USE_INLINE} inline; {$ENDIF}
    function Pop: TPasCioHelper; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function Peek: TPasCioHelper; {$IFDEF USE_INLINE} inline; {$ENDIF}
  end;

  // @name stores a series of @link(TRawDescriptionInfo TRawDescriptionInfos).
  // It is modelled after TStringList but has only the minimum number
  // of methods required for use in PasDoc.
  TRawDescriptionInfoList = class(TObject)
  private
    // @name holds the @link(TRawDescriptionInfo TRawDescriptionInfos) in @classname
    FItems: array of TRawDescriptionInfo;
    // @name holds the number of items currently stored in @classname.
    // @seealso(Count).
    FCount: integer;
    // @name is the read specifier for @link(Items)
    function GetItems(Index: integer): TRawDescriptionInfo;
    // @name expands the capacity of @link(FItems).
    procedure Grow;
  public
    // @name adds a new @link(TRawDescriptionInfo) to @classname.
    function Append(Comment: TRawDescriptionInfo): integer;
    // @name is the number of @link(TRawDescriptionInfo TRawDescriptionInfos) in
    // @classname.
    property Count: integer read FCount;
    Constructor Create;
    // @name provides read access to the
    // @link(TRawDescriptionInfo TRawDescriptionInfos) in @classname.
    property Items[Index: integer]: TRawDescriptionInfo read GetItems; default;
  end;

  TOwnerItemType = (otUnit, otCio);

  { Parser class that will process a complete unit file and all of its
    include files, regarding directives.
    When creating this object constructor @link(Create) takes as an argument
    an input stream and a list of directives.
    Parsing work is done by calling @link(ParseUnitOrProgram) method.
    If no errors appear, should return a @link(TPasUnit) object with
    all information on the unit. Else exception is raised.

    Things that parser inits in items it returns:

    @unorderedList(
      @item(Of every TPasItem :
        Name, RawDescription, Visibility, HintDirectives, DeprecatedNote,
        FullDeclararation (note: for now not all items
        get sensible FullDeclararation, but the intention is to improve this
        over time; see @link(TPasItem.FullDeclaration) to know where
        FullDeclararation is available now).

        Note to IsDeprecated: parser inits it basing on hint directive
        "deprecated" presence in source file; it doesn't handle the fact
        that @@deprecated tag may be specified inside RawDescription.

        Note to RawDescription: parser inits them from user's comments
        that preceded given item in source file.
        It doesn't handle the fact that @@member and @@value tags
        may also assign RawDescription for some item.)

      @item Of TPasCio: Ancestors, Fields, Methods, Properties, MyType.

      @item Of TPasEnum: Members, FullDeclararation.

      @item Of TPasRoutine: What.

      @item Of TPasVarConst: FullDeclaration.

      @item(Of TPasProperty: IndexDecl, FullDeclaration.
        PropType, NoDefault, Stored, DefaultValue, Reader, Writer.
        TODO: Parsing TPasProperty.DefaultInClass.)

      @item(Of TPasUnit; UsesUnits, Types, Variables, CIOs, Constants,
        FuncsProcs.)
    )

    It doesn't init other values.
    E.g. AbstractDescription or DetailedDescription of TPasItem
    should be inited while expanding this item's tags.
    E.g. SourceFileDateTime and SourceFileName of TPasUnit must
    be set by other means. }

  TParser = class
  private
    FImplicitVisibility: TImplicitVisibility;

    FCioSk : TPasCioHelperStack;

    { Last comment found in input.
      This only takes into account normal comments, i.e. not back-comments.
      Modified by @link(GetLastComment) and @link(PeekNextToken)
      (and consequently by all @link(PeekNextToken) and @link(GetNextToken)
      versions).

      LastCommentContent is only the comment content, with comment braces
      and markers already stripped. }
    IsLastComment: boolean;
    LastCommentWasCStyle, LastCommentHelpInsight: boolean;
    LastCommentInfo: TRawDescriptionInfo;

    AttributeIsPossible: boolean;
    CurrentAttributes: TStringPairVector;

    { The underlying scanner object. }
    Scanner: TScanner;

    FOnMessage: TPasDocMessageEvent;
    FVerbosity: Cardinal;
    FCommentMarkers: TStringList;
    FIgnoreMarkers: TStringList;
    FMarkersOptional: boolean;
    FIgnoreLeading: string;
    FShowVisibilities: TVisibilities;
    FAutoBackComments: boolean;
    FInfoMergeType: TInfoMergeType;

    { These are the items that the next "back-comment"
      (the comment starting with "<", see
      [https://pasdoc.github.io/WhereToPlaceComments]
      section "Placing comments after the item") will apply to. }
    ItemsForNextBackComment: TPasItems;

    { Returns @link(TRoutineType) value for corresponding @link(TKeyWord) value.
      @raises(EInternalParserError
        If given KeyWord has no corresponding @link(TRoutineType) value.) }
    function KeyWordToRoutineType(KeyWord: TKeyWord): TRoutineType;

    procedure DoError(const AMessage: string;
      const AArguments: array of const);
    procedure DoMessage(const AVerbosity: Cardinal; const MessageType:
      TPasDocMessageType; const AMessage: string; const AArguments: array of const);

    { Checks if T.MyType is ATokenType, if not calls DoError
      with appropriate error mesg. }
    procedure CheckToken(T: TToken; ATokenType: TTokenType); overload;

    { Checks if T.IsSymbol(ASymbolType), if not calls DoError
      with appropriate error mesg. }
    procedure CheckToken(T: TToken; ASymbolType: TSymbolType); overload;

    { Checks if T.IsKeyWord(AKeyWord), if not calls DoError
      with appropriate error mesg. }
    procedure CheckToken(T: TToken; AKeyWord: TKeyWord); overload;

    { Replaces HelpInsight XML tags like <summary> with PasDoc tags }
    procedure ExpandHelpInsightDescriptions(var DescriptionInfo: TRawDescriptionInfo);

    { Remove Lazarus %region declarations from a description,
      see http://wiki.freepascal.org/IDE_Window:_Editor_Options_Code_Folding#About_.7B.25Region.7D }
    procedure RemoveRegionDeclarations(var DescriptionInfo: TRawDescriptionInfo);

    { If not IsLastComment, then returns @link(EmptyRawDescriptionInfo)
      otherwise returns LastCommentInfo and sets IsLastComment to false. }
    function GetLastComment: TRawDescriptionInfo;

    { Reads tokens and throws them away as long as they are either whitespace
      or comments.

      Sets WhitespaceCollector to all the whitespace that was skipped.
      (Does @italic(not) append them to WhitespaceCollector,
      it @italic(sets) WhitespaceCollector to them, deleting previous
      WhitespaceCollector value.)
      The overloaded version with WhitespaceCollectorItem
      @italic(appends) whitespace to WhitespaceCollectorItem.FullDeclaration
      (unless WhitespaceCollectorItem is @nil).
      Remember that whitespace is always consumed --- unlike the token itself
      (which is not "eaten" by peek operation),
      whitespace is consumed, so you @italic(must) capture it here, or lose it.

      Comments are collected to [Is]LastCommentXxx properties, so that you can
      use GetLastComment.

      Returns non-white token that was found.
      This token is equal to @code(Scanner.PeekToken).
      Note that this token was @italic(peeked)
      from the stream, i.e. the caller is still responsible for doing
      @code(Scanner.ConsumeToken).
      Calling this method twice in a row will return the same thing.

      Always returns something non-nil (will raise exception in case
      of problems, e.g. when stream ended). }
    function PeekNextToken(out WhitespaceCollector: string): TToken; overload;
    function PeekNextToken(const WhitespaceCollectorItem: TPasItem): TToken; overload;

    { Same thing as PeekNextToken(Dummy) }
    function PeekNextToken: TToken; overload;

    { Just like @link(PeekNextToken), but returned token is already consumed.
      Next call to @name will return next token. }
    function GetNextToken(out WhitespaceCollector: string): TToken; overload;

    { Just like @link(PeekNextToken), but returned token is already consumed.

      Moreover, whitespace collected is appended to
      WhitespaceCollectorItem.FullDeclaration
      (does not delete previous WhitespaceCollectorItem.FullDeclaration value,
      it only appends to it).
      Unless WhitespaceCollectorItem is @nil. }
    function GetNextToken(const WhitespaceCollectorItem: TPasItem): TToken; overload;

    function GetNextToken: TToken; overload;
    function GetNextTokenNotAttribute(const WhitespaceCollectorItem: TPasItem): TToken; overload;

    { This does @link(GetNextToken), then checks is it a ATokenType
      (using @link(CheckToken)), then frees the token.
      Returns token Data.
      Just a comfortable routine. }
    function GetAndCheckNextToken(ATokenType: TTokenType): string; overload;
    { This is an overload for parsing a unit name that may contain one or
      more dots ('.')
      @param AIsUnitName is a dummy parameter to allow overloading that is assumed to be true }
    function GetAndCheckNextToken(ATokenType: TTokenType; AIsUnitname: boolean): string; overload;

    { This does @link(GetNextToken), then checks is it a symbol with
      ASymbolType (using @link(CheckToken)), then frees the token.
      Returns token Data.
      Just a comfortable routine. }
    function GetAndCheckNextToken(ASymbolType: TSymbolType): string; overload;

    { This does @link(GetNextToken), then checks is it a keyword with
      AKeyWord (using @link(CheckToken)), then frees the token.
      Returns token Data.
      Just a comfortable routine. }
    function GetAndCheckNextToken(AKeyWord: TKeyWord): string; overload;

    { Parses a constructor, a destructor, a function or a procedure
      or an operator (for FPC).
      Resulting @link(TPasRoutine) item will be returned in M.

      ClassKeywordString contains the keyword 'class'
      in the exact spelling as it was found in input,
      for class methods. Else it contains ''.

      RoutineTypeString contains the keyword 'constructor', 'destructor',
      'function' or 'procedure' or standard directive 'operator'
      in the exact spelling as it was found in input.
      You can specify RoutineTypeString = '', this way you avoid including
      such keyword at the beginning of returned M.FullDeclaration.

      RoutineType is used for the What field of the resulting TPasRoutine.
      This should correspond to RoutineTypeString.

      D may contain a description or nil. }
    procedure ParseRoutine(out M: TPasRoutine;
      const ClassKeywordString: string;
      const RoutineTypeString: string; RoutineType: TRoutineType;
      const RawDescriptionInfo: TRawDescriptionInfo;
      const NeedName: boolean; InitItemsForNextBackComment: boolean;
      const IsGeneric: String);

    { Parses a class, an interface or an object.
      U is the unit this item will be added to on success.
      N is the name of this item.
      CIOType describes if item is class, interface or object.
      D may contain a description or nil. }
    procedure ParseCIO(const U: TPasUnit;
      const CioName, CioNameWithGeneric: string; CIOType: TCIOType;
      const RawDescriptionInfo: TRawDescriptionInfo;
      const IsInRecordCase: boolean);

    procedure ParseCioEx(const U: TPasUnit;
      const CioName, CioNameWithGeneric: string; CIOType: TCIOType;
      const RawDescriptionInfo: TRawDescriptionInfo;
      const IsInRecordCase: boolean);

    function ParseCioMembers(const ACio: TPasCio; var Mode: TItemParseMode;
      const IsInRecordCase: Boolean; var Visibility: TVisibility): Boolean;

    { Assume that T is "<" symbol, and parse everything up to a matching ">".
      Append everything (including this "<") to Content string.
      At the end, T is freed and nil. }
    procedure ParseGenericTypeIdentifierList(var T: TToken; var Content: string);

    procedure ParseCioTypeDecl(out ACio: TPasCio;
      const CioName, CioNameWithGeneric: string; CIOType: TCIOType;
      const RawDescriptionInfo: TRawDescriptionInfo; var Visibility: TVisibility);

    procedure ParseRecordCase(const R: TPasCio; const SubCase: boolean);
    procedure ParseConstant(OwnerItemType: TOwnerItemType; out Constant: TPasItem);

    { This parses type, var or const section that doesn't belong to a CIO
      (unit intf section, unit impl section, inside a standalone routine).
      This assumes that next token is a keyword starting the section.
      Method stops when it encounters a keyword that is not part of
      type/variable/constant declaration.
      U is optional unit object. If it's assigned, parsed items will be added
      to corresponding list. If it's @nil, items will be just parsed and
      immediately disposed. }
    procedure ParseTVCSection(U: TPasUnit);

    procedure ParseInterfaceSection(const U: TPasUnit);
    procedure ParseImplementationSection(const U: TPasUnit);
    procedure ParseProperty(out p: TPasProperty);
    procedure ParseType(const U: TPasUnit; IsGeneric: String);

    { This assumes that you just read left parenthesis starting
      an enumerated type. It finishes parsing of TPasEnum,
      returning is as P. }
    procedure ParseEnum(out p: TPasEnum; const Name: string;
      const RawDescriptionInfo: TRawDescriptionInfo);
    { Parse an alias type, assuming the "type" token has just been read. }
    procedure ParseStrongAlias(out P: TPasAliasType; const Name: string;
      const RawDescriptionInfo: TRawDescriptionInfo);

    procedure ParseUses(const U: TPasUnit);

    { This parses the sequence of identifiers separated by commas
      and ended by symbol FinalSymbol. More specifically in EBNF it parses
        TOK_IDENTIFIER (SYM_COMMA TOK_IDENTIFIER)+ FinalSymbol
      FinalSymbol must be something else than SYM_COMMA.
      After executing this, next token (returned by GetNextToken and PeekNextToken)
      will point to the next token right after FinalSymbol.
      All found identifiers will be appended to Names.

      If RawDescriptions <> nil then this will also get
      all comments documenting the identifiers in Names
      (it will append the same number of items to
      RawDescriptions as it appended to Names).
      The strategy how comments are assigned to item in this case is
      described on [https://pasdoc.github.io/WhereToPlaceComments]
      (see section "Multiple fields/variables in one declaration"). }
    procedure ParseCommaSeparatedIdentifiers(Names: TStrings;
      FinalSymbol: TSymbolType;
      RawDescriptions: TRawDescriptionInfoList);

    procedure ParseVariables(const U: TPasUnit);

    { Parse one variables or fields clause
      ("one clause" is something like
        NAME1, NAME2, ... : TYPE;
      i.e. a list of variables/fields sharing one type declaration.)

      @param(Items If Items <> nil then it adds parsed variables/fields to Items.)
      @param(Visibility will be assigned to Visibility of
        each variable/field instance.)
      @param(IsInRecordCase indicates if we're within record's case.
        It's relevant only if OfObject is true.) }
    procedure ParseFieldsVariables(Items: TPasItems;
      OfObject: boolean; Visibility: TVisibility; IsInRecordCase: boolean;
      const ClassKeyWordString: string = '');

    { Read all tokens until you find a semicolon at brace-level 0 and
      end-level (between "record" and "end" keywords) also 0.

      Alternatively, also stops before reading "end" without beginning
      "record" (so it can handle some cases where declaration doesn't end
      with semicolon).

      Alternatively, only if IsInRecordCase, also stops before reading
      ')' without matching '('. That's because fields' declarations
      inside record case may be terminated by just ')' indicating
      that this case clause terminates, without a semicolon.

      If you pass Item <> nil then all read data will be
      appended to Item.FullDeclaration. Also hint directives
      (Item.HintDirectives, Item.DeprecatedNote) may be set (to true/non-empty) if appropriate
      hint directive will occur in source file. }
    procedure SkipDeclaration(const Item: TPasItem; IsInRecordCase: boolean);

    procedure SetCommentMarkers(const Value: TStringList);

    procedure SetIgnoreMarkers(const Value: TStringList);

    { Consume a hint directive (platform, library or deprecated) as long as you
      see one. Skips all whitespace and comments.
      Sets the Item.HintDirectives and Item.DeprecatedNote as necessary.

      Stops when PeekNextToken returns some token that is not a whitespace,
      comment or hint directive.

      If ConsumeFollowingSemicolon then we will also look for, and consume,
      a semicolon following (any one of) the hint directives. This is a little
      hazy, but parsing rules for hint directives *are* hazy, the semicolon
      sometimes is optional and sometimes required, see tests/ok_hint_directives.pas
      testcase.

      If ExtendFullDeclaration then the hint directives (and eventual semicolons,
      if ConsumeFollowingSemicolon) will also be added to the Item.FullDeclaration). }
    procedure ParseHintDirectives(Item: TPasItem;
      const ConsumeFollowingSemicolon: boolean = false;
      const ExtendFullDeclaration: boolean = false);

    procedure ParseUnit(U: TPasUnit);
    procedure ParseProgram(U: TPasUnit);
    procedure ParseProgramOrLibraryUses(U: TPasUnit);
    procedure ParseLibrary(U: TPasUnit);
    procedure AddDirectives(const Directives: TStringVector);

  public
    { Create a parser, initialize the scanner with input stream S.
      All strings in SD are defined compiler directives. }
    constructor Create(
      const InputStream: TStream;
      const Directives: TStringVector;
      const IncludeFilePaths: TStringVector;
      const OnMessageEvent: TPasDocMessageEvent;
      const VerbosityLevel: Cardinal;
      const AStreamName, AStreamPath: string;
      const AHandleMacros: boolean);

    { Release all dynamically allocated memory. }
    destructor Destroy; override;

    { This does the real parsing work, creating U unit and parsing
      InputStream and filling all U properties. }
    procedure ParseUnitOrProgram(var U: TPasUnit);

    property OnMessage: TPasDocMessageEvent read FOnMessage write FOnMessage;
    property CommentMarkers: TStringList read FCommentMarkers write SetCommentMarkers;
    property MarkersOptional: boolean read fMarkersOptional write fMarkersOptional;
    property IgnoreLeading: string read FIgnoreLeading write FIgnoreLeading;
    property IgnoreMarkers: TStringList read FIgnoreMarkers write SetIgnoreMarkers;
    property ShowVisibilities: TVisibilities
      read FShowVisibilities write FShowVisibilities;

    { See command-line option @--implicit-visibility documentation at
      [https://pasdoc.github.io/ImplicitVisibilityOption] }
    property ImplicitVisibility: TImplicitVisibility
      read FImplicitVisibility write FImplicitVisibility;
    { See command-line option @--auto-back-comments documentation at
      [https://pasdoc.github.io/AutoBackComments] }
    property AutoBackComments: boolean read FAutoBackComments write FAutoBackComments;
    { Whether to read comments from the implementation,
      and how to merge them with the interface comments. }
    property InfoMergeType: TInfoMergeType read FInfoMergeType write FInfoMergeType;
  end;

implementation

uses
  {$ifdef FPC_RegExpr} RegExpr, {$endif}
  {$ifdef DELPHI_RegularExpressions} RegularExpressions, {$endif}
  PasDoc_Utils, PasDoc_Hashes;

{ Extend full Pascal declaration of something by NextToken.
  Makes sure to delimit by space if necessary. }
function AppendDeclaration(const FullDeclaration, NextToken: String): String;
const
  NonSymbol = ['0'..'9', 'a'..'z', 'A'..'Z', '_'];
var
  LastFullDeclaration, FirstNextToken: Char;
begin
  if FullDeclaration <> '' then
    LastFullDeclaration := FullDeclaration[Length(FullDeclaration)]
  else
    LastFullDeclaration := #0;

  if NextToken <> '' then
    FirstNextToken := NextToken[1]
  else
    FirstNextToken := #0;

  if LastFullDeclaration in [':', ';'] then
    { Put space after ':' to make type declarations like "const Key: String" in FullDeclaration. }
    Result := FullDeclaration + ' ' + NextToken
  else
  if (LastFullDeclaration in NonSymbol) and
     (FirstNextToken in NonSymbol) then
    { Separate 2 non-symbols by space.
      This way e.g. parsing "property MetadataBoolean[const Key: String]" results in FullDeclaration
      with space between "const" and "Key". }
    Result := FullDeclaration + ' ' + NextToken
  else
    Result := FullDeclaration + NextToken;
end;

{ ---------------------------------------------------------------------------- }
{ TParser }
{ ---------------------------------------------------------------------------- }

constructor TParser.Create(
  const InputStream: TStream;
  const Directives: TStringVector;
  const IncludeFilePaths: TStringVector;
  const OnMessageEvent: TPasDocMessageEvent;
  const VerbosityLevel: Cardinal;
  const AStreamName, AStreamPath: string;
  const AHandleMacros: boolean);
begin
  inherited Create;
  FOnMessage := OnMessageEvent;
  FVerbosity := VerbosityLevel;

  Scanner := TScanner.Create(InputStream, OnMessageEvent,
    VerbosityLevel, AStreamName, AStreamPath, AHandleMacros);
  AddDirectives(Directives);
  Scanner.IncludeFilePaths := IncludeFilePaths;
  FCommentMarkers := TStringlist.Create;
  FIgnoreMarkers := TStringlist.Create;
  ItemsForNextBackComment := TPasItems.Create(false);
  FCioSk := TPasCioHelperStack.Create;
  CurrentAttributes := TStringPairVector.Create(true);
end;

{ ---------------------------------------------------------------------------- }

destructor TParser.Destroy;
begin
  CurrentAttributes.Free;
  FCommentMarkers.Free;
  FIgnoreMarkers.Free;
  Scanner.Free;
  ItemsForNextBackComment.Free;
  FCioSk.Free;
  inherited;
end;

{ ---------------------------------------------------------------------------- }

function TParser.KeyWordToRoutineType(KeyWord: TKeyWord): TRoutineType;
begin
  case KeyWord of
    KEY_CONSTRUCTOR: Result := ROUTINE_CONSTRUCTOR;
    KEY_DESTRUCTOR:  Result := ROUTINE_DESTRUCTOR;
    KEY_FUNCTION:    Result := ROUTINE_FUNCTION;
    KEY_PROCEDURE:   Result := ROUTINE_PROCEDURE;
  else
    raise EInternalParserError.Create('KeyWordToRoutineType: invalid keyword');
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.DoError(const AMessage: string;
  const AArguments: array of const);
begin
  raise EPasDoc.Create(Scanner.GetStreamInfo + ': ' + AMessage, AArguments, 1);
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.DoMessage(const AVerbosity: Cardinal; const MessageType:
  TPasDocMessageType; const AMessage: string; const AArguments: array of const);
begin
  if (AVerbosity <= FVerbosity) and Assigned(FOnMessage) then
    FOnMessage(MessageType, Format(AMessage, AArguments), AVerbosity);
end;

procedure TParser.ExpandHelpInsightDescriptions(var DescriptionInfo: TRawDescriptionInfo);

  {$ifdef FPC_RegExpr}
  function ReplaceRegEx(const AInputStr, ARegExpr, AReplaceStr: string): string;
  begin
    Result := ReplaceRegExpr(ARegExpr, AInputStr, AReplaceStr, true);
  end;
  {$else}
  {$ifdef DELPHI_RegularExpressions}
  function ReplaceRegEx(const AInputStr, ARegExpr, AReplaceStr: string): string;
  begin
    Result := TRegEx.Replace(AInputStr, ARegExpr, AReplaceStr);
  end;
  {$else}
  { No regular expressions support, help insight comments will not work. }
  function ReplaceRegEx(const AInputStr, ARegExpr, AReplaceStr: string): string;
  begin
    Result := AInputStr;
  end;
  {$endif}
  {$endif}

var s: string;
begin
  s := DescriptionInfo.Content;
  s := ReplaceRegEx(s, '<summary[^>]*>', '@abstract(');
  s := ReplaceRegEx(s, '</summary>', ')');
  { handle <param.. before <para.., otherwise <para.. would match <param.. too }
  s := ReplaceRegEx(s, '<param[ \t]+name[ \t]*=[ \t]*"([^"]*)"[ \t]*>', '@param($1 ');
  s := ReplaceRegEx(s, '</param>', ')'+LineEnding + LineEnding);
  s := ReplaceRegEx(s, '<para[^>]*>', LineEnding + LineEnding);
  s := ReplaceRegEx(s, '</para>', LineEnding + LineEnding);
  s := ReplaceRegEx(s, '<returns[ ]*([^>]*)>', '@returns($1');
  s := ReplaceRegEx(s, '</returns>', ')');
  s := ReplaceRegEx(s, '<exception[ \t]+cref[ \t]*=[ \t]*"([^"]*)"[ \t]*>', '@raises($1 ');
  s := ReplaceRegEx(s, '<exception[ ]*([^>]*)>', '@raises($1');
  s := ReplaceRegEx(s, '</exception>', ')');
  s := ReplaceRegEx(s, '<permission[ ]*([^>]*)>', '@permission($1');  //not yet implemented
  s := ReplaceRegEx(s, '</permission>', ')');
  s := ReplaceRegEx(s, '<c>', '@code(');
  s := ReplaceRegEx(s, '</c>', ')');
  s := ReplaceRegEx(s, '<code>', '@preformatted(');
  s := ReplaceRegEx(s, '</code>', ')');
  s := ReplaceRegEx(s, '<b>', '@bold(');
  s := ReplaceRegEx(s, '</b>', ')');
  s := ReplaceRegEx(s, '<strong>', '@bold(');
  s := ReplaceRegEx(s, '</strong>', ')');
  s := ReplaceRegEx(s, '<i>', '@italic(');
  s := ReplaceRegEx(s, '</i>', ')');
  s := ReplaceRegEx(s, '<em>', '@italic(');
  s := ReplaceRegEx(s, '</em>', ')');
  s := ReplaceRegEx(s, '<u>', '@underline(');  // not yet implemented
  s := ReplaceRegEx(s, '</u>', ')');
  s := ReplaceRegEx(s, '<br */?>', '@br');
  s := ReplaceRegEx(s, '<ul>', '@unorderedList(');
  s := ReplaceRegEx(s, '</ul>', ')');
  s := ReplaceRegEx(s, '<ol>', '@orderedList(');
  s := ReplaceRegEx(s, '</ol>', ')');
  s := ReplaceRegEx(s, '<li>', '@item(');
  s := ReplaceRegEx(s, '</li>', ')');
  s := ReplaceRegEx(s, '<remark>', '');
  s := ReplaceRegEx(s, '</remark>', '');
  s := ReplaceRegEx(s, '<remarks>', '');
  s := ReplaceRegEx(s, '</remarks>', '');
  s := ReplaceRegEx(s, '<comment>', '');
  s := ReplaceRegEx(s, '</comment>', '');
  s := ReplaceRegEx(s, '<exclude[^/]*/>', '@exclude');
  s := ReplaceRegEx(s, '<see[ \t]+cref[ \t]*=[ \t]*"([^"]*)"[ \t]*/>', '@link($1)');
  s := ReplaceRegEx(s, '<see[ \t]+cref[ \t]*=[ \t]*"([^"]*)"[ \t]*>', '@link($1 ');
  s := ReplaceRegEx(s, '</see>', ')');
  s := ReplaceRegEx(s, '<seealso[ \t]+cref[ \t]*=[ \t]*"([^"]*)"[ \t]*/>', '@seealso($1)');
  s := ReplaceRegEx(s, '<seealso[ \t]+cref[ \t]*=[ \t]*"([^"]*)"[ \t]*>', '@seealso($1 ');
  s := ReplaceRegEx(s, '</seealso>', ')');
  DescriptionInfo.Content := s;
end;

procedure TParser.RemoveRegionDeclarations(var DescriptionInfo: TRawDescriptionInfo);
begin
  if IsPrefix('%region /fold', DescriptionInfo.Content) then
    DescriptionInfo.Content := RemovePrefix('%region /fold', DescriptionInfo.Content)
  else
  if IsPrefix('%region', DescriptionInfo.Content) then
    DescriptionInfo.Content := RemovePrefix('%region', DescriptionInfo.Content)
  else
  if IsPrefix('%endregion', DescriptionInfo.Content) then
    DescriptionInfo.Content := RemovePrefix('%endregion', DescriptionInfo.Content);
end;

{ ---------------------------------------------------------------------------- }

const
  SExpectedButFound = '%s expected but %s found';

procedure TParser.CheckToken(T: TToken; ATokenType: TTokenType);
begin
  if T.MyType <> ATokenType then
    DoError(SExpectedButFound,
      [TOKEN_TYPE_NAMES[ATokenType], T.Description]);
end;

procedure TParser.CheckToken(T: TToken; ASymbolType: TSymbolType);
begin
  if not T.IsSymbol(ASymbolType) then
    DoError(SExpectedButFound,
      [Format('symbol "%s"', [SymbolNames[ASymbolType]]), T.Description]);
end;

procedure TParser.CheckToken(T: TToken; AKeyWord: TKeyWord);
begin
  if not T.IsKeyWord(AKeyWord) then
    DoError(SExpectedButFound,
      [Format('reserved word "%s"', [LowerCase(KeyWordArray[AKeyWord])]),
      T.Description]);
end;

{ ---------------------------------------------------------------------------- }

function TParser.GetLastComment: TRawDescriptionInfo;
begin
  if IsLastComment then
  begin
    if LastCommentHelpInsight then
      ExpandHelpInsightDescriptions(LastCommentInfo);
    RemoveRegionDeclarations(LastCommentInfo);
    Result := LastCommentInfo;
    IsLastComment := false;
  end else
    Result := EmptyRawDescriptionInfo;
end;

{ ---------------------------------------------------------------------------- }

function TParser.GetNextToken(out WhitespaceCollector: string): TToken;
begin
  Result := PeekNextToken(WhitespaceCollector);
  Scanner.ConsumeToken;
end;

function TParser.GetNextToken(const WhitespaceCollectorItem: TPasItem): TToken;
begin
  Result := PeekNextToken(WhitespaceCollectorItem);
  Scanner.ConsumeToken;
end;

function TParser.GetNextToken: TToken;
begin
  Result := PeekNextToken;
  Scanner.ConsumeToken;
end;

function TParser.GetNextTokenNotAttribute(const WhitespaceCollectorItem: TPasItem): TToken;
var
  OldAttributeIsPossible: Boolean;
begin
  OldAttributeIsPossible := AttributeIsPossible;
  AttributeIsPossible := False;
  Result := GetNextToken(WhitespaceCollectorItem);
  AttributeIsPossible := OldAttributeIsPossible;
end;

{ ---------------------------------------------------------------------------- }

function TParser.GetAndCheckNextToken(ATokenType: TTokenType): string;
var
  T: TToken;
begin
  T := GetNextToken;
  try
    CheckToken(T, ATokenType);
    Result := T.Data;
  finally
    T.Free;
  end;
end;

function TParser.GetAndCheckNextToken(ATokenType: TTokenType; AIsUnitname: boolean): string;
var
  T: TToken;
  s: string;
begin
  // note: the Value of AIsUnitName is ignored, assume it is true
  Result := '';
  while true do begin
    T := GetNextToken;
    try
      CheckToken(T, ATokenType);
      Result := Result + T.Data;
    finally
      T.Free;
    end;
    t := PeekNextToken(s);
    if (s = '') and t.IsSymbol(SYM_PERIOD) then begin
      Result := Result + '.';
      Scanner.ConsumeToken;
      t.Free;
    end else
      exit;
  end;
end;

function TParser.GetAndCheckNextToken(ASymbolType: TSymbolType): string;
var
  T: TToken;
begin
  T := GetNextToken;
  try
    CheckToken(T, ASymbolType);
    Result := T.Data;
  finally
    T.Free;
  end;
end;

function TParser.GetAndCheckNextToken(AKeyWord: TKeyWord): string;
var
  T: TToken;
begin
  T := GetNextToken;
  try
    CheckToken(T, AKeyWord);
    Result := T.Data;
  finally
    T.Free;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseRoutine(out M: TPasRoutine;
  const ClassKeywordString: string;
  const RoutineTypeString: string; RoutineType: TRoutineType;
  const RawDescriptionInfo: TRawDescriptionInfo;
  const NeedName: boolean; InitItemsForNextBackComment: boolean;
  const IsGeneric: String);

  procedure ReadNestedName;
  var t: TToken;
  begin
    t := nil;
    repeat
      FreeAndNil(t);
      t := GetNextToken;
      if (t.MyType = TOK_IDENTIFIER) or t.IsSymbol(SYM_PERIOD) then
        M.Name := M.Name + t.Data
      else
      // Whitespaces are allowed ("function TClass . Foo"), just skip them
      if t.MyType = TOK_WHITESPACE then
        // skip
      else
      begin
        Scanner.UnGetToken(t);
        Break;
      end;
    until False;
  end;

  { Reads tokens (adding them to M.FullDeclaration) until a semicolon
    (on parenthesis level zero) is found (this final semicolon
    is also read and appended to M.FullDeclaration). }
  procedure ReadTokensUntilSemicolon;
  var
    T: TToken;
    Level: Integer;
    IsSemicolon: Boolean;
  begin
    Level := 0;
    repeat
      T := Scanner.GetToken;
      try
        if T.MyType = TOK_WHITESPACE then
        begin
          { add exactly *one space* at the end of M.FullDeclaration }
          if Length(M.FullDeclaration) > 0 then
          begin
            if (M.FullDeclaration[Length(M.FullDeclaration)] <> ' ') then
              M.FullDeclaration := M.FullDeclaration + ' ';
          end;
        end else
        if not (T.MyType in TokenCommentTypes) then
          M.FullDeclaration := M.FullDeclaration + T.Data;

        if T.IsSymbol(SYM_LEFT_PARENTHESIS) then Inc(level);
        if T.IsSymbol(SYM_RIGHT_PARENTHESIS) then Dec(level);
        IsSemicolon := T.IsSymbol(SYM_SEMICOLON);
      finally
        FreeAndNil(T);
      end;
    until IsSemicolon and (Level = 0);
  end;

  function IsParameterIdentifier(Token: TToken): Boolean;
  begin
    if Token.MyType = TOK_IDENTIFIER then
      Result := True
    else if Token.MyType = TOK_KEYWORD then
    begin
      Result := Token.Info.KeyWord in [KEY_STRING, KEY_ARRAY, KEY_SET, KEY_OF, KEY_FILE];
    end
    else
      Result := False;
  end;

  procedure ReadParameters;
  var
    T: TToken;
    Level: Integer;
    IsSemicolon: Boolean;
    ParamsInGroup: Integer;
    TypeNamePartStarted: Boolean;
    TypeNamePartEnded: Boolean;
    ParamEnded: Boolean;
    I: Integer;
    TypeName: String;
  begin
    Level := 0;
    ParamsInGroup := 0;
    TypeNamePartStarted := False;
    TypeNamePartEnded := False;
    ParamEnded := False;
    TypeName := '';
    repeat
      T := Scanner.GetToken;
      try
        if T.MyType = TOK_WHITESPACE then
        begin
          { add exactly *one space* at the end of M.FullDeclaration }
          if Length(M.FullDeclaration) > 0 then
          begin
            if (M.FullDeclaration[Length(M.FullDeclaration)] <> ' ') then
              M.FullDeclaration := M.FullDeclaration + ' ';
          end;
        end else
        if not (T.MyType in TokenCommentTypes) then
          M.FullDeclaration := M.FullDeclaration + T.Data;

        if Level > 0 then
        begin
          if T.IsSymbol(SYM_COLON) then
            TypeNamePartStarted := True
          else if T.IsSymbol(SYM_EQUAL) then
            // Any identifiers after the equals are NOT parameters
            TypeNamePartEnded := True
          else if (not TypeNamePartEnded) and IsParameterIdentifier(T) then
          begin
            if TypeNamePartStarted then
              TypeName := IfThen(TypeName = '', '', TypeName + ' ') + T.Data
            else
              Inc(ParamsInGroup);
          end
          else if T.IsSymbol(SYM_SEMICOLON) or T.IsSymbol(SYM_LEFT_PARENTHESIS)
            or T.IsSymbol(SYM_RIGHT_PARENTHESIS) then
            ParamEnded := True;
        end;

        if ParamEnded then
        begin
          if TypeName = '' then TypeName := 'const';

           for I := 0 to ParamsInGroup - 1 do
            M.ParamTypes.Add(TypeName);

          TypeName := '';
          TypeNamePartStarted := False;
          TypeNamePartEnded := False;
          ParamEnded := False;
          ParamsInGroup := 0;
        end;

        if T.IsSymbol(SYM_LEFT_PARENTHESIS) then Inc(level);
        if T.IsSymbol(SYM_RIGHT_PARENTHESIS) then Dec(level);
        IsSemicolon := T.IsSymbol(SYM_SEMICOLON);
      finally
        FreeAndNil(T);
      end;
    until IsSemicolon and (Level = 0);

    for I := 0 to ParamsInGroup - 1 do
      M.ParamTypes.Add('const');
  end;

var
  t: TToken;
  InvalidType, WasDeprecatedDirective: boolean;
begin
  t := nil;
  WasDeprecatedDirective := false;
  M := TPasRoutine.Create;
  try
    M.RawDescriptionInfo^ := RawDescriptionInfo;
    M.SetAttributes(CurrentAttributes);

    M.What := RoutineType;

    if IsGeneric <> '' then
      M.FullDeclaration := SAppendPart(M.FullDeclaration, ' ', IsGeneric);
    if ClassKeyWordString <> '' then
      M.FullDeclaration := SAppendPart(M.FullDeclaration, ' ', ClassKeyWordString);
    M.FullDeclaration := SAppendPart(M.FullDeclaration, ' ', RoutineTypeString);

    { next non-wc token must be the name }
    if NeedName then
    begin
      t := GetNextToken;

      if (RoutineType = ROUTINE_OPERATOR) then
      begin
        { In FPC operators "or", "and", "xor" (expressed as keywords) can be
          overloaded, also symbolic operators like "+", "*" etc..
          In Delphi 2006+ "operator" is followed by identifiers like
          "Implicit", "Explicit" or "LogicalNot",  "BitwiseAnd" etc.. }
        InvalidType := (t.MyType <> TOK_IDENTIFIER) and
                       (t.MyType <> TOK_SYMBOL) and (t.MyType <> TOK_KEYWORD);
      end
      else
        InvalidType := (t.MyType <> TOK_IDENTIFIER);

      if InvalidType then
        DoError('Unexpected token %s', [T.Description]);

      { Consume all following period-delimited identifiers as a single name.
        Actual only when reading impl section. Resulting name requires additional
        processing (splitting to class and method names).
        Not used for FPC keyword and symbol operators }
      if (RoutineType = ROUTINE_OPERATOR) and (t.MyType <> TOK_IDENTIFIER) then
        M.Name := t.Data
      else
      begin
        Scanner.UnGetToken(t);
        ReadNestedName;
      end;

      DoMessage(5, pmtInformation, 'Parsing %s "%s"',
        [RoutineTypeToString(RoutineType), M.Name]);
      M.FullDeclaration := M.FullDeclaration + ' ' + M.Name;
      FreeAndNil(t);
    end;

    ReadParameters;

    { This must be after the parameters are read, as TPasRoutine.Signature is generated from the parameters. }
    if InitItemsForNextBackComment then
      ItemsForNextBackComment.ClearAndAdd(M);

    { first get non-WC token - if it is not an identifier in SD_SET put it back
      into stream and leave; otherwise copy tokens until semicolon }
    repeat
      FreeAndNil(t);
      t := GetNextToken;

      if t.MyType = TOK_IDENTIFIER then
      begin
        case t.Info.StandardDirective of
          SD_ABSTRACT, SD_ASSEMBLER, SD_CDECL, SD_DYNAMIC, SD_EXPORT,
          SD_FAR, SD_FORWARD, SD_NEAR, SD_OVERLOAD, SD_OVERRIDE, SD_INLINE,
          SD_PASCAL, SD_REGISTER, SD_SAFECALL, SD_STATIC,
          SD_STDCALL, SD_REINTRODUCE, SD_VIRTUAL,
          SD_VARARGS, SD_FINAL:
            begin
              M.Directives := M.Directives + [t.Info.StandardDirective];
              M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
              FreeAndNil(t);
              t := GetNextToken;
            end;

          { * External declarations might be followed by a string constant.
            * Messages are followed by an integer constant between 1 and 49151 which
              specifies the message ID.
            * Deprecated might be followed by a string constant since D2010. }
          SD_EXTERNAL, SD_MESSAGE, SD_NAME, SD_DEPRECATED:
            begin
              M.Directives := M.Directives + [t.Info.StandardDirective];

              WasDeprecatedDirective := t.Info.StandardDirective = SD_DEPRECATED;
              if WasDeprecatedDirective then
                M.HintDirectives := M.HintDirectives + [hdDeprecated];
              M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
              FreeAndNil(t);

              // Keep on reading up to the next semicolon or declaration
              repeat
                t := GetNextToken;
                if t.IsSymbol(SYM_SEMICOLON) then
                  Break;
                { Some directives mean that the next part starts immediately,
                  without a semicolon between. For example,
                  "deprecated platform" may be placed after a procedure
                  (without a semicolon after "deprecated"). }
                if t.MyType = TOK_IDENTIFIER then
                  case t.Info.StandardDirective of
                    SD_ABSTRACT, SD_ASSEMBLER, SD_CDECL, SD_DYNAMIC, SD_EXPORT,
                    SD_EXTERNAL,
                    SD_FAR, SD_FORWARD, SD_NEAR, SD_OVERLOAD, SD_OVERRIDE,
                    SD_PASCAL, SD_REGISTER, SD_SAFECALL, SD_STATIC,
                    SD_STDCALL, SD_REINTRODUCE, SD_VIRTUAL,
                    SD_DEPRECATED, SD_PLATFORM, SD_EXPERIMENTAL:
                      begin
                        M.Directives := M.Directives + [t.Info.StandardDirective];
                        Scanner.UnGetToken(t);
                        Break;
                      end;
                  end;
                if WasDeprecatedDirective then
                begin
                  if T.MyType = TOK_STRING then
                    M.DeprecatedNote := M.DeprecatedNote + T.StringContent else
                    { end WasDeprecatedDirective on non-string token, summing
                      all previous string tokens into M.DeprecatedNote }
                    WasDeprecatedDirective := false;
                end;
                M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
                FreeAndNil(t);
              until False;
            end;
          SD_PLATFORM:
            begin
              M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
              M.HintDirectives := M.HintDirectives + [hdPlatform];
              FreeAndNil(t);
              t := GetNextToken;
            end;
          SD_EXPERIMENTAL:
            begin
              M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
              M.HintDirectives := M.HintDirectives + [hdExperimental];
              FreeAndNil(t);
              t := GetNextToken;
            end;
          SD_DISPID:
            begin
              M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
              FreeAndNil(T);

              ReadTokensUntilSemicolon;

              t := GetNextToken;
            end;
        else //case
            Scanner.UnGetToken(t);
            Break;
        end; // case
      end else
      if t.MyType = TOK_KEYWORD then
      begin
        case t.Info.KeyWord of
          KEY_INLINE: begin
              M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
              FreeAndNil(t);
              t := GetNextToken;
            end;
          KEY_LIBRARY:
            begin
              M.FullDeclaration := M.FullDeclaration + ' ' + t.Data;
              M.HintDirectives := M.HintDirectives + [hdLibrary];
              FreeAndNil(t);
              t := GetNextToken;
            end;
          else
            begin
              Scanner.UnGetToken(t);
              Break;
            end;
        end;
      end else
      begin
        Scanner.UnGetToken(t);
        Break;
      end;

      { Directives don't have to be separated and be terminated by a semicolon.
        This is known at least for combination "deprecated platform".
        Apparently, some Delphi versions also allowed not using semicolon
        at other places (and we have to mimic compiler behavior, not only
        documented behavior, since in practice people (over)use everything
        that compiler allows).

        So, check is current token a semicolon and append to FullDeclaration.
        Note that T may be nil now (e.g. because we used UnGetToken last). }
      if (t <> nil) and t.IsSymbol(SYM_SEMICOLON) then
        M.FullDeclaration := M.FullDeclaration + ';'
      else begin
        M.FullDeclaration := M.FullDeclaration + ' ';
        if t <> nil then Scanner.UnGetToken(t);
      end;
    until False;
  except
    M.Free;
    t.Free;
    raise;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseCIO(const U: TPasUnit;
  const CioName, CioNameWithGeneric: string; CIOType: TCIOType;
  const RawDescriptionInfo: TRawDescriptionInfo;
  const IsInRecordCase: boolean);
begin
  try
    ParseCioEx(U, CioName, CioNameWithGeneric, CIOType, RawDescriptionInfo,
      IsInRecordCase);
  finally
    FCioSk.Clear;
  end;
end;

{ ---------------------------------------------------------------------------- }
procedure TParser.ParseConstant(OwnerItemType: TOwnerItemType; out Constant: TPasItem);
var
  t: TToken;
  WhitespaceCollector: string;

  { Same as SkipDeclaration() except that it returns TRUE if we hit a  }
  { procedural constant, it might be followed by a calling convention  }
  function LocalSkipDeclaration: Boolean;
  var
    EndLevel, PLevel: Integer;
    IsSemicolon: Boolean;
    WhitespaceCollectorToAdd: string;
  begin
    EndLevel := 0;
    PLevel := 0;
    Result := False;
    {AG}
    repeat
      t := GetNextToken(Constant);
      WhitespaceCollectorToAdd := '';
      try
        case t.MyType of
          TOK_SYMBOL:
            case t.Info.SymbolType of
              SYM_LEFT_PARENTHESIS: Inc(PLevel);
              SYM_RIGHT_PARENTHESIS: Dec(PLevel);
            end;
          TOK_KEYWORD:
            case t.Info.KeyWord of
              KEY_END: Dec(EndLevel);
              KEY_RECORD: Inc(EndLevel);
              KEY_LIBRARY:
                Constant.HintDirectives := Constant.HintDirectives + [hdLibrary];
              KEY_FUNCTION,
              KEY_PROCEDURE:
                Result := True;
          end;
        TOK_IDENTIFIER:
          case t.Info.StandardDirective of
            SD_PLATFORM:
              Constant.HintDirectives := Constant.HintDirectives + [hdPlatform];
            SD_EXPERIMENTAL:
              Constant.HintDirectives := Constant.HintDirectives + [hdExperimental];
            SD_DEPRECATED:
              begin
                Constant.HintDirectives := Constant.HintDirectives + [hdDeprecated];
                while PeekNextToken(WhitespaceCollectorToAdd).MyType = TOK_STRING do
                begin
                  { consume the following string as DeprecatedNote }
                  Constant.FullDeclaration := Constant.FullDeclaration + t.Data + WhitespaceCollectorToAdd;
                  FreeAndNil(t);
                  t := GetNextToken(Constant);
                  Assert(T.MyType = TOK_STRING); // T is now the same thing we saw with PeekNextToken
                  Constant.DeprecatedNote := Constant.DeprecatedNote + t.StringContent;
                end;
                { otherwise WhitespaceCollectorToAdd will be added later }
              end;
          end;
        end;
        IsSemicolon := t.IsSymbol(SYM_SEMICOLON);

        { Reason for "EndLevel < 0" condition:
            Within records et al. the last declaration need not be terminated by ;
          Reason for "(PLevel < 0) and IsInRecordCase" condition:
            See autodoc of SkipDeclaration in TParser interface. }
        if (EndLevel < 0) {or
          ( (PLevel < 0) and IsInRecordCase )} then
        begin
          Scanner.UnGetToken(t);
          Exit;
        end;
        Constant.FullDeclaration := Constant.FullDeclaration + t.Data + WhitespaceCollectorToAdd;
        t.Free;
      except
        t.Free;
        raise;
      end;
    until IsSemicolon and (EndLevel = 0) and (PLevel = 0);
  end;
  {/AG}

begin
  { When in CIO treat this constant as a constant field }
  case OwnerItemType of
    otUnit: Constant := TPasConstant.Create;
    otCio:  Constant := TPasFieldVariable.Create;
  end;

  try
    Constant.Name := GetAndCheckNextToken(TOK_IDENTIFIER);
    DoMessage(5, pmtInformation, 'Parsing constant %s', [Constant.Name]);
    Constant.RawDescriptionInfo^ := GetLastComment;
    Constant.FullDeclaration := Constant.Name;
    Constant.SetAttributes(CurrentAttributes);
    {AG}
    if LocalSkipDeclaration then
    begin
      { Check for following calling conventions }
      t := GetNextToken(WhitespaceCollector);
      try
        case t.Info.StandardDirective of
          SD_CDECL, SD_STDCALL, SD_PASCAL, SD_REGISTER, SD_SAFECALL:
            begin
              Constant.FullDeclaration := Constant.FullDeclaration + WhitespaceCollector;
              Constant.FullDeclaration := Constant.FullDeclaration + t.Data;
              FreeAndNil(t);
              SkipDeclaration(Constant, false);
            end;
        else
          Scanner.UnGetToken(t);
        end;
      except
        t.Free;
        raise;
      end;
    end;
    //SkipDeclaration(i, false);
    {/AG}

    ItemsForNextBackComment.ClearAndAdd(Constant);
    if OwnerItemType = otCio then
    begin
      Constant.FullDeclaration := 'const ' + Constant.FullDeclaration;
      TPasFieldVariable(Constant).IsConstant := True;
    end;
  except
    FreeAndNil(Constant);
    raise;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseEnum(out p: TPasEnum;
  const Name: string; const RawDescriptionInfo: TRawDescriptionInfo);
var
  T: TToken;
  Item: TPasItem;
  ParenLevel: Integer;
begin
  t := nil;
  p := TPasEnum.Create;
  try
    p.Name := Name;
    p.RawDescriptionInfo^ := RawDescriptionInfo;
    p.FullDeclaration := Name + ' = (...)';
    p.SetAttributes(CurrentAttributes);
    ItemsForNextBackComment.ClearAndAdd(P);

    T := GetNextToken;
    while not T.IsSymbol(SYM_RIGHT_PARENTHESIS) do
    begin
      CheckToken(T, TOK_IDENTIFIER);
      Item := TPasItem.Create;
      Item.Name := T.Data;
      Item.RawDescriptionInfo^ := GetLastComment;
      Item.FullDeclaration := Item.Name;
      p.Members.Add(Item);
      ItemsForNextBackComment.ClearAndAdd(Item);
      FreeAndNil(T);

      T := GetNextToken;
      if T.IsSymbol(SYM_EQUAL) or T.IsSymbol(SYM_ASSIGN) then
      begin
        Item.FullDeclaration := Item.FullDeclaration + ' ' + T.Data + ' ';
        FreeAndNil(T);

        { Now read tokens until comma or right paren (but only on ParenLevel = 0). }
        ParenLevel := 0;
        repeat
          T := GetNextToken;

          if (ParenLevel = 0) and
             (T.IsSymbol(SYM_COMMA) or T.IsSymbol(SYM_RIGHT_PARENTHESIS)) then
            Break;

          if T.MyType = TOK_SYMBOL then
            case T.Info.SymbolType of
              SYM_LEFT_PARENTHESIS, SYM_LEFT_BRACKET: Inc(ParenLevel);
              SYM_RIGHT_PARENTHESIS, SYM_RIGHT_BRACKET: Dec(ParenLevel);
            end;

          Item.FullDeclaration := Item.FullDeclaration + T.Data;
          FreeAndNil(T);
        until false;
      end;

      if T.IsSymbol(SYM_COMMA) then
      begin
        FreeAndNil(T);
        T := GetNextToken;
      end;
    end;
    FreeAndNil(T);

    { Read semicolon, as an optional token.
      Actually, the stricter rule would be
      - if some hint directive ("deprecated" or such) follows,
        then semicolon here is prohibited.
      - otherwise, semicolon here is required. }
    T := PeekNextToken;
    if T.IsSymbol(SYM_SEMICOLON) then
    begin
      P.FullDeclaration := P.FullDeclaration + ';';
      Scanner.ConsumeToken;
      FreeAndNil(T);
    end;

    ParseHintDirectives(P, true, true);
  except
    p.Free;
    T.Free;
    raise;
  end;
end;

procedure TParser.ParseStrongAlias(out P: TPasAliasType; const Name: string;
  const RawDescriptionInfo: TRawDescriptionInfo);
var
  AliasName, LTemp: string;
  T: TToken;
  i: Integer;
begin
  P := TPasAliasType.Create;
  T := nil;
  try
    P.Name := Name;
    P.RawDescriptionInfo^ := RawDescriptionInfo;
    P.SetAttributes(CurrentAttributes);

    T := GetNextToken(LTemp);
    if T.MyType <> TOK_IDENTIFIER then
      DoError('Unexpected %s', [T.Description]);
    AliasName := T.Data;
    FreeAndNil(T);
    For i := 1 to 2 do
    begin
      T := PeekNextToken;
      If t.IsSymbol(SYM_PERIOD) then
      begin
        Scanner.ConsumeToken;
        FreeAndNil(T);

        T := GetNextToken(LTemp);
        if T.MyType <> TOK_IDENTIFIER then
          DoError('Unexpected %s', [T.Description]);
        AliasName := AliasName + '.' + T.Data;
        FreeAndNil(T);
      end;
    end;
    P.AliasedName:= AliasName;
    P.IsStrongAlias:= true;
    P.FullDeclaration := Name + ' = type ' + AliasName;

    T := PeekNextToken;
    if T.IsSymbol(SYM_SEMICOLON) then
    begin
      P.FullDeclaration := P.FullDeclaration + ';';
      Scanner.ConsumeToken;
      FreeAndNil(T);
    end;
    ParseHintDirectives(P, true, true);
  except
    P.Free;
    T.Free;
    raise;
  end;
end;

procedure TParser.ParseTVCSection(U: TPasUnit);
var
  Mode: TItemParseMode;
  t, t2: TToken;
  ConstantParsed: TPasItem;
begin
  Mode := pmUndefined;

  repeat
    t := GetNextToken;
    try
      case t.MyType of
        TOK_IDENTIFIER:
          begin
            if t.Info.StandardDirective = SD_OPERATOR then
            begin
              Scanner.UnGetToken(t);
              Break;
            end;

            case Mode of
              pmConst:
                begin
                  Scanner.UnGetToken(t);
                  ParseConstant(otUnit, ConstantParsed);
                  if U <> nil then
                    U.AddConstant(ConstantParsed)
                  else
                    FreeAndNil(ConstantParsed);
                end;
              pmType:
                begin
                  //In latest FPC version it is posible to define generic functions and procedures
                  if t.IsStandardDirective(SD_GENERIC) then
                  begin
                    t2 := PeekNextToken;
                    if t2.IsKeyWord(KEY_FUNCTION) or t2.IsKeyWord(KEY_PROCEDURE) then
                      Break
                    else
                      ParseType(U, t.Data)
                  end
                  else
                  begin
                    Scanner.UnGetToken(t);
                    ParseType(U, '')
                  end;
                  AttributeIsPossible := True;
                end;
              pmVar:
                begin
                  Scanner.UnGetToken(t);
                  ParseVariables(U);
                end;
              else
                DoError('Unexpected %s', [t.Description]);
            end;
          end;
        TOK_KEYWORD:
          begin
            // Back comments after a keyword are senseless
            ItemsForNextBackComment.Clear;
            case t.Info.KeyWord of
              KEY_RESOURCESTRING, KEY_CONST:
                Mode := pmConst;
              KEY_THREADVAR, KEY_VAR:
                Mode := pmVar;
              KEY_TYPE:
                begin
                  Mode := pmType;
                  AttributeIsPossible := True;
                end;
              else
                begin
                  Scanner.UnGetToken(t);
                  Break;
                end;
            end;
          end;
      end;
    finally
      FreeAndNil(t);
    end;
  until False;
end;

procedure TParser.ParseInterfaceSection(const U: TPasUnit);
var
  M: TPasRoutine;
  t, t2: TToken;
  PropertyParsed: TPasProperty;
begin
  DoMessage(4, pmtInformation, 'Entering interface section of unit %s',[U.Name]);

  AttributeIsPossible := False;

  repeat
    t := GetNextToken;

    try
      case t.MyType of
        TOK_IDENTIFIER:
          if t.Info.StandardDirective = SD_OPERATOR then
          begin
            ParseRoutine(M, '', t.Data, ROUTINE_OPERATOR,
              GetLastComment, true, true, '');
            U.FuncsProcs.Add(M);
          end
          //Generics functions and procedures
          else if t.IsStandardDirective(SD_GENERIC) then
          begin
            //Skip generics directive because delphi do not have and we can't return two tokens
            t2 := PeekNextToken();
            if t2.IsKeyWord(KEY_FUNCTION) or t2.IsKeyWord(KEY_PROCEDURE) then
            begin
              t := GetNextToken;
              try
                ParseRoutine(M, '', t.Data, KeyWordToRoutineType(t.Info.KeyWord),
                  GetLastComment, true, true, '');
                U.FuncsProcs.Add(M)
              finally
                FreeAndNil(t)
              end;
            end
            else
              DoError('Unexpected %s', [t2.Description]);
          end
          else
            DoError('Unexpected %s', [t.Description]);
        TOK_KEYWORD:
          begin
            case t.Info.KeyWord of
              KEY_RESOURCESTRING, KEY_CONST,
              KEY_TYPE,
              KEY_THREADVAR, KEY_VAR:
                begin
                  Scanner.UnGetToken(t);
                  ParseTVCSection(U);
                end;
              KEY_FUNCTION, KEY_PROCEDURE:
                begin
                  ParseRoutine(M, '', t.Data, KeyWordToRoutineType(t.Info.KeyWord),
                    GetLastComment, true, true, '');
                  U.FuncsProcs.Add(M);
                end;
              KEY_IMPLEMENTATION:
                begin
                  Scanner.UnGetToken(t);
                  Break;
                end;
              KEY_USES:
                ParseUses(U);
              KEY_PROPERTY:
                begin
                  ParseProperty(PropertyParsed);
                  U.Variables.Add(PropertyParsed);
                end;
            else
              DoError('Unexpected %s', [t.Description]);
            end;
          end;
      end;
    finally
      FreeAndNil(t);
    end;
  until False;
end;

{ Return string value calculated from old and new values according to merge method }
function MergeStringValues(const MergeType: TInfoMergeType;
  const OldValue, NewValue: string): string;
begin
  case MergeType of
    imtNone:
      Result := OldValue;
    imtPreferIntf:
      Result := IfThen(OldValue <> '', OldValue, NewValue);
    imtPreferImpl:
      Result := IfThen(NewValue <> '', NewValue, OldValue);
    // Also process case when OldValue is fully contained in NewValue.
    // This allows specifying short abstract in intf section and full description
    // in impl section.
    // At this stage values are raw, that is, could contain multiple whitespaces.
    // We must exclude whitespaces from check.
    imtJoin:
      // Note: this could be optimized to not use 2nd Trim or even check in-place without
      // creation of new string variables.
      if (Trim(OldValue) <> '') and not AnsiStartsStr(Trim(OldValue), Trim(NewValue)) then
        Result := OldValue + LineEnding + NewValue
      else
        Result := NewValue;
    else raise EInternalParserError.Create('TInfoMergeType unimplemented');
  end;
end;

{ Merge metadata of Source and Dest routines.
  At the stage of parsing impl section these items only have RawDescriptionInfo
  so that's the only data we've to merge.

  NB: this merge is only correct if routines haven't been processed yet so
  there's no sense in moving it to common util unit or TPasRoutine members. }
procedure MergeRoutineData(const MergeType: TInfoMergeType;
  const Dest: TPasRoutine; const SourceData: TRawDescriptionInfo);
begin
  Dest.RawDescriptionInfo^.Content :=
    MergeStringValues(MergeType, Dest.RawDescriptionInfo^.Content, SourceData.Content);
end;

procedure TParser.ParseImplementationSection(const U: TPasUnit);

var
  // Collector of all ignored items that won't go to `U`
  DummyUnit: TPasUnit;

  // Clear all comment data that was accumulated by PeekNextToken
  procedure ClearComments;
  begin
    IsLastComment := False;
    LastCommentWasCStyle := False;
    LastCommentHelpInsight := False;
    LastCommentInfo := EmptyRawDescriptionInfo;
  end;

  { Function to skip header (parameters declarations) of anon method aka lambda.
    Just read until "begin" }
  procedure SkipLambdaHeader;
  var t: TToken;
  begin
    t := nil;
    repeat
      t := PeekNextToken;
      if t.IsKeyWord(KEY_BEGIN) then
        Break;

      Scanner.ConsumeToken;
      FreeAndNil(t);
    until False;
  end;

  { Here we stand:
      - at the beginning of a routine's (function/procedure/constructor/destructor)
        valuable (non-comment) inner contents (after ParseRoutine invokation)
      or
      - right after function/procedure keyword (lambda assignment/declaration; parameters
        or comment or contents following)
    Skip everything until "end;" }
  procedure SkipMethodBody(IsLambda: Boolean = False);
  var
    t: TToken;
    EndLevel: Integer;
    InsideMethodBody, AsmBlock: Boolean;
    M: TPasRoutine;
  begin
    EndLevel := 0; InsideMethodBody := False; AsmBlock := False; t := nil;
    repeat
      if t = nil then
        t := GetNextToken;

      // Check only keywords; skip all keywords inside ASM blocks except "end"
      if (t.MyType = TOK_KEYWORD) and (not AsmBlock or (t.Info.KeyWord = KEY_END)) then
        case t.Info.KeyWord of
          // nested const/type section
          // KEY_RESOURCESTRING and KEY_THREADVAR are not allowed here but let them remain
          KEY_RESOURCESTRING, KEY_CONST,
          KEY_TYPE,
          KEY_THREADVAR:
            begin
              Scanner.UnGetToken(t);
              ParseTVCSection(DummyUnit); // ignore section contents
            end;
          // nested var section or inline var
          KEY_VAR:
            if not InsideMethodBody then
            begin
              // special case to handle typeless inline var declaration:
              // "begin ... var foo := 1; ... end;" - just skip if the clause is inside a method body
              Scanner.UnGetToken(t);
              ParseTVCSection(DummyUnit); // ignore section contents
            end;
          // If we encounter BEGIN or ASM - check that current nesting level is 0,
          // this means we've started the entrypoint of this method
          KEY_BEGIN, KEY_ASM:
            begin
              if EndLevel = 0 then
                InsideMethodBody := True;
              // Asm blocks have different syntax that allows any Pascal keyword, even "end"
              // so handle them specially
              AsmBlock := (t.Info.KeyWord = KEY_ASM);
              Inc(EndLevel);
            end;
          // Other constructions in the code section that must end with END keyword
          KEY_CASE, KEY_TRY:
            Inc(EndLevel);
          // Nested subroutine / lambda. Run the skip recursively.
          KEY_PROCEDURE, KEY_FUNCTION:
            begin
              // if InsideMethodBody - it's a lambda without name
              // otherwise it's a nested subroutine that requires name
              if not InsideMethodBody then
              begin
                ParseRoutine(M, '', '', KeyWordToRoutineType(t.Info.KeyWord), GetLastComment, True, False, '');
                FreeAndNil(M);
              end
              else
                SkipLambdaHeader;
              // There could not be external methods inside a method so skip body unconditionally
              SkipMethodBody(InsideMethodBody);
            end;
          KEY_END:
            begin
              // ASM blocks can contain labels or identifiers named "end" so check
              // whether the "end" is followed by ";". Skip if not
              if AsmBlock then
              begin
                if not PeekNextToken.IsSymbol(SYM_SEMICOLON) then
                begin
                  FreeAndNil(t);
                  t := GetNextToken;
                  Continue;
                end;
              end;
              AsmBlock := False;
              Dec(EndLevel);
              if InsideMethodBody and (EndLevel = 0) then
              begin
                // for subroutines ";" after "end" is obligatory
                if not IsLambda then
                  GetAndCheckNextToken(SYM_SEMICOLON)
                // lambdas could end with
                //   ";" (var assignment),
                //   ")" (as parameter in method),
                //   "," (as one of parameters in method, item of array etc),
                //   "]" (last item in array)
                // ...
                else
                  try
                    FreeAndNil(t);
                    t := GetNextToken;
                    if not ((t.MyType = TOK_SYMBOL) and
                      (t.Info.SymbolType in [SYM_SEMICOLON, SYM_COMMA,
                        SYM_RIGHT_PARENTHESIS, SYM_RIGHT_BRACKET])) then
                      DoError(SExpectedButFound,
                        ['one of ";", ")", ",", "]" symbols', T.Description]);
                  finally
                    FreeAndNil(t);
                  end;
                Break;
              end;
            end;
        end;
      FreeAndNil(t);
    until False;
    FreeAndNil(t);
    // Empty all comments accumulated inside method body by PeekNextToken
    // Otherwise they will go to a next item
    ClearComments;
  end;

  { Search for method object added by parsing intf section.
    Method could be standalone routine, FPC operator or CIO member.
    In the latter case it will have fully specified name (TClass.TNestedClass.Proc).
    Corresponding method likely could not exist in lists from intf section because
    of ignoring or visibility settings.
      @param U - unit to search in
      @param MethodName - full method name to search for
      @param Index - 0-based index of a method overload to search for. Currently
        method lists could contain multiple entries with identical names because
        of overload feature. Specifying Index allows to search for a concrete item.
        Of course this will work correctly only if methods were declared in the
        same order as they appear in impl section }
  function FindExistingRoutine(U: TPasUnit; const MethodName: string; Index: Integer): TPasRoutine;
  var
    NameParts: TNameParts;
    i: Integer;
    item: TBaseItem;
  begin
    Result := nil;
    // Check if we got a standalone routine or a class/record method
    // NB: method could be FPC operator with symbolic name so just check for dot inside
    if Pos('.', MethodName) = 0 then
    begin
      item := U.FuncsProcs.FindListItem(MethodName, Index);
      if item <> nil then
        Result := item as TPasRoutine;
    end
    else
    begin
      if not SplitNameParts(MethodName, NameParts) then
        DoError('Method name %s is invalid', [MethodName]);
      // Search for method owner
      item := U.CIOs.FindListItem(NameParts[0]);
      // Also in nested classes
      if item <> nil then
        for i := Low(NameParts) + 1 to High(NameParts) - 1 do
        begin
          item := (item as TPasCio).CIOs.FindListItem(NameParts[i]);
          if item = nil then
            Break;
        end;
      if item <> nil then
        Result := (item as TPasCio).Methods.FindListItem(NameParts[High(NameParts)], Index);
    end;
    // print message for debug purposes
    if Result = nil then
      DoMessage(5, pmtInformation, 'No definition of %d-th method "%s" found - probably internal or ignored', [Index, MethodName])
  end;

  { Read routine header and merge its description with existing item. }
  procedure HandleRoutine(U: TPasUnit; RoutineType: TRoutineType; IsGeneric: String; MethodCounts: THash = nil;
    const ClassKeyWordString: string = '');
  var
    M, ExistingRoutine: TPasRoutine;
    Count: NativeUInt;
  begin
    ParseRoutine(M, ClassKeyWordString, RoutineTypeToString(RoutineType), RoutineType,
      GetLastComment, True, True, IsGeneric);
    // ParseRoutine was called with InitItemsForNextBackComment so it added M to
    // ItemsForNextBackComment list. We must clear it so that following comments
    // in AutoBackComments mode won't glue to method object which is already disposed
    ItemsForNextBackComment.Clear;

    // If a method counter is given, search for method inside it. Otherwise
    // assume Count is 1.
    if MethodCounts <> nil then
    begin
      Count := NativeUInt(MethodCounts.GetObject(M.Name));
      Inc(Count);
      MethodCounts.SetObject(M.Name, Pointer(Count));
    end
    else
      Count := 1;

    ExistingRoutine := FindExistingRoutine(U, M.Name, Count - 1);
    // NB: Currently we don't add methods not declared in intf section
    if ExistingRoutine <> nil then
      MergeRoutineData(FInfoMergeType, ExistingRoutine, M.RawDescriptionInfo^);
    // External and forward methods have no body
    if [SD_FORWARD, SD_EXTERNAL]*M.Directives = [] then
    begin
      SkipMethodBody;
      DoMessage(5, pmtInformation, 'Skipped body of %s "%s"',
        [RoutineTypeToString(RoutineType), M.Name]);
    end;
    FreeAndNil(M);
  end;

var
  t, t2: TToken;
  PropertyParsed: TPasProperty;
  MethodCounts: THash;
begin
  { Parsing impl section clears the comment otherwise comment before "implementation"
    keyword would descend to first item of impl section }
  ClearComments;

  if FInfoMergeType = imtNone then Exit;

  DoMessage(4, pmtInformation, 'Entering implementation section of unit %s',[U.Name]);

  AttributeIsPossible := False;
  MethodCounts := THash.Create; // "[method name]=>count" hash map
  { We can't immediately dispose parsed items because we've got to handle back
    comments. So we create dummy container that will gather all ignored items. }
  DummyUnit := TPasUnit.Create;

  try
    repeat
      t := GetNextToken;
      try
        case t.MyType of
          TOK_IDENTIFIER:
            if t.Info.StandardDirective = SD_OPERATOR then
            begin
              HandleRoutine(U, ROUTINE_OPERATOR, '');
            end
            else if t.IsStandardDirective(SD_GENERIC) then
            begin
              t2 := PeekNextToken;
              if t2.Info.KeyWord in [KEY_FUNCTION, KEY_PROCEDURE] then
              begin
                 t2 := GetNextToken;
                 try
                   HandleRoutine(U, KeyWordToRoutineType(t2.Info.KeyWord), t.Data, MethodCounts)
                 finally
                   FreeAndNil(t2)
                 end
              end
              else
                 DoError('Unexpected %s', [t2.Description])
            end
            else
              DoError('Unexpected %s', [t.Description]);

          TOK_KEYWORD:
            begin
              case t.Info.KeyWord of
                KEY_RESOURCESTRING, KEY_CONST,
                KEY_TYPE,
                KEY_THREADVAR, KEY_VAR:
                  begin
                    Scanner.UnGetToken(t);
                    ParseTVCSection(DummyUnit); // parse section but don't add to resulting unit
                  end;
                KEY_CLASS:
                  //ClassKeyWordString := t.Data; // not needed after all
                  ;
                KEY_FUNCTION, KEY_PROCEDURE, KEY_CONSTRUCTOR, KEY_DESTRUCTOR:
                  begin
                    HandleRoutine(U, KeyWordToRoutineType(t.Info.KeyWord), '', MethodCounts);
                  end;
                // Do not read unit used internally for now - maybe will do in the future
                KEY_USES:
                  ParseUses(nil);
                KEY_PROPERTY:
                  begin
                    ParseProperty(PropertyParsed);
                    FreeAndNil(PropertyParsed);
                  end;
                { Stop parsing on "initialization", "finalization".
                  The "begin" at this point is equivalent to "initialization". }
                KEY_BEGIN, KEY_INITIALIZATION, KEY_FINALIZATION:
                  Break;
                // Stop parsing on "end.". Must come here only on final "end" so
                // don't care about other cases
                KEY_END:
                  begin
                    // skip possible whitespaces
                    repeat
                      FreeAndNil(t);
                      t := GetNextToken;
                    until not (t.MyType = TOK_WHITESPACE);
                    // token must be period
                    if t.IsSymbol(SYM_PERIOD) then
                      Break;
                    DoError('Unexpected %s', [t.Description]);
                  end;
              else
                DoError('Unexpected %s', [t.Description]);
              end;
            end;
        end;
      finally
        FreeAndNil(t);
      end;
    until False;
  finally
    FreeAndNil(MethodCounts);
    FreeAndNil(DummyUnit);
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseProperty(out p: TPasProperty);

  { Parse property information
      read XXX
      write XXX
      default XXX
      nodefault
      stored XXX

    Stops on semicolon.

    Also all whitespace and tokens are added to P.FullDeclaration.
  }
  procedure ParsePropertyEnding(const P: TPasProperty);
  var
    HasCurrentDirective: Boolean;
    CurrentDirective: TStandardDirective;
    CurrentDirectiveCollected: String;

    procedure CurrentDirectiveFinished;
    begin
      if HasCurrentDirective then
      begin
        case CurrentDirective of
          SD_READ: P.Reader := CurrentDirectiveCollected;
          SD_WRITE: P.Writer := CurrentDirectiveCollected;
          SD_DEFAULT: P.DefaultValue := CurrentDirectiveCollected;
          SD_STORED: P.Stored := CurrentDirectiveCollected;
          else raise EInternalParserError.Create('CurrentDirectiveFinished: unexpected CurrentDirective');
        end;
        HasCurrentDirective := false;
      end;
    end;

  var
    T: TToken;
    PreviousWasDot: Boolean;
  begin
    { HasCurrentDirective, CurrentDirective track are we now collecting
      tokens that follow a directive like SD_READ, SD_WRITE.
      We are prepared that each directive may be followed by a series of tokens,
      which allows to parse e.g. "default TXxx.Yyy" or "default 10 + 20". }
    HasCurrentDirective := false;

    { We track if previous token is dot, to not treat 2nd "default" as start
      of default section in code like this: "property Xxx default TSomething.Default". }
    PreviousWasDot := false;

    repeat
      T := GetNextToken(P);

      { regardless of the whole logic of capturing current directive (like read, write),
        we have to add all whitespace and tokens to P.FullDeclaration. }
      P.FullDeclaration := P.FullDeclaration + T.Data;

      if T.IsStandardDirective(SD_NODEFAULT) then
      begin
        T.Free;
        CurrentDirectiveFinished;
        PreviousWasDot := false;
        P.NoDefault := true;
      end else
      if (T.MyType = TOK_IDENTIFIER) and
         (not PreviousWasDot) and
         (T.Info.StandardDirective in [SD_READ, SD_WRITE, SD_DEFAULT, SD_STORED]) then
      begin
        CurrentDirectiveFinished;
        PreviousWasDot := false;
        { initialize tracking new CurrentDirective }
        HasCurrentDirective := true;
        CurrentDirective := T.Info.StandardDirective;
        CurrentDirectiveCollected := '';
        T.Free;
      end else
      if T.IsSymbol(SYM_SEMICOLON) then
      begin
        T.Free;
        CurrentDirectiveFinished;
        // PreviousWasDot := false; // PreviousWasDot value doesn't matter further in this case
        Break;
      end else
      begin
        if HasCurrentDirective then
          CurrentDirectiveCollected := SAppendPart(CurrentDirectiveCollected, ' ', T.Data);
        PreviousWasDot := T.IsSymbol(SYM_PERIOD);
        T.Free;
      end;
    until false; // always exits with Break
  end;

var
  Finished: Boolean;
  t: TToken;
begin
  t := nil;
  p := TPasProperty.Create;
  try
    p.Name := GetAndCheckNextToken(TOK_IDENTIFIER);
    DoMessage(5, pmtInformation, 'Parsing property %s', [p.Name]);
    p.IndexDecl := '';
    p.Proptype := '';
    p.FullDeclaration := 'property ' + p.Name;
    p.RawDescriptionInfo^ := GetLastComment;
    p.SetAttributes(CurrentAttributes);
    ItemsForNextBackComment.ClearAndAdd(P);

    { Is this only a redeclaration of property from ancestor
      (to e.g. change it's visibility, or add a hint directive) }
    t := GetNextToken(P);
    if t.IsSymbol(SYM_SEMICOLON) then
    begin
      p.FullDeclaration := p.FullDeclaration + ';';
      FreeAndNil(t);
      ParseHintDirectives(P, true, true);
      Exit;
    end;

    { get index }
    if t.IsSymbol(SYM_LEFT_BRACKET) then
    begin
      FreeAndNil(t);
      p.IndexDecl := '[';
      p.FullDeclaration := p.FullDeclaration + '[';
      repeat
        t := GetNextToken;

        if not (t.MyType in TokenCommentTypes + [TOK_DIRECTIVE]) then
        begin
          p.IndexDecl := p.IndexDecl + t.Data;
          p.FullDeclaration := AppendDeclaration(p.FullDeclaration, t.Data);
        end;
        Finished := t.IsSymbol(SYM_RIGHT_BRACKET);
        FreeAndNil(t);
      until Finished;

      t := GetNextToken;
    end;

    { now if there is a colon, it is followed by the type }
    if t.IsSymbol(SYM_COLON) then
    begin
      FreeAndNil(t);

      { get property type }
      t := GetNextToken;
      if (t.MyType <> TOK_IDENTIFIER) and (t.MyType <> TOK_KEYWORD) then
        DoError('Identifier or keyword expected but %s found', [T.Description]);

      p.Proptype := t.Data;
      FreeAndNil(t);
      p.FullDeclaration := p.FullDeclaration + ': ' + p.Proptype;
    end else
    begin
      p.FullDeclaration := p.FullDeclaration + t.Data;
      FreeAndNil(t);
    end;

    ParsePropertyEnding(P);

    ParseHintDirectives(P, true, true);
  except
    p.Free;
    t.Free;
    raise
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseRecordCase(const R: TPasCio;
  const SubCase: boolean);
var
  t: TToken;
  LNeedId: boolean;
  P: TPasFieldVariable;
  OldAttribPossible: Boolean;
begin
  t := GetNextToken;
  try
    CheckToken(t, TOK_IDENTIFIER);

    if PeekNextToken.IsSymbol(SYM_COLON) then
    begin
      { Then we have "case FieldName: FieldType of" }

      { consume and free the colon token }
      GetNextToken.Free;

      P := TPasFieldVariable.Create;
      p.Name := t.Data;
      p.RawDescriptionInfo^ := GetLastComment;
      p.FullDeclaration := p.Name + ': ' + GetAndCheckNextToken(TOK_IDENTIFIER);
      p.SetAttributes(CurrentAttributes);
      ItemsForNextBackComment.ClearAndAdd(P);
      R.Fields.Add(p);
    end;

    FreeAndNil(t);

    GetAndCheckNextToken(KEY_OF);

    CurrentAttributes.Clear;
    OldAttribPossible := AttributeIsPossible;
    AttributeIsPossible := False;

    { no support for attributes in case record, if found, unexpected behaviour }
    t := GetNextToken;
    LNeedId := True;
    repeat
      while true do
      begin
        case t.MyType of
          TOK_SYMBOL:
            begin
              case t.Info.SymbolType of
                SYM_COLON: break;
                SYM_COMMA: LNeedId := True;
              end;
            end;
          TOK_IDENTIFIER,
          TOK_NUMBER:
            if not LNeedId then
              DoError('Unexpected %s', [t.Description]);

          TOK_KEYWORD:
            begin
              if not (t.Info.KeyWord in [KEY_OR, KEY_AND]) then
                DoError('Unexpected %s', [t.Description]);
            end;
          else // case
            DoError('Unexpected %s', [t.Description]);
        end; // case
        FreeAndNil(t);
        t := GetNextToken;
      end;
      // read all identifiers before colon

      FreeAndNil(t);

      GetAndCheckNextToken(SYM_LEFT_PARENTHESIS);

      while not PeekNextToken.IsSymbol(SYM_RIGHT_PARENTHESIS) do
      begin
        if PeekNextToken.IsKeyWord(KEY_CASE) then
        begin
          GetNextToken.Free; { consume and free "case" token }
          ParseRecordCase(R, true);
        end else
          ParseFieldsVariables(R.Fields, true, viPublic, true);
      end;

      GetNextToken.Free; // free ')' token

      t := GetNextToken;
      if t.IsSymbol(SYM_SEMICOLON) then
      begin
        FreeAndNil(t);
        t := GetNextToken;
      end;

    until t.IsKeyWord(KEY_END) or
      (SubCase and t.IsSymbol(SYM_RIGHT_PARENTHESIS));

    AttributeIsPossible := OldAttribPossible;

    Scanner.UnGetToken(t);
  except
    t.Free;
    raise;
  end;
end;

procedure TParser.ParseType(const U: TPasUnit; IsGeneric: String);

  function KeyWordToCioType(KeyWord: TKeyword; IsPacked: Boolean): TCIOType;
  begin
    if not IsPacked then
      case KeyWord of
        KEY_CLASS:         Result := CIO_CLASS;
        KEY_DISPINTERFACE: Result := CIO_DISPINTERFACE;
        KEY_INTERFACE:     Result := CIO_INTERFACE;
        KEY_OBJECT:        Result := CIO_OBJECT;
        KEY_RECORD:        Result := CIO_RECORD;
        else               raise EInternalParserError.Create('KeyWordToCioType: invalid keyword');
      end
    else
      case KeyWord of
        KEY_CLASS:         Result := CIO_PACKEDCLASS;
        KEY_OBJECT:        Result := CIO_PACKEDOBJECT;
        KEY_RECORD:        Result := CIO_PACKEDRECORD;
        else               raise EInternalParserError.Create('KeyWordToCioType: invalid keyword');
      end;
  end;

  procedure TryPromoteToWeakAlias(var P: TPasType);
  var
    MaybeAliasName: String;
    WeakAliasType: TPasAliasType;
  begin
    MaybeAliasName := P.FullDeclaration
      .SubString(pos('=', P.FullDeclaration)).TrimLeft
      .TrimRight([';']).TrimRight;

    if IsValidMultipartName(MaybeAliasName) then
    begin
      WeakAliasType := TPasAliasType.Create;
      with WeakAliasType do
      begin
        // copy properties
        Name := P.Name;
        FullDeclaration:= P.FullDeclaration;
        DeprecatedNote:= P.DeprecatedNote;
        HintDirectives:= P.HintDirectives;

        IsStrongAlias:= false;
        AliasedName:= MaybeAliasName;
      end;
      P.Free;
      P := WeakAliasType;
    end;
  end;

var
  RawDescriptionInfo: TRawDescriptionInfo;
  NormalType: TPasType;
  TypeName: string;
  LCollected, LTemp, TypeNameWithGeneric: string;
  RoutineType: TPasRoutine;
  EnumType: TPasEnum;
  AliasType: TPasAliasType;
  T, T2: TToken;
begin
  { Read the type name, preceded by optional "generic" directive.
    Calculate TypeName, IsGeneric, TypeNameWithGeneric.
    FPC requires "generic" directive, but Delphi doesn't,
    so it's just optional for us (serves for some checks later). }
  T := GetNextToken;
  try
    if IsGeneric = '' then
      TypeNameWithGeneric := ''
    else
      TypeNameWithGeneric := IsGeneric + ' ';

    CheckToken(T, TOK_IDENTIFIER);
    TypeName := T.Data;
    TypeNameWithGeneric := TypeNameWithGeneric + TypeName;
  finally FreeAndNil(T) end;

  DoMessage(5, pmtInformation, 'Parsing type "%s"', [TypeName]);

  RawDescriptionInfo := GetLastComment;
  AttributeIsPossible := False;
  t := GetNextToken(LCollected);
  try
    if T.IsSymbol(SYM_LESS_THAN) then
    begin
      ParseGenericTypeIdentifierList(T, TypeNameWithGeneric);
      T := GetNextToken(LCollected);
    end;

    if T.IsSymbol(SYM_SEMICOLON) then
    begin
      FreeAndNil(T);
      Exit;
    end else
    if T.IsSymbol(SYM_EQUAL) then
    begin
      LCollected := TypeNameWithGeneric + LCollected + T.Data;
      FreeAndNil(T);
    end else
    begin
      FreeAndNil(T);
      DoError('Symbol "=" expected', []);
    end;

    t := GetNextToken(LTemp);
    LCollected := LCollected + LTemp + t.Data;

    if (t.MyType = TOK_KEYWORD) then
      case t.Info.KeyWord of
        KEY_CLASS: begin
            FreeAndNil(t);
            t := GetNextToken(LTemp);
            LCollected := LCollected + LTemp + t.Data;
            if t.IsKeyWord(KEY_OF) then
            begin
              { include "identifier = class of something;" as standard type }
            end else begin
              Scanner.UnGetToken(t);
              ParseCIO(U, TypeName, TypeNameWithGeneric, CIO_CLASS,
                RawDescriptionInfo, False);
              Exit;
            end;
          end;
        KEY_DISPINTERFACE,
        KEY_INTERFACE,
        KEY_OBJECT,
        KEY_RECORD: begin
            ParseCIO(U, TypeName, TypeNameWithGeneric, KeyWordToCioType(t.Info.KeyWord, False),
              RawDescriptionInfo, False);
            FreeAndNil(t);
            Exit;
          end;
        KEY_TYPE: begin
            T2 := PeekNextToken(LTemp);
            if T2.IsStandardDirective(SD_HELPER) then
            begin
              ParseCIO(U, TypeName, TypeNameWithGeneric, CIO_TYPE,
                RawDescriptionInfo, False);
              FreeAndNil(t);
              exit;
            end else
            begin
              ParseStrongAlias(AliasType, TypeName, RawDescriptionInfo);
              if U <> nil then
                U.AddType(AliasType)
              else
                FreeAndNil(AliasType);
              exit;
            end;
          end;
        KEY_PACKED: begin
            FreeAndNil(t);
            t := GetNextToken(LTemp);
            LCollected := LCollected + LTemp + t.Data;
            if t.Info.KeyWord in [KEY_RECORD, KEY_OBJECT, KEY_CLASS] then
            begin
              // for class - no check for "of", no packed classpointers allowed
              ParseCIO(U, TypeName, TypeNameWithGeneric, KeyWordToCioType(t.Info.KeyWord, True),
                RawDescriptionInfo, False);
              FreeAndNil(t);
              Exit;
            end;
          end;
      end;
    if Assigned(t) then begin
      if (t.MyType = TOK_KEYWORD) then begin
        if t.Info.KeyWord in [KEY_FUNCTION, KEY_PROCEDURE] then
        begin
          ParseRoutine(RoutineType, '', t.Data, KeyWordToRoutineType(t.Info.KeyWord),
            RawDescriptionInfo, false, true, '');
          RoutineType.Name := TypeName;
          RoutineType.FullDeclaration :=
            TypeName + ' = ' + RoutineType.FullDeclaration;
          if U <> nil then
            U.AddType(RoutineType)
          else
            FreeAndNil(RoutineType);
          FreeAndNil(t);
          Exit;
        end;
      end;
      if t.IsSymbol(SYM_LEFT_PARENTHESIS) then
      begin
        ParseEnum(EnumType, TypeName, RawDescriptionInfo);
        if U <> nil then
          U.AddType(EnumType)
        else
          FreeAndNil(EnumType);
        FreeAndNil(t);
        Exit;
      end;
      SetLength(LCollected, Length(LCollected)-Length(t.Data));
      Scanner.UnGetToken(t);
    end;
    FreeAndNil(t);

    AttributeIsPossible := True;

    NormalType := TPasType.Create;
    try
      NormalType.FullDeclaration := LCollected;
      SkipDeclaration(NormalType, false);
      NormalType.Name := TypeName;
      TryPromoteToWeakAlias(NormalType);
      NormalType.RawDescriptionInfo^ := RawDescriptionInfo;
      NormalType.SetAttributes(CurrentAttributes);
      ItemsForNextBackComment.ClearAndAdd(NormalType);
      if U <> nil then
        U.AddType(NormalType)  { This is the last line here since "U" owns the
                                 objects, bad luck if adding the item raised an
                                 exception. }
      else
        FreeAndNil(NormalType);
    except
      FreeAndNil(NormalType);
      raise;
    end;
  except
    FreeAndNil(t);
    raise;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseUnit(U: TPasUnit);
begin
  GetAndCheckNextToken(KEY_UNIT);
  U.RawDescriptionInfo^ := GetLastComment;

  { get unit name identifier }
  U.Name := GetAndCheckNextToken(TOK_IDENTIFIER, true);

  ItemsForNextBackComment.ClearAndAdd(U);

  ParseHintDirectives(U);

  { skip semicolon }
  GetAndCheckNextToken(SYM_SEMICOLON);

  { get 'interface' keyword }
  GetAndCheckNextToken(KEY_INTERFACE);

  { now parse the interface section of that unit }
  ParseInterfaceSection(U);

  { get 'implementation' keyword }
  GetAndCheckNextToken(KEY_IMPLEMENTATION);

  { now parse the implementation section of that unit }
  ParseImplementationSection(U);
end;

{ ---------------------------------------------------------------------------- }
procedure TParser.ParseProgramOrLibraryUses(U: TPasUnit);
var
  T: TToken;
begin
  U.RawDescriptionInfo^ := GetLastComment;

  { get program/library name identifier }
  U.Name := GetAndCheckNextToken(TOK_IDENTIFIER);

  ItemsForNextBackComment.ClearAndAdd(U);

  ParseHintDirectives(U);

  { skip semicolon }
  GetAndCheckNextToken(SYM_SEMICOLON);

  T := GetNextToken;
  try
    if T.IsKeyWord(KEY_USES) then
      ParseUses(U);
  finally
    FreeAndNil(T);
  end;
end;

procedure TParser.ParseProgram(U: TPasUnit);
begin
  GetAndCheckNextToken(KEY_PROGRAM);
  ParseProgramOrLibraryUses(U);
end;

procedure TParser.ParseLibrary(U: TPasUnit);
begin
  GetAndCheckNextToken(KEY_LIBRARY);
  ParseProgramOrLibraryUses(U);
end;

procedure TParser.AddDirectives(const Directives: TStringVector);
const
  MacroSeparator = ':=';
var
  D: string;
  IndexMacroSeparator: NativeInt;
begin
  for D in Directives do
  begin
    IndexMacroSeparator := Pos(MacroSeparator, D);
    if IndexMacroSeparator > 0 then
    begin
      Scanner.AddMacro(Copy(D, 1, IndexMacroSeparator - 1),
        SEnding(D, IndexMacroSeparator + Length(MacroSeparator)));
    end else
      Scanner.AddSymbol(D);
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseUnitOrProgram(var U: TPasUnit);
var
  t: TToken;
begin
  U := TPasUnit.Create;
  try
    t := PeekNextToken;
    CheckToken(t, TOK_KEYWORD);
    case t.Info.KeyWord of
      KEY_UNIT:
        begin
          U.IsUnit := True;
          ParseUnit(U);
        end;
      KEY_PROGRAM:
        begin
          U.IsProgram := True;
          ParseProgram(U);
        end;
      KEY_LIBRARY:
        ParseLibrary(U);
      else
        DoError(SExpectedButFound,
          [Format('one of reserved words "%s", "%s" or "%s"',
            [LowerCase(KeyWordArray[KEY_UNIT]), LowerCase(KeyWordArray[KEY_LIBRARY]), LowerCase(KeyWordArray[KEY_PROGRAM])]),
          T.Description]);
    end;
  except
    FreeAndNil(U);
    raise;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseUses(const U: TPasUnit);
var
  T: TToken;
  UsedUnit: string;
begin
  { Parsing uses clause clears the comment, otherwise
    - normal comments before "uses" clause would be assigned to normal unit
      items (like a procedure), which is quite unexpected
      (see ok_comment_over_uses_clause.pas testcase).
    - analogously, back comments after "uses" clause would be assigned to the unit
      description (see ok_comment_over_uses_clause_2.pas testcase).
  }
  IsLastComment := false;
  ItemsForNextBackComment.Clear;

  repeat
    UsedUnit := GetAndCheckNextToken(TOK_IDENTIFIER, true);
    if U <> nil then
      U.UsesUnits.Append(UsedUnit);

    T := GetNextToken;
    try
      if T.IsKeyWord(KEY_IN) then
      begin
        FreeAndNil(T);

        { Below we just ignore the value of next string token.

          We can do this -- because PasDoc (at least for now)
          does not recursively parse units on "uses" clause.
          So we are not interested in the value of
          given string (which should be a file-name (usually relative,
          but absolute is also allowed AFAIK) with given unit.)

          If we will ever want to implement such "recursive parsing
          of units" in PasDoc, we will have to fix this to
          *not* ignore value of token below. }
        GetAndCheckNextToken(TOK_STRING);

        T := GetNextToken;
      end;

      if T.IsSymbol(SYM_COMMA) then Continue else
      if T.IsSymbol(SYM_SEMICOLON) then Break else
        DoError('One of "," or ";" expected', []);
    finally
      FreeAndNil(T);
    end;
  until false;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseCommaSeparatedIdentifiers(Names: TStrings;
  FinalSymbol: TSymbolType;
  RawDescriptions: TRawDescriptionInfoList);
var
  T: TToken;
  FirstIdentifier: boolean;
begin
  FirstIdentifier := true;

  repeat
    Names.Append(GetAndCheckNextToken(TOK_IDENTIFIER));

    { Now we modify FirstIdentifier and append item to RawDescriptions }
    if FirstIdentifier then
    begin
      FirstIdentifier := false;
      if RawDescriptions <> nil then
        RawDescriptions.Append(GetLastComment);
    end else
    if RawDescriptions <> nil then
    begin
      if IsLastComment then
        RawDescriptions.Append(GetLastComment) else
        RawDescriptions.Append(RawDescriptions[RawDescriptions.Count - 1]);
    end;

    T := GetNextToken;
    try
      if (T.MyType <> TOK_SYMBOL) or
        ( (T.Info.SymbolType <> SYM_COMMA) and
          (T.Info.SymbolType <> FinalSymbol) ) then
        DoError('One of symbols "," or "%s" expected', [SymbolNames[FinalSymbol]]);
      if T.Info.SymbolType = FinalSymbol then
        break;
    finally
      FreeAndNil(T);
    end;
  until false;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseVariables(const U: TPasUnit);
begin
  if U <> nil then
    ParseFieldsVariables(U.Variables, false, viPublished, false)
  else
    ParseFieldsVariables(nil, false, viPublished, false);
end;

procedure TParser.ParseFieldsVariables(Items: TPasItems;
  OfObject: boolean; Visibility: TVisibility; IsInRecordCase: boolean;
  const ClassKeyWordString: string = '');

  // Parse variable/field modifiers in FPC.
  // See: http://www.freepascal.org/docs-html/ref/refse19.html for variable
  // modifiers.
  // This consumes some tokens and appends them to ItemCollector.FullDeclaration.
  procedure ParseModifiers(ItemCollector: TPasFieldVariable);
  const
    Modifiers: array [boolean { OfObject }] of set of TStandardDirective =
    ( ([SD_CVAR, SD_EXPORT, SD_EXTERNAL, SD_PUBLIC]),
      ([SD_STATIC]) );
  var
    ModifierFound: Boolean;
    SemicolonFound: boolean;
    ttemp: TToken;
  begin
    repeat
      ttemp := GetNextToken;
      try
        // The first token after the semicolon may be a variable or field modifier.
        // If we see it, we eat it, up to the next semicolon.
        // This does not take into account the "absolute" modifier
        // (which is not preceeded by a semicolon).

        ModifierFound :=
          (ttemp.MyType = TOK_IDENTIFIER) and
          (ttemp.Info.StandardDirective in Modifiers[OfObject]);

        if ModifierFound then
        begin
          ItemCollector.FullDeclaration := ItemCollector.FullDeclaration +  ' ' + ttemp.Data;
          FreeAndNil(ttemp);

          { now eat tokens up to a ";" }
          SemicolonFound := false;
          while not SemicolonFound do
          begin
            ttemp := GetNextToken;
            if ttemp.IsSymbol(SYM_SEMICOLON) then
            begin
              SemicolonFound := True;
              ItemCollector.FullDeclaration := ItemCollector.FullDeclaration +  ttemp.Data;
            end
            else
            begin
              ItemCollector.FullDeclaration := ItemCollector.FullDeclaration +  ' ' + ttemp.Data;
            end;
            FreeAndNil(ttemp);
          end;
        end else
          Scanner.UnGetToken(ttemp);
      except
        ttemp.Free;
        raise;
      end;
    until not ModifierFound;
  end;

var
  NewItem: TPasFieldVariable;
  ItemCollector: TPasFieldVariable;
  m: TPasRoutine;
  t: TToken;
  NewItemNames: TStringList;
  I: Integer;
  RawDescriptions: TRawDescriptionInfoList;
  NewItems: TPasItems;
begin
  NewItemNames := nil;
  RawDescriptions := nil;
  NewItems := nil;
  try
    NewItemNames := TStringList.Create;
    RawDescriptions := TRawDescriptionInfoList.Create;
    { When Items = nil, we will gather NewItems only for internal use,
      so we will free them ourselves. }
    NewItems := TPasItems.Create(Items = nil);

    // allow attributes for fields in classes
    AttributeIsPossible := true;
    ParseCommaSeparatedIdentifiers(NewItemNames, SYM_COLON, RawDescriptions);

    ItemCollector := TPasFieldVariable.Create;
    try
      ItemCollector.FullDeclaration := ':';
      t := GetNextToken(ItemCollector);

      { If symnbol is "(", we will later unget it and read it once again.
        This way SkipDeclaration can read type declaration up to the matching
        parenthesis, which is needed to handle fiels/var declarations with
        inline enumareted type, like
          var MyVar: (One, Two);
      }
      if not t.IsSymbol(SYM_LEFT_PARENTHESIS) then
      begin
        ItemCollector.FullDeclaration := ItemCollector.FullDeclaration + t.Data;
      end;

      if (t.MyType = TOK_KEYWORD) and
         (t.Info.KeyWord in [KEY_FUNCTION, KEY_PROCEDURE]) then
      begin
        { RoutineTypeString for ParseRoutine below is '', because we already included
          t.Data inside ItemCollector.FullDeclaration.
          If RoutineTypeString would be t.Data, then we would incorrectly
          append t.Data twice to ItemCollector.FullDeclaration
          when appending m.FullDeclaration to ItemCollector.FullDeclaration.

          Note that param InitItemsForNextBackComment for ParseRoutine
          below is false. We will free M in the near time, and we don't
          want M to grab back-comment intended for our fields. }
        ParseRoutine(M, '', '', KeyWordToRoutineType(t.Info.KeyWord),
          EmptyRawDescriptionInfo, false, false, '');
        try
          ItemCollector.FullDeclaration :=
            ItemCollector.FullDeclaration + M.FullDeclaration;
        finally
          M.Free;
        end;
        FreeAndNil(t);
        t := GetNextToken(ItemCollector);

        if t.IsSymbol(SYM_EQUAL) then
        begin
          ItemCollector.FullDeclaration :=
            ItemCollector.FullDeclaration + t.Data;
          SkipDeclaration(ItemCollector, IsInRecordCase);
        end else
        begin
          Scanner.UnGetToken(t);
        end;
      end else
      if t.IsKeyWord(KEY_RECORD) then
      begin
        ParseCIO(nil, '', '', CIO_RECORD, EmptyRawDescriptionInfo, IsInRecordCase);
      end else
      if t.IsKeyWord(KEY_PACKED) then
      begin
        FreeAndNil(t);
        t := GetNextToken;
        if t.IsKeyWord(KEY_RECORD) then
        begin
          ParseCIO(nil, '', '', CIO_PACKEDRECORD, EmptyRawDescriptionInfo, IsInRecordCase);
        end else
        begin
          SkipDeclaration(ItemCollector, IsInRecordCase);
        end;
      end else
      begin
        if t.IsSymbol(SYM_LEFT_PARENTHESIS) then
        begin
          Scanner.UnGetToken(t);
        end;
        SkipDeclaration(ItemCollector, IsInRecordCase);
      end;

      { Create and add (to Items and ItemsForNextBackComment and NewItems)
        new items now.
        We must do it, because we want to init ItemsForNextBackComment *now*,
        not later (after ParseModifiers).
        Otherwise we could accidentaly "miss"
        some back-comment while searching for variable/field modifiers
        in ParseModifiers.

        Note that we have to set ItemsForNextBackComment regardless
        of Items being nil or not. Items may be nil when caller is not interested
        in gathering them, e.g. a private fields section.
        Even in this case, we still want to capture back comments and assign them
        to private fields (that will be thrown out later), instead of accidentally
        assigning back comments to previous non-private item.
        See tests/ok_back_comment_private.pas for example when this is important.

        Note that when parsing variable modifiers, Get/PeekNextToken
        inside may actually use ItemsForNextBackComment and clear it,
        that's why we can't count on ItemsForNextBackComment to hold
        all our new items and we have to use NewItems. }
      ItemsForNextBackComment.Clear;
      for I := 0 to NewItemNames.Count - 1 do
      begin
        NewItem := TPasFieldVariable.Create;
        NewItem.Name := NewItemNames[I];
        NewItem.RawDescriptionInfo^ := RawDescriptions[I];
        NewItem.Visibility := Visibility;
        NewItem.SetAttributes(CurrentAttributes);
        if Items <> nil then Items.Add(NewItem);
        NewItems.Add(NewItem);
        ItemsForNextBackComment.Add(NewItem);
      end;
      CurrentAttributes.Clear;

      ParseModifiers(ItemCollector);

      { Now, when whole parsing work is finished, finish initializing NewItems. }
      if Items <> nil then
      begin
        for I := 0 to NewItems.Count - 1 do
        begin
          NewItem := NewItems[I] as TPasFieldVariable;
          NewItem.FullDeclaration := NewItem.Name + ItemCollector.FullDeclaration;
          if ClassKeyWordString <> '' then
          begin
            NewItem.FullDeclaration := ClassKeyWordString
              + ' ' + NewItem.FullDeclaration;
          end;
          NewItem.HintDirectives := ItemCollector.HintDirectives;
          NewItem.DeprecatedNote := ItemCollector.DeprecatedNote;
        end;
      end;
    finally
      ItemCollector.Free;
      t.Free;
    end;
  finally
    NewItemNames.Free;
    RawDescriptions.Free;
    NewItems.Free;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.SetCommentMarkers(const Value: TStringList);
begin
  FCommentMarkers.Assign(Value);
end;

procedure TParser.SetIgnoreMarkers(const Value: TStringList);
begin
  FIgnoreMarkers.Assign(Value);
end;

procedure TParser.SkipDeclaration(const Item: TPasItem; IsInRecordCase: boolean);
var
  EndLevel: Integer;
  IsSemicolon: Boolean;
  PLevel: Integer;
  t: TToken;
  WhitespaceCollectorToAdd: string;
begin
  EndLevel := 0;
  PLevel := 0;
  repeat
    WhitespaceCollectorToAdd := '';
    t := GetNextTokenNotAttribute(Item);
    try
      case t.MyType of
        TOK_SYMBOL:
          case t.Info.SymbolType of
            SYM_LEFT_PARENTHESIS: Inc(PLevel);
            SYM_RIGHT_PARENTHESIS: Dec(PLevel);
          end;
        TOK_KEYWORD:
          case t.Info.KeyWord of
            KEY_END: Dec(EndLevel);
            KEY_RECORD: Inc(EndLevel);
            KEY_LIBRARY:
              if Assigned(Item) then
                Item.HintDirectives := Item.HintDirectives + [hdLibrary];
          end;
        TOK_IDENTIFIER:
          case t.Info.StandardDirective of
            SD_PLATFORM:
              if Assigned(Item) then
                Item.HintDirectives := Item.HintDirectives + [hdPlatform];
            SD_EXPERIMENTAL:
              if Assigned(Item) then
                Item.HintDirectives := Item.HintDirectives + [hdExperimental];
            SD_DEPRECATED:
              begin
                if Assigned(Item) then
                  Item.HintDirectives := Item.HintDirectives + [hdDeprecated];
                while PeekNextToken(WhitespaceCollectorToAdd).MyType = TOK_STRING do
                begin
                  { consume the following string as DeprecatedNote }
                  if Assigned(Item) then
                    Item.FullDeclaration := Item.FullDeclaration + t.Data + WhitespaceCollectorToAdd;
                  FreeAndNil(t);
                  WhitespaceCollectorToAdd := '';
                  t := GetNextTokenNotAttribute(Item);
                  if Assigned(Item) then
                  begin
                    Assert(T.MyType = TOK_STRING); // T is now the same thing we saw with PeekNextToken
                    Item.DeprecatedNote := Item.DeprecatedNote + t.StringContent;
                  end;
                end;
              end;
          end;
      end;
      IsSemicolon := t.IsSymbol(SYM_SEMICOLON);

      { Reason for "EndLevel < 0" condition:
          Within records et al. the last declaration need not be terminated by ;
        Reason for "(PLevel < 0) and IsInRecordCase" condition:
          See autodoc of SkipDeclaration in TParser interface. }
      if (EndLevel < 0) or
         ( (PLevel < 0) and IsInRecordCase ) then
      begin
        Scanner.UnGetToken(t);
        Exit;
      end;

      if Assigned(Item) then Item.FullDeclaration := Item.FullDeclaration + t.Data + WhitespaceCollectorToAdd;

      FreeAndNil(t);
    except
      t.Free;
      raise;
    end;
  until IsSemicolon and (EndLevel = 0) and (PLevel = 0);
end;

{ ---------------------------------------------------------------------------- }

function TParser.PeekNextToken(out WhitespaceCollector: string): TToken;

  { Returns the offset of the next line in string S after position Index }
  function FindNextLine(const S: string; Index: integer): integer;
  begin
    while SCharIs(S, Index, AllChars - WhiteSpaceNL) do Inc(Index);
    while SCharIs(S, Index, WhiteSpaceNL) do Inc(Index);
    if (Index > Length(S)) then
      Result := 0
    else
      Result := Index;
  end;

  { Checks whether string S starts with sub-string SubS at Index.
    Returns true is S starts with SubS, false otherwise. }
  function SStartsWith(const S: string; Index: integer; const SubS: string): boolean;
  var
    I: integer;
  begin
    Result := false;

    if Length(S) < Length(SubS) + Index - 1 then
      Exit;

    for I := 0 to Length(SubS)-1 do
    begin
      if S[Index + I] <> SubS[I+1] then
        Exit;
    end;

    Result := true;
  end;

  { Extracts the documentation comment from T.CommentContent
    (and some other T properties needed for TRawDescriptionInfo)
    to CommentInfo. Always T.MyType must be within TokenCommentTypes.

    T must not be nil.

    The comment is intended to be a "documentation comment",
    i.e. we intend to put it inside output documentation.
    So comment markers, if present,
    are removed from the beginning and end of the data.
    Also, if comment markers were required but were not present,
    then CommentInfo.Content is an empty string.

    Also back-comment marker, the '<', is removed, if exists,
    and BackComment is set to @true. Otherwise BackComment is @false. }
  procedure ExtractDocComment(
    const t: TToken; out CommentInfo: TRawDescriptionInfo;
    out BackComment: boolean);
  const
    BackCommentMarker = '<';
  var
    i: integer;
    Marker: string;
    WasMarker: boolean;
    CurPos: integer;
  begin
    BackComment := false;
    CommentInfo.Content := T.CommentContent;
    CommentInfo.StreamName := T.StreamName;
    CommentInfo.BeginPosition := T.BeginPosition;
    CommentInfo.EndPosition := T.EndPosition;

    // Check if comment should be ignored
    if IgnoreMarkers.Count <> 0 then
    begin
      for i := 0 to IgnoreMarkers.Count - 1 do
        if IsPrefix(IgnoreMarkers[i], CommentInfo.Content) then
        begin
          CommentInfo.Content := '';
          Exit;
        end;
    end;

    if CommentMarkers.Count <> 0 then
    begin
      WasMarker := false;
      for i := 0 to CommentMarkers.Count - 1 do
      begin
        Marker := CommentMarkers[i];
        if IsPrefix(Marker, CommentInfo.Content) then
        begin
          Delete(CommentInfo.Content, 1, Length(Marker));
          WasMarker := true;
          Break;
        end;
      end;

      if (not MarkersOptional) and (not WasMarker) then
      begin
        CommentInfo.Content := '';
        Exit;
      end;
    end;

    if (not (T.MyType = TOK_COMMENT_HELPINSIGHT)) and SCharIs(CommentInfo.Content, 1, BackCommentMarker) then
    begin
      BackComment := true;
      Delete(CommentInfo.Content, 1, Length(BackCommentMarker));
    end;

    { Replace leading characters (Nothing to do for single-line (//) comments) }
    if (IgnoreLeading <> '') and
       (T.MyType in [TOK_COMMENT_PAS, TOK_COMMENT_EXT]) then
    begin
      CurPos := 1;
      repeat
        while SCharIs(CommentInfo.Content, CurPos, WhiteSpace) do Inc(CurPos);
        while SStartsWith(CommentInfo.Content, CurPos, IgnoreLeading) do
          Delete(CommentInfo.Content, CurPos, Length(IgnoreLeading));
        CurPos := FindNextLine(CommentInfo.Content, CurPos);
      until CurPos = 0;
    end;
  end;

var
  T: TToken;
  TBackComment, TIsCStyle, THelpInsight: boolean;
  TCommentInfo: TRawDescriptionInfo;
  i: Integer;
  name, value: string;
  AttribPair: TStringPair;
  innerBrackets: Integer;
  parenthesis: Integer;
  firstToken, WasLineFeed: Boolean;
begin
  Result := nil;
  WhitespaceCollector := ''; WasLineFeed := False;
  repeat
    t := Scanner.PeekToken;
    try
      { when identifier is found, it cannot be attribute until next semicolon }
      if T.MyType = TOK_IDENTIFIER then AttributeIsPossible := False;

      if t.MyType = TOK_SYMBOL then
      begin
        if AttributeIsPossible and T.IsSymbol(SYM_LEFT_BRACKET) then begin
          name := '';
          value := '';
          innerBrackets := 0;
          parenthesis := 0;
          firstToken := True;
          repeat
            Scanner.ConsumeToken;
            FreeAndNil(T);
            t := Scanner.PeekToken;
            { first token is the attribute class, at this moment unevaluated }
            if firstToken then begin
              case t.MyType of
              TOK_IDENTIFIER:
                begin
                  name := t.Data;
                  firstToken := False;
                end;
              TOK_STRING:
                begin
                  { this is GUID, belongs to the interface, but no check for
                  interface only is performed }
                  name := 'GUID';
                  value := '[' + t.Data + ']';
                  firstToken := False;
                end;
              end;
              continue;
            end;

            { check for start of attribute parameters }
            { there might be more nested parenthesis }
            if T.IsSymbol(SYM_LEFT_PARENTHESIS) then Inc(parenthesis);
            if T.IsSymbol(SYM_RIGHT_PARENTHESIS) then begin
              if parenthesis = 0 then DoError('parenthesis do not match.', []);
              Dec(parenthesis);
            end;

            {there might be some square brackets used in attributes parameters,
            ignore them, but count them (example: param is set)}
            if T.IsSymbol(SYM_LEFT_BRACKET) then Inc(innerBrackets);
            if T.IsSymbol(SYM_RIGHT_BRACKET) then begin
              if innerBrackets > 0 then begin
                Dec(innerBrackets);
                value := value + t.Data;
              end else Break;
            end else begin
              { there is list of attributes separated by coma }
              if t.IsSymbol(SYM_COMMA) and (parenthesis = 0) then begin
                AttribPair := TStringPair.Create(name, value);
                CurrentAttributes.Add(AttribPair);
                firstToken := True;
                name := '';
                value := '';
              end else value := value + t.Data;  // anything other
            end;
          until False;

          Scanner.ConsumeToken;
          FreeAndNil(T);
          AttribPair := TStringPair.Create(name, value);
          CurrentAttributes.Add(AttribPair);
        end else
        begin
          Result := t;
          break;
        end;
      end else
      if t.MyType in TokenCommentTypes then
      begin
        Scanner.ConsumeToken;

        { Get info from T }
        ExtractDocComment(T, TCommentInfo, TBackComment);
        TIsCStyle := (t.MyType in [TOK_COMMENT_CSTYLE, TOK_COMMENT_HELPINSIGHT]);
        THelpInsight := t.MyType = TOK_COMMENT_HELPINSIGHT;

        { Automatic back-comments.
          The logic behind is following: this function stops at an identifier and
          in the next call it will start from a whitespace if it's present and the
          next token after a whitespace will be peeked inside the same loop.
          So when we encounter //-style comment, we check if there was a whitespace
          containing line feed. If yes, proceed as usual (comment is at a new line).
          If no, the comment probably should be auto-back-ed. We check if there's
          any items saved for next back comment and if they are, that's our case. }
        if AutoBackComments then
          if (t.MyType = TOK_COMMENT_CSTYLE) and not WasLineFeed and
            (ItemsForNextBackComment.Count > 0) then
            TBackComment := True;

        FreeAndNil(T);

        if TBackComment then
        begin
          if ItemsForNextBackComment.Count = 0 then
            DoMessage(1, pmtWarning,
              '%s: This is a back-comment (comment starting with "<") ' +
              'but there is no item declared right before it: "%s"',
              [Scanner.GetStreamInfo, TCommentInfo.Content]);

          for i := 0 to ItemsForNextBackComment.Count - 1 do
          begin
            // use Trim(), see https://sourceforge.net/p/pasdoc/bugs/89/
            if Trim(ItemsForNextBackComment.PasItemAt[i].RawDescription) <> '' then
              DoMessage(1, pmtWarning,
                '%s: Item %s already has one description, now it''s ' +
                'overridden by back-comment (comment starting with "<"): "%s"',
                [ Scanner.GetStreamInfo,
                  ItemsForNextBackComment.PasItemAt[i].QualifiedName,
                  TCommentInfo.Content]);

            ItemsForNextBackComment.PasItemAt[i].RawDescriptionInfo^ := TCommentInfo;
          end;

          ItemsForNextBackComment.Clear;
        end else
        if IsLastComment and LastCommentWasCStyle and TIsCStyle then
        begin
          { If there are several //-style comments in a row, combine them }
          LastCommentInfo.Content := LastCommentInfo.Content +
            LineEnding + TCommentInfo.Content;
          if LastCommentInfo.StreamName = TCommentInfo.StreamName then
            LastCommentInfo.EndPosition := TCommentInfo.EndPosition else
            // ' ' is used to indicate that there is no
            // single stream containing the entire comment.
            LastCommentInfo.StreamName := ' ';
        end else
        begin
          { This is a normal comment, so fill [Is]LastCommentXxx properties }
          IsLastComment := true;
          LastCommentWasCStyle := TIsCStyle;
          LastCommentHelpInsight := THelpInsight;
          LastCommentInfo := TCommentInfo;
        end;
      end else
      if t.MyType = TOK_WHITESPACE then
      begin
        Scanner.ConsumeToken;
        if (Pos(#10, t.Data) <> 0) or (Pos(#13, t.Data) <> 0) then
          WasLineFeed := True;
        WhitespaceCollector := WhitespaceCollector + t.Data;
        FreeAndNil(t);
      end else
      begin
        Result := t;
        break;
      end;
    except
      t.Free;
      raise;
    end;
  until False;
end;

function TParser.PeekNextToken: TToken;
var
  Dummy: string;
begin
  Result := PeekNextToken(Dummy);
end;

function TParser.PeekNextToken(const WhitespaceCollectorItem: TPasItem): TToken;
var
  WhitespaceCollector: string;
begin
  Result := PeekNextToken(WhitespaceCollector);
  if Assigned(WhitespaceCollectorItem) then
    WhitespaceCollectorItem.FullDeclaration :=
      WhitespaceCollectorItem.FullDeclaration + WhitespaceCollector;
end;

{ ------------------------------------------------------------ }

procedure TParser.ParseHintDirectives(Item: TPasItem;
  const ConsumeFollowingSemicolon: boolean; const ExtendFullDeclaration: boolean);
var
  T: TToken;
  WasDeprecatedDirective: boolean;
begin
  repeat
    WasDeprecatedDirective := false;
    T := PeekNextToken;

    if T.IsStandardDirective(SD_PLATFORM) then
      Item.HintDirectives := Item.HintDirectives + [hdPlatform]
    else
    if T.IsStandardDirective(SD_EXPERIMENTAL) then
      Item.HintDirectives := Item.HintDirectives + [hdExperimental]
    else
    if T.IsStandardDirective(SD_DEPRECATED) then
    begin
      Item.HintDirectives := Item.HintDirectives + [hdDeprecated];
      WasDeprecatedDirective := true;
    end else
    if T.IsKeyWord(KEY_LIBRARY) then
      Item.HintDirectives := Item.HintDirectives + [hdLibrary]
    else
      break;

    if ExtendFullDeclaration then
      Item.FullDeclaration := Item.FullDeclaration + ' ' + T.Data;
    Scanner.ConsumeToken;
    FreeAndNil(T);

    if WasDeprecatedDirective then
    begin
      while PeekNextToken.MyType = TOK_STRING do
      begin
        T := PeekNextToken;
        if ExtendFullDeclaration then
          Item.FullDeclaration := Item.FullDeclaration + ' ' + T.Data;
        Item.DeprecatedNote := Item.DeprecatedNote + T.StringContent;
        Scanner.ConsumeToken;
        FreeAndNil(T);
      end;
    end;

    if ConsumeFollowingSemicolon then
    begin
      T := PeekNextToken;
      if T.IsSymbol(SYM_SEMICOLON) then
      begin
        if ExtendFullDeclaration then
          Item.FullDeclaration := Item.FullDeclaration + T.Data;
        Scanner.ConsumeToken;
        FreeAndNil(T);
      end;
    end;
  until false;
end;

{ ------------------------------------------------------------ }

function TParser.ParseCioMembers(const ACio: TPasCio; var Mode: TItemParseMode;
  const IsInRecordCase: Boolean; var Visibility: TVisibility): Boolean;

  { Parse fields clause, i.e. something like
      NAME1, NAME2, ... : TYPE;
    If AddToFields then adds parsed fields to i.Fields.
    Visibility of created fields is set to given Visibility parameter. }
  procedure ParseFields(AddToFields: Boolean;
    Visibility: TVisibility; const ClassKeyWordString: string);
  var Items: TPasItems;
  begin
    if AddToFields then
      Items := ACio.Fields
    else
      Items := nil;

    { Note: 4th arg for ParseFieldsVariables is always "false",
      not "IsInRecordCase". That's because even if declaration
      of this CIO is within a record case, then we want to
      see record's terminating "end" keyword anyway.
      So it doesn't matter here whether our IsInRecordCase
      parameter is true. }
    ParseFieldsVariables(Items, True, Visibility, False, ClassKeyWordString);
  end;

  // Adds `Item` to `Items` if it should be visible according to `Visibility`
  // Clears back comments and frees the item otherwise
  procedure AddItemIfVisible(var Item: TPasItem; Items: TPasItems; Visibility: TVisibility);
  begin
    if Visibility in ShowVisibilities then
    begin
      Item.Visibility := Visibility;
      Items.Add(Item);
    end
    else begin
      ItemsForNextBackComment.Clear;
      FreeAndNil(Item);
    end;
  end;

var
  ClassKeyWordString: string;
  M: TPasRoutine;
  ConstantParsed: TPasItem;
  p: TPasProperty;
  StrictVisibility: Boolean;
  t, t2: TToken;
begin
  t := nil;
  try
    { ClassKeyWordString is used to include 'class' in
      class methods, properties and variables declarations. }
    ClassKeyWordString := '';
    StrictVisibility := False;
    Result := False;
    repeat
      FreeAndNil(t);
      { Attribute can be just in front of keyword or identifier }
      AttributeIsPossible := True;
      t := GetNextToken;
      AttributeIsPossible := False;

      if (t.IsSymbol(SYM_SEMICOLON)) then
      begin
        { A declaration of type "name = class(ancestor);" }
        Scanner.UnGetToken(T);
        Result := TRUE;
        Break;
      end
      else if (t.MyType = TOK_KEYWORD) then
      begin
        Mode := pmUndefined;
        if StrictVisibility then
          DoError('"strict" found in an unexpected location', []);

        case t.Info.KeyWord of
          KEY_THREADVAR,
          KEY_VAR:
            begin
              if ClassKeyWordString = '' then
              begin
                Mode := pmVar;
                ClassKeyWordString := t.Data;
                ParseFields(Visibility in ShowVisibilities, Visibility,
                  ClassKeyWordString);
                if not (Visibility in ShowVisibilities) then
                  ItemsForNextBackComment.Clear;
                ClassKeyWordString := '';
              end
              else
                ClassKeyWordString := Trim(ClassKeyWordString + ' ' + t.Data);
            end;
          KEY_CLASS: ClassKeyWordString := t.Data;
          KEY_CONSTRUCTOR,
          KEY_DESTRUCTOR,
          KEY_FUNCTION,
          KEY_PROCEDURE:
            begin
              ParseRoutine(M, ClassKeyWordString,
                t.Data, KeyWordToRoutineType(t.Info.KeyWord),
                GetLastComment, true, true, '');

              ClassKeyWordString := '';
              AddItemIfVisible(TPasItem(M), ACio.Methods, Visibility);
            end;
          KEY_END:
            begin
              Result := TRUE;
              FreeAndNil(t);
              Break;
            end;
          KEY_PROPERTY:
            begin
              ParseProperty(p);

              { append ClassKeyWordString to property FullDeclaration,
                to have 'class property Foo: ...'. }
              if ClassKeyWordString <> '' then
              begin
                P.FullDeclaration := ClassKeyWordString + ' ' + P.FullDeclaration;
                ClassKeyWordString := '';
              end;

              AddItemIfVisible(TPasItem(p), ACio.Properties, Visibility);
            end;
          KEY_CASE:
            ParseRecordCase(ACio, False);
          KEY_TYPE:
            begin
              Mode := pmType;
              FreeAndNil(t);
              Exit;
            end;
          KEY_CONST:
            begin
              Mode := pmConst;
              FreeAndNil(t);
              ParseConstant(otCio, ConstantParsed);
              AddItemIfVisible(ConstantParsed, ACio.Fields, Visibility);
            end;
           else
             DoError('Unexpected %s', [T.Description]);
          end; // case
      end
      else if (t.MyType = TOK_IDENTIFIER) then
      begin
        case t.Info.StandardDirective of
          SD_OPERATOR:
            begin
              { Same code as for KEY_CONSTRUCTOR, KEY_DESTRUCTOR,
              KEY_FUNCTION, KEY_PROCEDURE above, something to be optimized. }
              Mode := pmUndefined;
              ParseRoutine(M, ClassKeyWordString, t.Data, ROUTINE_OPERATOR,
                GetLastComment, true, true, '');
              ClassKeyWordString := '';

              AddItemIfVisible(TPasItem(M), ACio.Methods, Visibility);
            end;
          SD_DEFAULT:
            begin
              if StrictVisibility then
                DoError('"strict" found in an unexpected location', []);

              { Note: 2nd arg for SkipDeclaration is always "false",
                not "IsInRecordCase". That's because even if declaration
                of this CIO is within a record case, then we want to
                see record's terminating "end" keyword anyway.
                So it doesn't matter here whether our IsInRecordCase
                parameter is true. }
              SkipDeclaration(nil, false);
              DoMessage(5, pmtInformation, 'Skipped default property keyword.', []);
            end;
          SD_PUBLIC:
            begin
              if StrictVisibility then
                DoError('"strict" found in an unexpected location', []);

              Visibility := viPublic;
              Mode := pmUndefined;
            end;
          SD_PUBLISHED:
            begin
              if StrictVisibility then
                DoError('"strict" found in an unexpected location', []);

              Visibility := viPublished;
              Mode := pmUndefined;
            end;
          SD_PRIVATE:
            begin
              if StrictVisibility then
              begin
                StrictVisibility := False;
                Visibility := viStrictPrivate;
              end
              else
                Visibility := viPrivate;
              Mode := pmUndefined;
            end;
          SD_PROTECTED:
            begin
              if StrictVisibility then
              begin
                StrictVisibility := False;
                Visibility := viStrictProtected;
              end
              else
                Visibility := viProtected;
              Mode := pmUndefined;
            end;
          SD_AUTOMATED:
            begin
              Visibility := viAutomated;
              Mode := pmUndefined;
            end;
          SD_STRICT:
            begin
              StrictVisibility := True;
              Mode := pmUndefined;
            end;
          SD_GENERIC:
            begin
              t2 := PeekNextToken;
              if t2.Info.KeyWord in [KEY_FUNCTION, KEY_PROCEDURE] then
              begin
                 t2 := GetNextToken;
                 try
                   ParseRoutine(M, ClassKeyWordString,
                     t2.Data, KeyWordToRoutineType(t2.Info.KeyWord),
                     GetLastComment, true, true, t.Data);

                   ClassKeyWordString := '';
                   AddItemIfVisible(TPasItem(M), ACio.Methods, Visibility)
                 finally
                   FreeAndNil(t2)
                 end
              end
              else
                 DoError('Unexpected %s', [t2.Description])
            end
          else // case
            Scanner.UnGetToken(T);
            if Mode = pmType then
              Exit;
            if Mode = pmConst then
            begin
              ParseConstant(otCio, ConstantParsed);
              AddItemIfVisible(ConstantParsed, ACio.Fields, Visibility);
            end
            else begin
              ParseFields(Visibility in ShowVisibilities, Visibility,
                ClassKeyWordString);
              if not (Visibility in ShowVisibilities) then
                ItemsForNextBackComment.Clear;
              ClassKeyWordString := '';
            end;
        end; // case
      end;
      FreeAndNil(t);
    until False;
    CurrentAttributes.Clear;

    ParseHintDirectives(ACio);

    t := GetNextToken;
    if not t.IsSymbol(SYM_SEMICOLON) then
    begin
      if IsInRecordCase then
      begin
        if t.IsSymbol(SYM_RIGHT_PARENTHESIS) then
          Scanner.UnGetToken(t)
        else
          DoError('Unexpected symbol at end of sub-record', []);
      end
      else
        DoError('Semicolon at the end of Class / Object / Interface' +
          ' / Record expected', []);
    end;
  finally
    t.Free;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseGenericTypeIdentifierList(var T: TToken; var Content: string);
var
  Level: Cardinal;
begin
  Content := Content + T.Data;
  Level := 1;
  repeat
    FreeAndNil(T);
    T := GetNextToken;
    Content := Content + T.Data;
    if T.IsSymbol(SYM_LESS_THAN) then
      Inc(Level) else
    if T.IsSymbol(SYM_GREATER_THAN) then
      Dec(Level);
  until Level = 0;
  FreeAndNil(T); { free last ">" }
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseCioTypeDecl(out ACio: TPasCio;
  const CioName, CioNameWithGeneric: string; CIOType: TCIOType;
  const RawDescriptionInfo: TRawDescriptionInfo; var Visibility: TVisibility);
var
  Finished: Boolean;
  AncestorName, AncestorFullDeclaration: string;
  t: TToken;
begin
  DoMessage(5, pmtInformation, 'Parsing class/interface/object "%s"', [CioNameWithGeneric]);
  t := nil;
  try
    AttributeIsPossible := True;
    t := GetNextToken;
    AttributeIsPossible := False;

    { Test for forward class definition here:
        class MyClass = class;
      with no ancestor or class members listed after the word class. }
    if t.IsSymbol(SYM_SEMICOLON) then
      // No error, continue the parsing.
      Exit;

    ACio := TPasCio.Create;
    try
      ACio.Name := CioName;
      ACio.NameWithGeneric := CioNameWithGeneric;
      ACio.RawDescriptionInfo^ := RawDescriptionInfo;
      ACio.MyType := CIOType;

      if (CIOType in [ CIO_CLASS, CIO_PACKEDCLASS ] ) and
        (t.MyType = TOK_IDENTIFIER) and
        (t.Info.StandardDirective in [SD_ABSTRACT, SD_SEALED]) then
      begin
        if t.Info.StandardDirective = SD_ABSTRACT then
          ACio.ClassDirective := CT_ABSTRACT
        else
          ACio.ClassDirective := CT_SEALED;
        FreeAndNil(t);
        t := GetNextToken;
      end
      else if (CIOType in [ CIO_CLASS, CIO_RECORD, CIO_TYPE ]) and
        (t.MyType = TOK_IDENTIFIER) and
        (t.Info.StandardDirective = SD_HELPER) then
      begin
        { Class or record helpers are declared as:
          identifierName = class|record helper [(ancestor list)] for TypeIdentifierName
            memberList
          end;

          Ancestor list is optional, records cannot have ancestors, accepted nevertheless.
        }
        ACio.ClassDirective := CT_HELPER;
        FreeAndNil(t);
        t := GetNextToken;
      end;

      { This allows to write back-comment for class declarations like
          TMyClass = class(TMyAncestor) //< back comment
      }
      ItemsForNextBackComment.ClearAndAdd(ACio);

      { get all ancestors; remember, this could look like
        TNewClass = class ( Classes.TClass, MyClasses.TFunkyClass, MoreClasses.YAC) ... end;
        All class ancestors are supposed to be included in the docs!
      }
      { TODO -otwm :
        That's not quite true since multiple inheritance is not supported by
         Delphi/Kylix or FPC. Every entry but the first must be an interface. }
      if t.IsSymbol(SYM_LEFT_PARENTHESIS) then begin
          { optional ancestor introduced by ( }
        FreeAndNil(t);
        Finished := False;
        { outer repeat loop: one ancestor per pass }
        repeat
          FreeAndNil(t);
          t := GetNextToken;

          if t.MyType = TOK_IDENTIFIER then { an ancestor }
          begin
            AncestorFullDeclaration := t.Data;
            AncestorName := t.Data;

            { For FPC-style generic specialization, the "specialize"
              directive is specified before generic name.
              That's easy to handle, just move to the next token. }
            if t.IsStandardDirective(SD_SPECIALIZE) then
            begin
              FreeAndNil(t);
              t := GetNextToken;
              CheckToken(T, TOK_IDENTIFIER);
              AncestorFullDeclaration := AncestorFullDeclaration + ' ' + t.Data;
              AncestorName := t.Data; { previous AncestorName was wrong }
            end;

            { inner repeat loop: one part of the ancestor per name }
            repeat
              FreeAndNil(t);
              t := GetNextToken;
              if not t.IsSymbol(SYM_PERIOD) then
              begin
                Scanner.UnGetToken(t);
                Break; { leave inner repeat loop }
              end;
              FreeAndNil(t);
              t := GetNextToken;
              if t.MyType <> TOK_IDENTIFIER then
                DoError('Expected class, object or interface in ancestor' +
                  ' declaration', []);

              AncestorFullDeclaration := AncestorFullDeclaration + '.' + T.Data;
              AncestorName            := AncestorName            + '.' + T.Data;
            until False;

            { Dumb reading of generic specialization, just blindly consume
              (add to AncestorFullDeclaration) everything between <...>. }
            t := GetNextToken;
            if t.IsSymbol(SYM_LESS_THAN) then
              ParseGenericTypeIdentifierList(T, AncestorFullDeclaration) else
              Scanner.UnGetToken(t);

            { This check secures from later problems (possible infinite loops when processing)
              in case of tricky ancestors, like:

              - "TMyClass = class(TMyClass)" (invalid code) or
              - "TMyClass<T> = class(TMyClass)" (valid code as far as I know in Delphi,
                and it's an unrelated error that for now we would consider it a loop at "TMyClass",
                instead of treating generic and non-generic TMyClass as distinct).

              PasDoc processes ancestors tree in a few places (like
              TPasCio.FindItemInAncestors ,
              TPasCio.GetInheritedItemDescriptions ,
              WriteHierarchy in TGenericHTMLDocGenerator.WriteCIO ) and adding everywhere
              a test to secure from it (e.g. check for maximum recursion depth)
              would complicate that code a lot.
              It is simpler to just avoiding creating an inheritance tree with loops here.

              TODO: This doesn't secure from non-trivial loops, like

                TMyClass2 = class;
                TMyClass1 = class(TMyClass2);
                TMyClass2 = class(TMyClass1);
            }
            if AncestorName = ACio.Name then
              DoMessage(1, pmtWarning,
                'Class or interface refers to itself as ancestor, ignoring to avoid infinite loops when traversing hierarchy: %s', [
                ACio.Name
              ])
            else
              ACio.Ancestors.Add(TStringPair.Create(AncestorName, AncestorFullDeclaration));
          end
          else begin
            if (t.IsSymbol(SYM_COMMA)) then
                { comma, separating two ancestors }
              FreeAndNil(t)
            else begin
              CheckToken(t, SYM_RIGHT_PARENTHESIS);
              FreeAndNil(t);
              Finished := true;
            end;
          end;
        until Finished;
      end
      else begin
        Scanner.UnGetToken(t);
        case ACio.MyType of
          CIO_CLASS, CIO_PACKEDCLASS:
            begin
              if not SameText(ACio.Name, 'tobject') then
                ACio.Ancestors.Add(TStringPair.Create('TObject', 'TObject'));
            end;
          CIO_DISPINTERFACE:
            begin
              if not SameText(ACio.Name, 'idispinterface') then
                ACio.Ancestors.Add(TStringPair.Create('IDispInterface', 'IDispInterface'));
            end;
          CIO_INTERFACE:
            begin
              if not SameText(ACio.Name, 'iinterface') then
                ACio.Ancestors.Add(TStringPair.Create('IInterface', 'IInterface'));
            end;
          CIO_OBJECT, CIO_PACKEDOBJECT:
            begin
              if not SameText(ACio.Name, 'tobject') then
              ACio.Ancestors.Add(TStringPair.Create('TObject', 'TObject'));
            end;
        end;
      end;

      if (CIOType in [ CIO_CLASS, CIO_RECORD, CIO_TYPE ]) and
          (ACio.ClassDirective = CT_HELPER) then
      begin
        t := PeekNextToken;
        if t.IsKeyWord(KEY_FOR) then
        begin
          Scanner.ConsumeToken;
          FreeAndNil(t);

          t := GetNextToken;
          if t.MyType = TOK_IDENTIFIER then
          begin
            ACio.HelperTypeIdentifier := t.Data;
            FreeAndNil(t);
          end
          else
            DoError('Identifier expected but %s found', ['''' + t.Data + '''']);
        end
        else
          DoError('Keyword FOR expected but %s found', ['''' + t.Data + '''']);
      end;

      if ACio.MyType in [ CIO_CLASS, CIO_PACKEDCLASS ] then
      begin
        { Visibility of members at the beginning of a class declaration
          that don't have a specified visibility is controlled
          by ImplicitVisibility value. }
        case ImplicitVisibility of
          ivPublic:
            if Scanner.SwitchOptions['M'] then
              Visibility := viPublished
            else
              Visibility := viPublic;
          ivPublished:
            Visibility := viPublished;
          ivImplicit:
            Visibility := viImplicit;
          else
            raise EInternalParserError.Create('ImplicitVisibility = ??');
        end;
      end
      else
        { Everything besides a class always starts with visibility "public". }
        Visibility := viPublic;

      ACio.SetAttributes(CurrentAttributes);
      { now collect methods, fields and properties }
      { Flag is set when the class is finished     }
      { Code moved to ParseCioMembers              }

    except
      FreeAndNil(ACio);
      raise;
    end;

  finally
    t.Free;
  end;
end;

{ ---------------------------------------------------------------------------- }

procedure TParser.ParseCioEx(const U: TPasUnit;
  const CioName, CioNameWithGeneric: string;
  CIOType: TCIOType; const RawDescriptionInfo: TRawDescriptionInfo;
  const IsInRecordCase: Boolean);

  { TODO: this is mostly a copy&paste of ParseType! Should be merged,
    otherwise modifying one of them always needs to be carefully duplicated. }
  procedure ParseNestedType;
  var
    RawDescriptionInfo: TRawDescriptionInfo;
    NormalType: TPasType;
    TypeName: string;
    LCollected, LTemp, TypeNameWithGeneric: string;
    RoutineType: TPasRoutine;
    EnumType: TPasEnum;
    T: TToken;
    IsGeneric: boolean;
  begin
    { Read the type name, preceded by optional "generic" directive.
      Calculate TypeName, IsGeneric, TypeNameWithGeneric.
      FPC requires "generic" directive, but Delphi doesn't,
      so it's just optional for us (serves for some checks later). }
    T := GetNextToken;
    try
      TypeNameWithGeneric := '';
      IsGeneric := T.IsStandardDirective(SD_GENERIC);
      if IsGeneric then
      begin
        TypeNameWithGeneric := T.Data + ' ';
        TypeName := GetAndCheckNextToken(TOK_IDENTIFIER);
      end else
      begin
        CheckToken(T, TOK_IDENTIFIER);
        TypeName := T.Data;
      end;
      TypeNameWithGeneric := TypeNameWithGeneric + TypeName;
    finally FreeAndNil(T) end;

    RawDescriptionInfo := GetLastComment;
    t := GetNextToken(LCollected);
    try
      if T.IsSymbol(SYM_LESS_THAN) then
      begin
        ParseGenericTypeIdentifierList(T, TypeNameWithGeneric);
        T := GetNextToken(LCollected);
      end;

      if T.IsSymbol(SYM_SEMICOLON) then
      begin
        FreeAndNil(T);
        Exit;
      end else
      if T.IsSymbol(SYM_EQUAL) then
      begin
        LCollected := TypeNameWithGeneric + LCollected + T.Data;
        FreeAndNil(T);
      end else
      begin
        FreeAndNil(T);
        DoError('Symbol "=" expected', []);
      end;

      t := GetNextToken(LTemp);
      LCollected := LCollected + LTemp + t.Data;

      if (t.MyType = TOK_KEYWORD) then
      begin
        FCioSk.Peek.SkipCioDecl := FALSE;
        case t.Info.KeyWord of
          KEY_CLASS:
            begin
              FreeAndNil(t);
              t := GetNextToken(LTemp);
              LCollected := LCollected + LTemp + t.Data;
              if t.IsKeyWord(KEY_OF) then
              begin
                { include "identifier = class of something;" as standard type }
              end
              else begin
                Scanner.UnGetToken(t);
                ParseCioEx(U, TypeName, TypeNameWithGeneric, CIO_CLASS,
                  RawDescriptionInfo, False);
                Exit;
              end;
            end;
          KEY_DISPINTERFACE:
            begin
              FreeAndNil(t);
              ParseCioEx(U, TypeName, TypeNameWithGeneric, CIO_DISPINTERFACE,
                RawDescriptionInfo, False);
              Exit;
            end;
          KEY_INTERFACE:
            begin
              FreeAndNil(t);
              ParseCioEx(U, TypeName, TypeNameWithGeneric, CIO_INTERFACE,
                RawDescriptionInfo, False);
              Exit;
            end;
          KEY_OBJECT:
            begin
              FreeAndNil(t);
              ParseCioEx(U, TypeName, TypeNameWithGeneric, CIO_OBJECT,
                RawDescriptionInfo, False);
              Exit;
            end;
          KEY_RECORD:
            begin
              FreeAndNil(t);
              ParseCioEx(U, TypeName, TypeNameWithGeneric, CIO_RECORD,
                RawDescriptionInfo, False);
              Exit;
            end;
          KEY_PACKED:
            begin
              FreeAndNil(t);
              t := GetNextToken(LTemp);
              LCollected := LCollected + LTemp + t.Data;
              if t.IsKeyWord(KEY_RECORD) then
              begin
                FreeAndNil(t);
                ParseCioEx(U, TypeName, TypeNameWithGeneric, CIO_PACKEDRECORD,
                  RawDescriptionInfo, False);
                exit;
              end
              else if t.IsKeyWord(KEY_OBJECT) then
              begin
                FreeAndNil(t);
                ParseCioEx(U, TypeName, TypeNameWithGeneric, CIO_PACKEDOBJECT,
                  RawDescriptionInfo, False);
                Exit;
              end
              else if t.IsKeyWord(KEY_CLASS) then
              begin
                // no check for "of", no packed classpointers allowed
                FreeAndNil(t);
                ParseCioEx(U, TypeName, TypeNameWithGeneric, CIO_PACKEDCLASS,
                  RawDescriptionInfo, False);
                Exit;
              end;
            end;
        end;
      end;
      if Assigned(t) then
      begin
        if (t.MyType = TOK_KEYWORD) then
        begin
          if t.Info.KeyWord in [KEY_FUNCTION, KEY_PROCEDURE] then
          begin
            ParseRoutine(RoutineType, '', t.Data, KeyWordToRoutineType(t.Info.KeyWord),
            RawDescriptionInfo, false, true, '');
            RoutineType.Name := TypeName;
            RoutineType.FullDeclaration :=
              TypeName + ' = ' + RoutineType.FullDeclaration;
            FCioSk.Peek.Cio.Types.Add(RoutineType);
            FreeAndNil(t);
            FCioSk.Peek.SkipCioDecl := TRUE;
            ParseCioEx(U, TypeName, TypeNameWithGeneric, CIOType, RawDescriptionInfo, False); //recursion
            Exit;
          end;
        end;
        if t.IsSymbol(SYM_LEFT_PARENTHESIS) then
        begin
          ParseEnum(EnumType, TypeName, RawDescriptionInfo);
          EnumType.Visibility := FCioSk.Peek.CurVisibility;
          if FCioSk.Peek.CurVisibility in ShowVisibilities then
            FCioSk.Peek.Cio.Types.Add(EnumType)
          else
            FreeAndNil(EnumType);
          FreeAndNil(t);
          FCioSk.Peek.SkipCioDecl := TRUE;
          ParseCioEx(U, TypeName, TypeNameWithGeneric, CIOType, RawDescriptionInfo, False); //recursion
          Exit;
        end;
        SetLength(LCollected, Length(LCollected)-Length(t.Data));
        Scanner.UnGetToken(t);
      end;
      FreeAndNil(t);

      NormalType := TPasType.Create;
      try
        NormalType.FullDeclaration := LCollected;
        SkipDeclaration(NormalType, false);
        NormalType.Name := TypeName;
        NormalType.RawDescriptionInfo^ := RawDescriptionInfo;
        NormalType.Visibility := FCioSk.Peek.CurVisibility;
        ItemsForNextBackComment.ClearAndAdd(NormalType);
        if FCioSk.Peek.CurVisibility in ShowVisibilities then
          FCioSk.Peek.Cio.Types.Add(NormalType)
        else
          NormalType.Free;
      except
        NormalType.Free;
        raise;
      end;

      FCioSk.Peek.SkipCioDecl := TRUE;
      ParseCioEx(U, TypeName, TypeNameWithGeneric, CIOType, RawDescriptionInfo, False); //recursion
    except
      FreeAndNil(t);
      raise;
    end;
  end;

{ - - - - - - }

{ This is the attempt to change as less as possible and to reuse as much code
  as possible to support nested types. A design change as DoDi suggested in
  PasDoc2 should be considered sooner or later. }

var
  LCio         : TPasCio;
  LHlp         : TPasCioHelper;
  LMode        : TItemParseMode;
  LVisibility  : TVisibility;

begin
  LCio := nil;
  LHlp := nil;

  try
    if FCioSk.Count > 0 then
    begin
      if FCioSk.Peek.SkipCioDecl then
      begin
        LCio  := FCioSk.Peek.Cio;
        LMode := FCioSk.Peek.Mode;
        LVisibility := FCioSk.Peek.CurVisibility;
        FCioSk.Pop.Free;
      end
      else begin
        LMode := pmUndefined;
      end;
    end
    else
      LMode := pmUndefined;

    if not Assigned(LCio) then
      ParseCioTypeDecl(LCio, CioName, CioNameWithGeneric, CioType,
        RawDescriptionInfo, LVisibility);

    if not Assigned(LCio) then
      Exit;

    while ParseCioMembers(LCio, LMode, IsInRecordCase, LVisibility) do
    begin // A Cio completed, nested or outer CIO
      { Clear any orthan comments - do not let them break away from the CIO }
      IsLastComment := false;
      ItemsForNextBackComment.ClearAndAdd(LCio);
      if (FCioSk.Count > 0) then
      begin
        LVisibility := FCioSk.Peek.CurVisibility;
        if LVisibility in ShowVisibilities then
        begin
          LCio.Visibility := LVisibility;
          FCioSk.Peek.Cio.Cios.Add(LCio);
        end
        else
          FreeAndNil(LCio);
        LCio  := FCioSk.Peek.Cio;
        LMode := FCioSk.Peek.Mode;
        FCioSk.Pop.Free;
      end
      else begin
        if Assigned(U) then
        begin
          ItemsForNextBackComment.ClearAndAdd(LCio);
          U.AddCIO(LCio);
          LCio := nil;
        end
        else
          LCio.Free;
        Exit; // Finished
      end;
    end;

    LHlp := TPasCioHelper.Create;
    LHlp.Mode := LMode;
    LHlp.CurVisibility := LVisibility;
    LHlp.Cio := LCio;
    FCioSk.Push(LHlp);
    LHlp := nil;
    LCio := nil;

    ParseNestedType;

  except
    LCio.Free;
    LHlp.Free;
    FCioSk.Clear;
    raise;
  end;
end;

{ TRawDescriptionInfoList ---------------------------------------------------- }

function TRawDescriptionInfoList.GetItems(Index: integer): TRawDescriptionInfo;
begin
  { FItems is a dynarray, so compiler will automatically
    add appropriate range checks here in $R+ mode.
    So no need to explicitly check Index for validity here. }
  Result := FItems[Index];
end;

procedure TRawDescriptionInfoList.Grow;
var
  Delta: integer;
begin
  if Length(FItems) < 16 then begin
    Delta := 4;
  end
  else begin
    Delta := Length(FItems) div 4;
  end;
  SetLength(FItems, Length(FItems) + Delta);
end;

function TRawDescriptionInfoList.Append(Comment: TRawDescriptionInfo): integer;
begin
  if Length(FItems) = Count then Grow;
  FItems[Count] := Comment;
  result := Count;
  Inc(FCount);
end;

constructor TRawDescriptionInfoList.Create;
begin
  inherited;
  SetLength(FItems, 4);
  FCount := 0;
end;

{ TPasCioHelperStack }

procedure TPasCioHelperStack.Clear;
begin
  while Count > 0 do
    Pop.FreeAll;
end;

function TPasCioHelperStack.Peek: TPasCioHelper;
begin
  Result := TPasCioHelper(inherited Peek);
end;

function TPasCioHelperStack.Pop: TPasCioHelper;
begin
  Result := TPasCioHelper(inherited Pop);
end;

function TPasCioHelperStack.Push(AHelper: TPasCioHelper): TPasCioHelper;
begin
  Result := TPasCioHelper(inherited Push(AHelper));
end;

{ TPasCioHelper }

procedure TPasCioHelper.FreeAll;
begin
  FCio.Free;
  Destroy;
end;

end.
