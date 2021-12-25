{
  Copyright 1998-2018 PasDoc developers.

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

{ @abstract(defines all items that can appear within a Pascal unit's interface)
  @created(11 Mar 1999)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @author(Michalis Kamburelis)
  @author(Richard B. Winston <rbwinst@usgs.gov>)
  @author(Damien Honeyford)
  @author(Arno Garrels <first name.name@nospamgmx.de>)

  For each item (type, variable, class etc.) that may appear in a Pascal
  source code file and can thus be taken into the documentation, this unit
  provides an object type which will store name, unit, description and more
  on this item. }

unit PasDoc_Items;

{$I PasDoc_Defines.inc}

interface

uses
  SysUtils,
  PasDoc_Types,
  PasDoc_StringVector,
  PasDoc_ObjectVector,
  PasDoc_Hashes,
  Classes,
  PasDoc_TagManager,
  PasDoc_Serialize,
  PasDoc_SortSettings,
  PasDoc_StringPairVector,
  PasDoc_Tokenizer;

type
  { Visibility of a field/method. }
  TVisibility = (
    { indicates field or method is published }
    viPublished,
    { indicates field or method is public }
    viPublic,
    { indicates field or method is protected }
    viProtected,
    { indicates field or method is strict protected }
    viStrictProtected,
    { indicates field or method is private }
    viPrivate,
    { indicates field or method is strict private }
    viStrictPrivate,
    { indicates field or method is automated }
    viAutomated,
    { implicit visibility, marks the implicit members if user
      used @--implicit-visibility=implicit command-line option. }
    viImplicit
    );

  TVisibilities = set of TVisibility;

const
  VisibilityStr: array[TVisibility] of string[16] =
  (
   'published',
   'public',
   'protected',
   'strict protected',
   'private',
   'strict private',
   'automated',
   'implicit'
  );

  AllVisibilities: TVisibilities = [Low(TVisibility) .. High(TVisibility)];
  DefaultVisibilities: TVisibilities =
    [viProtected, viPublic, viPublished, viAutomated];

type
  { Type of merging intf section and impl section metadata of an item }
  TInfoMergeType = (
    { impl section is not scanned - default behavior }
    imtNone,
    { data is taken from intf, if it's empty - from impl }
    imtPreferIntf,
    { data is concatenated }
    imtJoin,
    { data is taken from impl, if it's empty - from intf }
    imtPreferImpl
  );

const
  InfoMergeTypeStr: array[TInfoMergeType] of string = (
    'none',
    'prefer-interface',
    'join',
    'prefer-implementation'
  );

type
  TPasCio = class;
  TPasMethod = class;
  TPasProperty = class;
  TPasUnit = class;
  TAnchorItem = class;

  TBaseItems = class;
  TPasItems = class;
  TPasMethods = class;
  TPasProperties = class;
  TPasNestedCios = class;
  TPasTypes = class;
  TPasEnum = class;

  { Raw description, in other words: the contents of comment before
    given item. Besides the content, this also
    specifies filename, begin and end positions of given comment. }
  TRawDescriptionInfo = record
    { This is the actual content the comment. }
    Content: string;

    // @name is the name of the TStream from which this comment was read.
    // Will be '' if no comment was found.  It will be ' ' if
    // the comment was somehow read from more than one stream.
    StreamName: string;

    // @name is the position in the stream of the start of the comment.
    BeginPosition: Int64;

    // @name is the position in the stream of the character immediately
    // after the end of the comment describing the item.
    EndPosition: Int64;
  end;
  PRawDescriptionInfo = ^TRawDescriptionInfo;

  { This is a basic item class, that is linkable,
    and has some @link(RawDescription). }
  TBaseItem = class(TSerializable)
  private
    FDetailedDescription: string;
    FFullLink: string;
    FLastMod: string;
    FName: string;
    FAuthors: TStringVector;
    FCreated: string;
    FAutoLinkHereAllowed: boolean;
    FRawDescriptionInfo: TRawDescriptionInfo;

    procedure SetAuthors(const Value: TStringVector);
    function GetRawDescription: string;
    procedure WriteRawDescription(const Value: string);

    procedure StoreAuthorTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
    procedure StoreCreatedTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
    procedure StoreLastModTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
    procedure StoreCVSTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
    procedure PreHandleNoAutoLinkTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
    procedure HandleNoAutoLinkTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
  protected
    { Serialization of TPasItem need to store in stream only data
      that is generated by parser. That's because current approach
      treats "loading from cache" as equivalent to parsing a unit
      and stores to cache right after parsing a unit.
      So what is generated by parser must be written to cache.

      That said,

      @orderedList(
        @item(
          It will not break anything if you will accidentally store
          in cache something that is not generated by parser.
          That's because saving to cache will be done anyway right
          after doing parsing, so properties not initialized by parser
          will have their initial values anyway.
          You're just wasting memory for cache, and some cache
          saving/loading time.)

        @item(
          For now, in implementation of serialize/deserialize we try
          to add even things not generated by parser in a commented out
          code. This way if approach to cache will change some day,
          we will be able to use this code.)
      ) }
    procedure Serialize(const ADestination: TStream); override;
    procedure Deserialize(const ASource: TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    { It registers @link(TTag)s that init @link(Authors),
      @link(Created), @link(LastMod) and remove relevant tags from description.
      You can override it to add more handlers. }
    procedure RegisterTags(TagManager: TTagManager); virtual;

    { Search for an item called ItemName @italic(inside this Pascal item).
      For units, it searches for items declared
      @italic(inside this unit) (like a procedure, or a class in this unit).
      For classes it searches for items declared @italic(within this class)
      (like a method or a property).
      For an enumerated type, it searches for members of this enumerated type.

      All normal rules of ObjectPascal scope apply, which means that
      e.g. if this item is a unit, @name searches for a class named
      ItemName but it @italic(doesn't) search for a method named ItemName
      inside some class of this unit. Just like in ObjectPascal
      the scope of identifiers declared within the class always
      stays within the class. Of course, in ObjectPascal you can
      qualify a method name with a class name, and you can also
      do such qualified links in pasdoc, but this is not handled
      by this routine (see @link(FindName) instead).

      Returns nil if not found.

      Note that it never compares ItemName with Self.Name.
      You may want to check this yourself if you want.

      Note that for TPasItem descendants, it always returns
      also some TPasItem descendant (so if you use this method
      with some TPasItem instance, you can safely cast result
      of this method to TPasItem).

      Implementation in this class always returns nil.
      Override as necessary. }
    function FindItem(const ItemName: string): TBaseItem; virtual;

    { This is just like @link(FindItem), but in case of classes
      or such it should also search within ancestors.
      In this class, the default implementation just calls FindItem. }
    function FindItemMaybeInAncestors(const ItemName: string):
      TBaseItem; virtual;

    { Do all you can to find link specified by NameParts.

      While searching this tries to mimic ObjectPascal identifier scope
      as much as it can. It seaches within this item,
      but also within class enclosing this item,
      within ancestors of this class,
      within unit enclosing this item, then within units used by unit
      of this item. }
    function FindName(const NameParts: TNameParts): TBaseItem; virtual;

    { Detailed description of this item.

      In case of TPasItem, this is something more elaborate
      than @link(TPasItem.AbstractDescription).

      This is already in the form suitable for final output,
      ready to be put inside final documentation. }
    property DetailedDescription: string
      read FDetailedDescription write FDetailedDescription;

    { This stores unexpanded version (as specified
      in user's comment in source code of parsed units)
      of description of this item.

      Actually, this is just a shortcut to
      @code(@link(RawDescriptionInfo).Content) }
    property RawDescription: string
      read GetRawDescription write WriteRawDescription;

    { Full info about @link(RawDescription) of this item,
      including it's filename and position.

      This is intended to be initialized by parser.

      This returns @link(PRawDescriptionInfo) instead of just
      @link(TRawDescriptionInfo) to allow natural setting of
      properties of this record
      (otherwise @longCode(# Item.RawDescriptionInfo.StreamName := 'foo'; #)
      would not work as expected) . }
    function RawDescriptionInfo: PRawDescriptionInfo;

    { a full link that should be enough to link this item from anywhere else }
    property FullLink: string read FFullLink write FFullLink;

    { Contains '' or string with date of last modification.
      This string is already in the form suitable for final output
      format (i.e. already processed by TDocGenerator.ConvertString). }
    property LastMod: string read FLastMod write FLastMod;

    { name of the item }
    property Name: string read FName write FName;

    { Returns the qualified name of the item.
      This is intended to return a concise and not ambigous name.
      E.g. in case of TPasItem it is overridden to return Name qualified
      by class name and unit name.

      In this class this simply returns Name. }
    function QualifiedName: String; virtual;

    { list of strings, each representing one author of this item }
    property Authors: TStringVector read FAuthors write SetAuthors;

    { Contains '' or string with date of creation.
      This string is already in the form suitable for final output
      format (i.e. already processed by TDocGenerator.ConvertString). }
    property Created: string read FCreated;

    { Is auto-link mechanism allowed to create link to this item ?
      This may be set to @false by @@noAutoLinkHere tag in item's description. }
    property AutoLinkHereAllowed: boolean
      read FAutoLinkHereAllowed write FAutoLinkHereAllowed default true;

    { The full (absolute) path used to resolve filenames in this item's descriptions.
      Must always end with PathDelim.
      In this class, this simply returns GetCurrentDir (with PathDelim added if needed). }
    function BasePath: string; virtual;
  end;

  THintDirective = (hdDeprecated, hdPlatform, hdLibrary, hdExperimental);
  THintDirectives = set of THintDirective;

  { This is a @link(TBaseItem) descendant that is always declared inside
    some Pascal source file.

    Parser creates only items of this class
    (e.g. never some basic @link(TBaseItem) instance).
    This class introduces properties and methods pointing
    to parent unit (@link(MyUnit)) and parent class/interface/object/record
    (@link(MyObject)). Also many other things not needed at @link(TBaseItem)
    level are introduced here: things related to handling @@abstract tag,
    @@seealso tag, used to sorting items inside (@link(Sort)) and some more. }
  TPasItem = class(TBaseItem)
  private
    FAbstractDescription: string;
    FAbstractDescriptionWasAutomatic: boolean;
    FVisibility: TVisibility;
    FMyEnum: TPasEnum;
    FMyObject: TPasCio;
    FMyUnit: TPasUnit;
    FHintDirectives: THintDirectives;
    FDeprecatedNote: string;
    FFullDeclaration: string;
    FSeeAlso: TStringPairVector;
    FCachedUnitRelativeQualifiedName: string; //< do not serialize
    FAttributes: TStringPairVector;
    FParams: TStringPairVector;
    FRaises: TStringPairVector;

    procedure StoreAbstractTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
    procedure HandleDeprecatedTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
    procedure HandleSeeAlsoTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
    procedure StoreRaisesTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
    procedure StoreParamTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
    function MyUnitName: String;
  protected
    procedure Serialize(const ADestination: TStream); override;
    procedure Deserialize(const ASource: TStream); override;

    { This does the same thing as @link(FindName) but it @italic(doesn't)
      scan other units. If this item is a unit, it searches only
      inside this unit, else it searches only inside @link(MyUnit)
      unit.

      Actually @link(FindName) uses this function. }
    function FindNameWithinUnit(const NameParts: TNameParts): TBaseItem; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    function FindName(const NameParts: TNameParts): TBaseItem; override;

    procedure RegisterTags(TagManager: TTagManager); override;

    { Abstract description of this item.
      This is intended to be short (e.g. one sentence) description of
      this object.

      This will be inited from @@abstract tag in RawDescription,
      or cutted out from first sentence in RawDescription
      if @--auto-abstract was used.

      Note that this is already in the form suitable for final output,
      with tags expanded, chars converted etc. }
    property AbstractDescription: string
      read FAbstractDescription write FAbstractDescription;

    (*
      TDocGenerator.ExpandDescriptions sets this property to
      true if AutoAbstract was used and AbstractDescription of this
      item was automatically deduced from the 1st sentence of
      RawDescription.

      Otherwise (if @@abstract was specified explicitly, or there
      was no @@abstract and AutoAbstract was false) this is set to false.

      This is a useful hint for generators: it tells them that when they
      are printing @italic(both) AbstractDescription and DetailedDescription of the item
      in one place (e.g. TTexDocGenerator.WriteItemLongDescription
      and TGenericHTMLDocGenerator.WriteItemLongDescription both do this)
      then they should @italic(not) put any additional space between
      AbstractDescription and DetailedDescription.

      This way when user will specify description like

      @longcode(#
        { First sentence. Second sentence. }
        procedure Foo;
      #)

      and @--auto-abstract was on, then "First sentence." is the
      AbstractDescription, " Second sentence." is DetailedDescription,
      AbstractDescriptionWasAutomatic is true and
      and TGenericHTMLDocGenerator.WriteItemLongDescription
      can print them as "First sentence. Second sentence."

      Without this property, TGenericHTMLDocGenerator.WriteItemLongDescription
      would not be able to say that this abstract was deduced automatically
      and would print additional paragraph break that was not present
      in desscription, i.e. "First sentence.<p> Second sentence."
    *)
    property AbstractDescriptionWasAutomatic: boolean
      read FAbstractDescriptionWasAutomatic
      write FAbstractDescriptionWasAutomatic;

    { Returns true if there is a DetailedDescription or AbstractDescription
      available. }
    function HasDescription: Boolean;

    function QualifiedName: String; override;
    function UnitRelativeQualifiedName: string; virtual;

    { Unit of this item. }
    property MyUnit: TPasUnit read FMyUnit write FMyUnit;

    { If this item is part of a class (or record, object., interface...),
      the corresponding class is stored here. @nil otherwise. }
    property MyObject: TPasCio read FMyObject write FMyObject;

    { If this item is a member of an enumerated type,
      then the enclosing enumerated type is stored here. @nil otherwise. }
    property MyEnum: TPasEnum read FMyEnum write FMyEnum;

    property Visibility: TVisibility read FVisibility write FVisibility;

    { Hint directives specify is this item deprecated, platform-specific,
      library-specific, or experimental. }
    property HintDirectives: THintDirectives read FHintDirectives write FHintDirectives;

    { Deprecation note, specified as a string after "deprecated" directive.
      Empty if none, always empty if @link(HintDirectives) does not
      contain hdDeprecated. }
    property DeprecatedNote: string
      read FDeprecatedNote write FDeprecatedNote;

    { This recursively sorts all items inside this item,
      and all items inside these items, etc.
      E.g. in case of TPasUnit, this method sorts all variables,
      consts, CIOs etc. inside (honouring SortSettings),
      and also recursively calls Sort(SortSettings) for every CIO.

      Note that this does not guarantee that absolutely everything
      inside will be really sorted. Some items may be deliberately
      left unsorted, e.g. Members of TPasEnum are never sorted
      (their declared order always matters,
      so we shouldn't sort them when displaying their documentation
      --- reader of such documentation would be seriously misleaded).
      Sorting of other things depends on SortSettings ---
      e.g. without ssMethods, CIOs methods will not be sorted.

      So actually this method @italic(makes sure that all things that should
      be sorted are really sorted). }
    procedure Sort(const SortSettings: TSortSettings); virtual;

     { Full declaration of the item.
       This is full parsed declaration of the given item.

       Note that that this is not used for some descendants.
       Right now it's used only with
       @unorderedList(
         @item TPasConstant
         @item TPasFieldVariable (includes type, default values, etc.)
         @item TPasType
         @item TPasMethod (includes parameter list, procedural directives, etc.)
         @item TPasProperty (includes read/write and storage specifiers, etc.)
         @item(TPasEnum

           But in this special case, '...' is used instead of listing individual
           members, e.g. 'TEnumName = (...)'. You can get list of Members using
           TPasEnum.Members. Eventual specifics of each member should be also
           specified somewhere inside Members items, e.g.
             @longcode# TMyEnum = (meOne, meTwo = 3); #
           and
             @longcode# TMyEnum = (meOne, meTwo); #
           will both result in TPasEnum with equal FullDeclaration
           (just @code('TMyEnum = (...)')) but this @code('= 3') should be
           marked somewhere inside Members[1] properties.)

         @item TPasItem when it's a CIO's field.
       )

       The intention is that in the future all TPasItem descendants
       will always have approprtate FullDeclaration set.
       It all requires adjusting appropriate places in PasDoc_Parser to
       generate appropriate FullDeclaration. }
    property FullDeclaration: string read FFullDeclaration write FFullDeclaration;

    { Items here are collected from @@seealso tags.

      Name of each item is the 1st part of @@seealso parameter.
      Value is the 2nd part of @@seealso parameter. }
    property SeeAlso: TStringPairVector read FSeeAlso;

    { List of attributes defined for this item }
    property Attributes: TStringPairVector read FAttributes;
    procedure SetAttributes(var Value: TStringPairVector);

    function BasePath: string; override;

    { Parameters of method or property.

      Name of each item is the name of parameter (without any surrounding
      whitespace), Value of each item is users description for this item
      (in already-expanded form).

      This is already in the form processed by
      @link(TTagManager.Execute), i.e. with links resolved,
      html characters escaped etc. So @italic(don't) convert them (e.g. before
      writing to the final docs) once again (by some ExpandDescription or
      ConvertString or anything like that). }
    property Params: TStringPairVector read FParams;

    { Exceptions raised by the method, or by property getter/setter.

      Name of each item is the name of exception class (without any surrounding
      whitespace), Value of each item is users description for this item
      (in already-expanded form).

      This is already in the form processed by
      @link(TTagManager.Execute), i.e. with links resolved,
      html characters escaped etc. So @italic(don't) convert them (e.g. before
      writing to the final docs) once again (by some ExpandDescription or
      ConvertString or anything like that). }
    property Raises: TStringPairVector read FRaises;

    { Is optional information (that may be empty for
      after parsing unit and expanding tags) specified.
      Currently this checks @link(Params) and @link(Raises) and
      @link(TPasMethod.Returns). }
    function HasOptionalInfo: boolean; virtual;
  end;

  { @abstract(Pascal constant.)

    Precise definition of "constant" for pasdoc purposes is
    "a name associated with a value".
    Optionally, constant type may also be specified in declararion.
    Well, Pascal constant always has some type, but pasdoc is too weak
    to determine the implicit type of a constant, i.e. to unserstand that
    constand @code(const A = 1) is of type Integer. }
  TPasConstant = class(TPasItem)
  end;

  { @abstract(Pascal global variable or field or nested constant of CIO.)

    Precise definition is "a name with some type". And Optionally with some
    initial value, for global variables. It also holds a nested constant of
    extended classes and records.
    In the future we may introduce here some property like Type: TPasType. }
  TPasFieldVariable = class(TPasItem)
  private
    FIsConstant: Boolean;
  protected
    procedure Serialize(const ADestination: TStream); override;
    procedure Deserialize(const ASource: TStream); override;
  public
    { @abstract(Set if this is a nested constant field) }
    property IsConstant: Boolean read FIsConstant write FIsConstant;
  end;

  { @abstract(Pascal type (but not a procedural type --- these are expressed
    as @link(TPasMethod).)) }
  TPasType = class(TPasItem)
  end;

  { @abstract(Enumerated type.) }
  TPasEnum = class(TPasType)
  protected
    FMembers: TPasItems;
    procedure Serialize(const ADestination: TStream); override;
    procedure Deserialize(const ASource: TStream); override;
    procedure StoreValueTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
  public
    procedure RegisterTags(TagManager: TTagManager); override;

    { Searches for a member of this enumerated type. }
    function FindItem(const ItemName: string): TBaseItem; override;

    destructor Destroy; override;
    constructor Create; override;
    property Members: TPasItems read FMembers;
  end;

  { Methodtype for @link(TPasMethod) }
  TMethodType = (METHOD_CONSTRUCTOR, METHOD_DESTRUCTOR,
    METHOD_FUNCTION, METHOD_PROCEDURE, METHOD_OPERATOR);

  { This represents:
    @orderedList(
      @item global function/procedure,
      @item method (function/procedure of a class/interface/object),
      @item pointer type to one of the above (in this case Name is the type name).
    ) }
  TPasMethod = class(TPasItem)
  protected
    FReturns: string;
    FWhat: TMethodType;
    FDirectives: TStandardDirectives;
    procedure Serialize(const ADestination: TStream); override;
    procedure Deserialize(const ASource: TStream); override;
    procedure StoreReturnsTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
  public
    constructor Create; override;
    destructor Destroy; override;

    { In addition to inherited, this also registers @link(TTag)
      that inits @link(Returns). }
    procedure RegisterTags(TagManager: TTagManager); override;

    { }
    property What: TMethodType read FWhat write FWhat;

    { What does the method return.

      This is already in the form processed by
      @link(TTagManager.Execute), i.e. with links resolved,
      html characters escaped etc. So @italic(don't) convert them (e.g. before
      writing to the final docs) once again (by some ExpandDescription or
      ConvertString or anything like that). }
    property Returns: string read FReturns;

    { Set of method directive flags }
    property Directives: TStandardDirectives read FDirectives write FDirectives;

    function HasOptionalInfo: boolean; override;
  end;

  TPasProperty = class(TPasItem)
  protected
    FDefault: Boolean;
    FNoDefault: Boolean;
    FIndexDecl: string;
    FStoredID: string;
    FDefaultID: string;
    FWriter: string;
    FPropType: string;
    FReader: string;
    procedure Serialize(const ADestination: TStream); override;
    procedure Deserialize(const ASource: TStream); override;
  public
    { contains the optional index declaration, including brackets }
    property IndexDecl: string read FIndexDecl write FIndexDecl;
    { contains the type of the property }
    property Proptype: string read FPropType write FPropType;
    { read specifier }
    property Reader: string read FReader write FReader;
    { write specifier }
    property Writer: string read FWriter write FWriter;
    { true if the property is the default property }
    property Default: Boolean read FDefault write FDefault;
    { keeps default value specifier }
    property DefaultID: string read FDefaultID write FDefaultID;
    { true if Nodefault property }
    property NoDefault: Boolean read FNoDefault write FNoDefault;
    { keeps Stored specifier }
    property StoredId: string read FStoredID write FStoredID;
  end;

  { enumeration type to determine type of @link(TPasCio) item }
  TCIOType = (CIO_CLASS, CIO_PACKEDCLASS,
    CIO_DISPINTERFACE, CIO_INTERFACE,
    CIO_OBJECT, CIO_PACKEDOBJECT,
    CIO_RECORD, CIO_PACKEDRECORD,
    CIO_TYPE);

  TClassDirective = (CT_NONE, CT_ABSTRACT, CT_SEALED, CT_HELPER);

  { @abstract(Extends @link(TPasItem) to store all items in
    a class / an object, e.g. fields.) }
  TPasCio = class(TPasType)
  protected
    FClassDirective: TClassDirective;
    FFields: TPasItems;
    FMethods: TPasMethods;
    FProperties: TPasProperties;
    FAncestors: TStringPairVector;
    FOutputFileName: string;
    FMyType: TCIOType;
    FHelperTypeIdentifier: string;
    FCios: TPasNestedCios;
    FTypes: TPasTypes;
    FNameWithGeneric: string;

    procedure Serialize(const ADestination: TStream); override;
    procedure Deserialize(const ASource: TStream); override;
  protected
    procedure StoreMemberTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
  public
    constructor Create; override;
    destructor Destroy; override;

    { If this class (or interface or object) contains a field, method or
      property with the name of ItemName, the corresponding item pointer is
      returned. }
    function FindItem(const ItemName: string): TBaseItem; override;

    function FindItemMaybeInAncestors(const ItemName: string):
      TBaseItem; override;

    { This searches for item (field, method or property) defined
      in ancestor of this cio. I.e. searches within the FirstAncestor,
      then within FirstAncestor.FirstAncestor, and so on.
      Returns nil if not found. }
    function FindItemInAncestors(const ItemName: string): TPasItem;

    procedure Sort(const SortSettings: TSortSettings); override;

    procedure RegisterTags(TagManager: TTagManager); override;
  public

    { Name of the ancestor (class, object, interface).
      Each item is a TStringPair, with
      @unorderedList(
        @item @code(Name) is the name (single Pascal identifier) of this ancestor,
        @item(@code(Value) is the full declaration of this ancestor.
          For example, in addition to Name, this may include "specialize"
          directive (for FPC generic specialization) at the beginning.
          And "<foo,bar>" section at the end (for FPC or Delphi
          generic specialization).)
        @item(@code(Data) is a TPasItem reference to this ancestor,
          or @nil if not found. This is assigned only in TDocGenerator.BuildLinks.)
      )

      Note that each ancestor is a TPasItem, @italic(not necessarily) TPasCio.
      Consider e.g. the case
      @longcode(#
        TMyStringList = Classes.TStringList;
        TMyExtendedStringList = class(TMyStringList)
          ...
        end;
      #)
      At least for now, such declaration will result in TPasType
      (not TPasCio!) with Name = 'TMyStringList', which means that
      ancestor of TMyExtendedStringList will be a TPasType instance.

      Note that the PasDoc_Parser already takes care of correctly
      setting Ancestors when user didn't specify any ancestor name
      at cio declaration. E.g. if this cio is a class,
      and user didn't specify ancestor name at class declaration,
      and this class name is not 'TObject' (in case pasdoc parses the RTL),
      the Ancestors[0] will be set to 'TObject'. }
    property Ancestors: TStringPairVector read FAncestors;

    { Nested classes (and records, interfaces...). }
    property Cios: TPasNestedCios read FCios;

    {@name is used to indicate whether a class is sealed or abstract.}
    property ClassDirective: TClassDirective read FClassDirective
      write FClassDirective;

    { This returns Ancestors[0].Data, i.e. instance of the first
      ancestor of this Cio (or nil if it couldn't be found),
      or nil if Ancestors.Count = 0. }
    function FirstAncestor: TPasItem;

    { This returns the name of first ancestor of this Cio.

      If Ancestor.Count > 0 then it simply returns Ancestors[0],
      i.e. the name of the first ancestor as was specified at class declaration,
      else it returns ''.

      So this method is @italic(roughly) something like
      @code(FirstAncestor.Name), but with a few notable differences:

      @unorderedList(
        @item(
          FirstAncestor is nil if the ancestor was not found in items parsed
          by pasdoc.
          But this method will still return in this case name of ancestor.)

        @item(@code(FirstAncestor.Name) is the name of ancestor as specified
        at declaration of an ancestor.
        But this method is the name of ancestor as specified at declaration
        of this cio --- with the same letter case, with optional unit specifier.)
      )

      If this function returns '', then you can be sure that
      FirstAncestor returns nil. The other way around is not necessarily true
      --- FirstAncestor may be nil, but still this function may return something
      <> ''. }
    function FirstAncestorName: string;

    { list of all fields }
    property Fields: TPasItems read FFields;

    { Class or record helper type identifier }
    property HelperTypeIdentifier: string read  FHelperTypeIdentifier
                                          write FHelperTypeIdentifier;

    { list of all methods }
    property Methods: TPasMethods read FMethods;

    { list of properties }
    property Properties: TPasProperties read FProperties;

    { determines if this is a class, an interface or an object }
    property MyType: TCIOType read FMyType write FMyType;

    { name of documentation output file (if each class / object gets
      its own file, that's the case for HTML, but not for TeX) }
    property OutputFileName: string read FOutputFileName write FOutputFileName;

    //function QualifiedName: String; override;

    { Is Visibility of items (Fields, Methods, Properties) important ? }
    function ShowVisibility: boolean;

    { Simple nested types (that don't fall into @link(Cios)). }
    property Types: TPasTypes read FTypes;

    { Name, with optional "generic" directive before (for FPC generics)
      and generic type identifiers list "<foo,bar>" after (for FPC and Delphi generics). }
    property NameWithGeneric: string read FNameWithGeneric write FNameWithGeneric;
  end;

  EAnchorAlreadyExists = class(Exception);

  { @name extends @link(TBaseItem) to store extra information about a project.
    @name is used to hold an introduction and conclusion to the project. }
  TExternalItem = class(TBaseItem)
  private
    FSourceFilename: string;
    FTitle: string;
    FShortTitle: string;
    FOutputFileName: string;
    // See @link(Anchors).
    FAnchors: TBaseItems;
    procedure SetOutputFileName(const Value: string);
  protected
    procedure HandleTitleTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
    procedure HandleShortTitleTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
  public
    Constructor Create; override;
    destructor Destroy; override;
    procedure RegisterTags(TagManager: TTagManager); override;
    { name of documentation output file }
    property OutputFileName: string read FOutputFileName write SetOutputFileName;
    property ShortTitle: string read FShortTitle write FShortTitle;
    property SourceFileName: string read FSourceFilename write FSourceFilename;
    property Title: string read FTitle write FTitle;
    function FindItem(const ItemName: string): TBaseItem; override;
    procedure AddAnchor(const AnchorItem: TAnchorItem); overload;

    { If item with Name (case ignored) already exists, this raises
      exception EAnchorAlreadyExists. Otherwise it adds TAnchorItem
      with given name to Anchors. It also returns created TAnchorItem. }
    function AddAnchor(const AnchorName: string): TAnchorItem; overload;

    // @name holds a list of @link(TAnchorItem)s that represent anchors and
    // sections within the @classname. The @link(TAnchorItem)s have no content
    // so, they should not be indexed separately.
    property Anchors: TBaseItems read FAnchors;

    function BasePath: string; override;
  end;

  { @name extends @link(TObjectVector) to store non-nil instances of @link(TExternalItem)}
  TExternalItemList = class(TObjectVector)
  public
    function Get(Index: Integer): TExternalItem;
  end;

  TAnchorItem = class(TBaseItem)
  private
    FExternalItem: TExternalItem;
    FSectionLevel: Integer;
    FSectionCaption: string;
  public
    property ExternalItem: TExternalItem read FExternalItem write FExternalItem;

    { If this is an anchor for a section, this tells section level
      (as was specified in the @@section tag).
      Otherwise this is 0. }
    property SectionLevel: Integer
      read FSectionLevel write FSectionLevel default 0;

    { If this is an anchor for a section, this tells section caption
      (as was specified in the @@section tag). }
    property SectionCaption: string
      read FSectionCaption write FSectionCaption;
  end;

  { extends @link(TPasItem) to store anything about a unit, its constants,
    types etc.; also provides methods for parsing a complete unit.

    Note: Remember to always set @link(CacheDateTime) after
    deserializing this unit. }
  TPasUnit = class(TPasItem)
  protected
    FTypes: TPasTypes;
    FVariables: TPasItems;
    FCIOs: TPasItems;
    FConstants: TPasItems;
    FFuncsProcs: TPasMethods;
    FUsesUnits: TStringVector;
    FSourceFilename: string;
    FOutputFileName: string;
    FCacheDateTime: TDateTime;
    FSourceFileDateTime: TDateTime;
    FIsUnit: boolean;
    FIsProgram: boolean;
    procedure Serialize(const ADestination: TStream); override;
    procedure Deserialize(const ASource: TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AddCIO(const i: TPasCio);
    procedure AddConstant(const i: TPasItem);
    procedure AddType(const i: TPasItem);
    procedure AddVariable(const i: TPasItem);
    function FindInsideSomeClass(const AClassName, ItemInsideClass: string): TPasItem;
    function FindInsideSomeEnum(const EnumName, EnumMember: string): TPasItem;
    function FindItem(const ItemName: string): TBaseItem; override;

    procedure Sort(const SortSettings: TSortSettings); override;
  public
    { list of classes, interfaces, objects, and records defined in this unit }
    property CIOs: TPasItems read FCIOs;
    { list of constants defined in this unit }
    property Constants: TPasItems read FConstants;
    { list of functions and procedures defined in this unit }
    property FuncsProcs: TPasMethods read FFuncsProcs;

    { The names of all units mentioned in a uses clause in the interface
      section of this unit.

      This is never nil.

      After @link(TDocGenerator.BuildLinks), for every i:
      UsesUnits.Objects[i] will point to TPasUnit object with
      Name = UsesUnits[i] (or nil, if pasdoc's didn't parse such unit).
      In other words, you will be able to use UsesUnits.Objects[i] to
      obtain given unit's instance, as parsed by pasdoc. }
    property UsesUnits: TStringVector read FUsesUnits;

    { list of types defined in this unit }
    property Types: TPasTypes read FTypes;
    { list of variables defined in this unit }
    property Variables: TPasItems read FVariables;
    { name of documentation output file
      THIS SHOULD NOT BE HERE! }
    property OutputFileName: string read FOutputFileName write FOutputFileName;

    property SourceFileName: string read FSourceFilename write FSourceFilename;
    property SourceFileDateTime: TDateTime
      read FSourceFileDateTime write FSourceFileDateTime;

    { If WasDeserialized then this specifies the datetime
      of a cache data of this unit, i.e. when cache data was generated.
      If cache was obtained from a file then this is just the cache file
      modification date/time.

      If not WasDeserialized then this property has undefined value --
      don't use it. }
    property CacheDateTime: TDateTime
      read FCacheDateTime write FCacheDateTime;

    { If @false, then this is a program or library file, not a regular unit
      (though it's treated by pasdoc almost like a unit, so we use TPasUnit
      class for this). }
    property IsUnit: boolean read FIsUnit write FIsUnit;
    property IsProgram: boolean read FIsProgram write FIsProgram;

    { Returns if unit WasDeserialized, and file FileName exists,
      and file FileName is newer than CacheDateTime.

      So if FileName contains some info generated from information
      of this unit, then we can somehow assume that FileName still
      contains valid information and we don't have to write
      it once again.

      Sure, we're not really 100% sure that FileName still
      contains valid information, but that's how current approach
      to cache works. }
    function FileNewerThanCache(const FileName: string): boolean;

    function BasePath: string; override;
  end;

  { Container class to store a list of @link(TBaseItem)s. }
  TBaseItems = class(TObjectVector)
  private
    FHash: TObjectHash;
    procedure Serialize(const ADestination: TStream);
    procedure Deserialize(const ASource: TStream);
  public
    constructor Create(const AOwnsObject: Boolean); override;
    destructor Destroy; override;

    { Find a given item name on a list.
      In the base class (TBaseItems), this simply searches the items
      (not recursively).

      In some cases, it may look within the items (recursively),
      when the identifiers inside the item are in same namespace as the items
      themselves. Example: it will look also inside enumerated types
      members, because (when "scoped enums" are off) the enumerated members
      are in the same namespace as the enumerated type name.

      Returns @nil if nothing can be found. }
    function FindListItem(const AName: string): TBaseItem;

    { Inserts all items of C into this collection.
      Disposes C and sets it to nil. }
    procedure InsertItems(const c: TBaseItems);

    { During Add, AObject is associated with AObject.Name using hash table,
      so remember to set AObject.Name @italic(before) calling Add(AObject). }
    procedure Add(const AObject: TBaseItem);

    { This is a shortcut for doing @link(Clear) and then
      @link(Add Add(AObject)). Useful when you want the list
      to contain exactly the one given AObject. }
    procedure ClearAndAdd(const AObject: TBaseItem);

    procedure Delete(const AIndex: Integer);
    procedure Clear; override;
  end;

  { Container class to store a list of @link(TPasItem)s. }
  TPasItems = class(TBaseItems)
  private
    function GetPasItemAt(const AIndex: Integer): TPasItem;
    procedure SetPasItemAt(const AIndex: Integer; const Value: TPasItem);
  public
    { A comfortable routine that just calls inherited and
      casts result to TPasItem, since every item on this list must
      be always TPasItem. }
    function FindListItem(const AName: string): TPasItem;

    { Copies all Items from c to this object, not changing c at all. }
    procedure CopyItems(const c: TPasItems);

    { Counts classes, interfaces and objects within this collection. }
    procedure CountCIO(var c, i, o: Integer);

    { Checks each element's Visibility field and removes all elements with a value
      of viPrivate. }
    procedure RemovePrivateItems;

    property PasItemAt[const AIndex: Integer]: TPasItem read GetPasItemAt
      write SetPasItemAt;

    { This sorts all items on this list by their name,
      and also calls @link(TPasItem.Sort Sort(SortSettings))
      for each of these items.
      This way it sorts recursively everything in this list.

      This is equivalent to doing both
      @link(SortShallow) and @link(SortOnlyInsideItems). }
    procedure SortDeep(const SortSettings: TSortSettings);

    { This calls @link(TPasItem.Sort Sort(SortSettings))
      for each of items on the list.
      It does @italic(not) sort the items on this list. }
    procedure SortOnlyInsideItems(const SortSettings: TSortSettings);

    { This sorts all items on this list by their name.
      Unlike @link(SortDeep), it does @italic(not) call @link(TPasItem.Sort Sort)
      for each of these items.
      So "items inside items" (e.g. class methods, if this list contains
      TPasCio objects) remain unsorted. }
    procedure SortShallow;

    { Sets FullDeclaration of every item to
      @orderedList(
        @item Name of this item (only if PrefixName)
        @item + Suffix.
      )
      Very useful if you have a couple of items that share a common
      declaration in source file, e.g. variables or fields declared like
      @longcode(#
        A, B: Integer;
      #) }
    procedure SetFullDeclaration(PrefixName: boolean; const Suffix: string);
  end;

  { Collection of methods. }
  TPasMethods = class(TPasItems)
    { Find an Index-th item with given name on a list. Index is 0-based.
      There could be multiple items sharing the same name (overloads) while
      method of base class returns only the one most recently added item.

      Returns @nil if nothing can be found. }
    function FindListItem(const AName: string; Index: Integer): TPasMethod; overload;
  end;

  { Collection of properties. }
  TPasProperties = class(TPasItems)
  end;

  { Collection of classes / records / interfaces. }
  TPasNestedCios = class(TPasItems)
  public
    constructor Create; reintroduce;
  end;

  { Collection of types. }
  TPasTypes = class(TPasItems)
    function FindListItem(const AName: string): TPasItem;
  end;

  { Collection of units. }
  TPasUnits = class(TPasItems)
  private
    function GetUnitAt(const AIndex: Integer): TPasUnit;
    procedure SetUnitAt(const AIndex: Integer; const Value: TPasUnit);
  public
    property UnitAt[const AIndex: Integer]: TPasUnit
      read GetUnitAt
      write SetUnitAt;
    function ExistsUnit(const AUnit: TPasUnit): Boolean;
  end;

const
  CIORecordType = [CIO_RECORD, CIO_PACKEDRECORD];
  CIONonHierarchy = CIORecordType;

  EmptyRawDescriptionInfo: TRawDescriptionInfo =
  ( Content: ''; StreamName: ''; BeginPosition: -1; EndPosition: -1; );

{ Returns lowercased keyword associated with given method type. }
function MethodTypeToString(const MethodType: TMethodType): string;

{ Returns VisibilityStr for each value in Visibilities,
  delimited by commas. }
function VisibilitiesToStr(const Visibilities: TVisibilities): string;

function VisToStr(const Vis: TVisibility): string;

implementation

uses StrUtils,
  PasDoc_Utils;

function ComparePasItemsByName(PItem1, PItem2: Pointer): Integer;
var
  P1, P2: TPasItem;
begin
  P1 := TPasItem(PItem1);
  P2 := TPasItem(PItem2);
  Result := CompareText(
    P1.UnitRelativeQualifiedName,
    P2.UnitRelativeQualifiedName);
  // Sort duplicate names by unit name if available.
  if Result = 0 then
    Result := CompareText(
      P1.MyUnitName,
      P2.MyUnitName);
  { If both name and unit are equal (so it's an overloaded routine),
    sort by description. The goal is to make output of AllIdentifiers.html
    and similar lists "stable", guaranteed regardless of sorting algorithm
    used by a particular compiler version, OS etc.

    In case descriptions are equal, the order is still undefined,
    but it will not matter (since everything generated for AllIdentifiers.html
    will be equal). }
  if Result = 0 then
    Result := CompareText(
      P1.DetailedDescription,
      P2.DetailedDescription);
end;

function ComparePasMethods(PItem1, PItem2: Pointer): Integer;
var
  P1: TPasMethod;
  P2: TPasMethod;
begin
  P1 := TPasMethod(PItem1);
  P2 := TPasMethod(PItem2);
  { compare 'method type', order is constructor > destructor > visibility > function, procedure }
  if P1.What = P2.What then begin
    { if 'method type' is equal, compare names }
    if P1.Visibility = P2.Visibility then begin
      Result := CompareText(P1.Name, P2.Name)
    end else begin
      if P1.Visibility < P2.Visibility then begin
        Result := -1
      end else begin
        Result := 1;
      end;
    end;
  end else begin
    if P1.What < P2.What then begin
      Result := -1
    end else begin
      Result := 1;
    end;
  end;
end;

{ TBaseItem ------------------------------------------------------------------- }

constructor TBaseItem.Create;
begin
  inherited Create;
  FAuthors := TStringVector.Create;
  AutoLinkHereAllowed := true;
end;

destructor TBaseItem.Destroy;
begin
  Authors.Free;
  inherited;
end;

function TBaseItem.FindItem(const ItemName: string): TBaseItem;
begin
  Result := nil;
end;

function TBaseItem.FindItemMaybeInAncestors(const ItemName: string):
  TBaseItem;
begin
  Result := FindItem(ItemName);
end;

function TBaseItem.FindName(const NameParts: TNameParts): TBaseItem;
begin
  Result := nil;
end;

procedure TBaseItem.StoreAuthorTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  if TagParameter = '' then exit;
  if Authors = nil then
    FAuthors := NewStringVector;
  Authors.Add(TagParameter);
  ReplaceStr := '';
end;

procedure TBaseItem.StoreCreatedTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  if TagParameter = '' then exit;
  FCreated := TagParameter;
  ReplaceStr := '';
end;

procedure TBaseItem.StoreLastModTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  if TagParameter = '' then exit;
  FLastMod := TagParameter;
  ReplaceStr := '';
end;

procedure TBaseItem.StoreCVSTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);

  function IsVersionControlTag(const S: string;
    const VersionControlTag: string; out TagValue: string): boolean;
  var
    Prefix: string;
  begin
    Prefix := '$' + VersionControlTag + ' ';
    Result := IsPrefix(Prefix, TagParameter);
    if Result then
    begin
      { cut off -1 to cut off final '$' }
      TagValue := Trim(Copy(S, Length(Prefix) + 1,  Length(S) - Length(Prefix) - 1));
    end;
  end;

  {$IFNDEF FPC}
  function TrimRightSet(const AText: string; const ACharS: TSysCharSet): string;
  var
    Length1: Integer;
  begin
    Result := AText;
    Length1 := Length(Result);
    while (Length1 > 0) and (CharInSet(Result[Length1], ACharS)) do begin
      Dec(Length1);
    end;
    SetLength(Result, Length1);
  end;
  {$ENDIF FPC}

var
  TagValue: string;
begin
  if IsVersionControlTag(TagParameter, 'Date:', TagValue) then
  begin
    LastMod := TagValue;
    ReplaceStr := '';
  end else
  if IsVersionControlTag(TagParameter, 'Date::', TagValue) then
  begin
    LastMod := TrimRightSet(TagValue, ['#']);
    ReplaceStr := '';
  end else
  { See http://svnbook.red-bean.com/en/1.7/svn.advanced.props.special.keywords.html
    about fixed date format in SVN. }
  if IsVersionControlTag(TagParameter, 'Author:', TagValue) then
  begin
    if Length(TagValue) > 0 then
    begin
      if not Assigned(Authors) then
        FAuthors := NewStringVector;
      Authors.AddNotExisting(TagValue);
      ReplaceStr := '';
    end;
  end;
end;

procedure TBaseItem.PreHandleNoAutoLinkTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  ReplaceStr := '';
  { We set AutoLinkHereAllowed in the 1st pass of expanding descriptions
    (i.e. in PreHandleNoAutoLinkTag, not in HandleNoAutoLinkTag)
    because all information about AutoLinkHereAllowed must be collected
    before auto-linking happens in the 2nd pass of expanding descriptions. }
  AutoLinkHereAllowed := false;
end;

procedure TBaseItem.HandleNoAutoLinkTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  ReplaceStr := '';
end;

procedure TBaseItem.RegisterTags(TagManager: TTagManager);
begin
  inherited;
  TTag.Create(TagManager, 'author', nil, {$IFDEF FPC}@{$ENDIF} StoreAuthorTag,
    [toParameterRequired]);
  TTag.Create(TagManager, 'created', nil, {$IFDEF FPC}@{$ENDIF} StoreCreatedTag,
    [toParameterRequired, toRecursiveTags, toAllowNormalTextInside]);
  TTag.Create(TagManager, 'lastmod', nil, {$IFDEF FPC}@{$ENDIF} StoreLastModTag,
    [toParameterRequired, toRecursiveTags, toAllowNormalTextInside]);
  TTag.Create(TagManager, 'cvs', nil, {$IFDEF FPC}@{$ENDIF} StoreCVSTag,
    [toParameterRequired]);
  TTopLevelTag.Create(TagManager, 'noautolinkhere',
    {$IFDEF FPC}@{$ENDIF} PreHandleNoAutoLinkTag,
    {$IFDEF FPC}@{$ENDIF} HandleNoAutoLinkTag, []);
end;

procedure TBaseItem.SetAuthors(const Value: TStringVector);
begin
  FAuthors.Assign(Value);
end;

function TBaseItem.QualifiedName: String;
begin
  Result := Name;
end;

procedure TBaseItem.Deserialize(const ASource: TStream);
begin
  inherited;
  Name := LoadStringFromStream(ASource);
  RawDescription := LoadStringFromStream(ASource);

  { No need to serialize, because it's not generated by parser:
  DetailedDescription := LoadStringFromStream(ASource);
  FullLink := LoadStringFromStream(ASource);
  LastMod := LoadStringFromStream(ASource);
  Authors.LoadFromBinaryStream(ASource);
  FCreated := LoadStringFromStream(ASource);
  AutoLinkHereAllowed }
end;

procedure TBaseItem.Serialize(const ADestination: TStream);
begin
  inherited;
  SaveStringToStream(Name, ADestination);
  SaveStringToStream(RawDescription, ADestination);

  { No need to serialize, because it's not generated by parser:
  SaveStringToStream(DetailedDescription, ADestination);
  SaveStringToStream(FullLink, ADestination);
  SaveStringToStream(LastMod, ADestination);
  Authors.SaveToBinaryStream(ADestination);
  SaveStringToStream(Created, ADestination);
  AutoLinkHereAllowed }
end;

function TBaseItem.RawDescriptionInfo: PRawDescriptionInfo;
begin
  Result := @FRawDescriptionInfo;
end;

function TBaseItem.GetRawDescription: string;
begin
  Result := FRawDescriptionInfo.Content;
end;

procedure TBaseItem.WriteRawDescription(const Value: string);
begin
  FRawDescriptionInfo.Content := Value;
end;

function TBaseItem.BasePath: string;
begin
  Result := IncludeTrailingPathDelimiter(GetCurrentDir);
end;

{ TPasItem ------------------------------------------------------------------- }

constructor TPasItem.Create;
begin
  inherited;
  FSeeAlso := TStringPairVector.Create(true);
  FAttributes := TStringPairVector.Create(true);
  FParams := TStringPairVector.Create(true);
  FRaises := TStringPairVector.Create(true);
end;

destructor TPasItem.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FRaises);
  FreeAndNil(FSeeAlso);
  FreeAndNil(FAttributes);
  inherited;
end;

function TPasItem.MyUnitName: String;
begin
  if MyUnit <> nil then
    Result := MyUnit.Name
  else
    Result := '';
end;

function TPasItem.FindNameWithinUnit(const NameParts: TNameParts): TBaseItem;
var
  p: TBaseItem;
  LNameParts0: string;
begin
  Result := nil;
  LNameParts0 := LowerCase(NameParts[0]);
  case Length(NameParts) of
    1: begin
         Result := FindItemMaybeInAncestors(NameParts[0]);
         if Result <> nil then Exit;

         if Assigned(MyObject) then begin { this item is a method or field }
           p := MyObject.FindItemMaybeInAncestors(NameParts[0]);
           if Assigned(p) then begin
             Result := p;
             Exit;
           end;
         end;

         if Assigned(MyUnit) then begin
           p := MyUnit.FindItem(NameParts[0]);
           if Assigned(p) then begin
             Result := p;
             Exit;
           end;
         end;

         if Assigned(MyUnit) and (LNameParts0 = LowerCase(MyUnit.Name)) then begin
           Result := MyUnit;
           Exit;
         end;
       end;

    2: begin
        if Assigned(MyObject) then begin
          if LowerCase(MyObject.Name) = LNameParts0 then begin
            p := MyObject.FindItem(NameParts[1]);
            if Assigned(p) then begin
              Result := p;
              Exit;
            end;
          end;
        end;

        if Assigned(MyUnit) then
        begin
          // To find links inside classes
          p := MyUnit.FindInsideSomeClass(NameParts[0], NameParts[1]);
          if Assigned(p) then begin
            Result := p;
            Exit;
          end;

          // To find links inside enums
          p := MyUnit.FindInsideSomeEnum(NameParts[0], NameParts[1]);
          if Assigned(p) then begin
            Result := p;
            Exit;
          end;
        end;
      end;
  end;
end;

function TPasItem.FindName(const NameParts: TNameParts): TBaseItem;

  procedure SearchUsedUnits(UsesUnits: TStringVector);
  var
    U: TPasUnit;
    i: Integer;
  begin
    for i := 0 to UsesUnits.Count - 1 do
    begin
      U := TPasUnit(UsesUnits.Objects[i]);
      if U <> nil then
      begin
        Result := U.FindNameWithinUnit(NameParts);
        if Result <> nil then Exit;
      end;
    end;
    Result := nil;
  end;

begin
  Result := FindNameWithinUnit(NameParts);

  if Result = nil then
  begin
    { Dirty code: checking for "Self is some class".
      This could be organized better by virtual methods. }
    if Self is TPasUnit then
      SearchUsedUnits(TPasUnit(Self).UsesUnits) else
    if MyUnit <> nil then
      SearchUsedUnits(MyUnit.UsesUnits);
  end;
end;

procedure TPasItem.StoreAbstractTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  if AbstractDescription <> '' then
    ThisTag.TagManager.DoMessage(1, pmtWarning,
      '@abstract tag was already specified for this item. ' +
      'It was specified as "%s"', [AbstractDescription]);
  AbstractDescription := TagParameter;
  ReplaceStr := '';
end;

procedure TPasItem.HandleDeprecatedTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  HintDirectives := HintDirectives + [hdDeprecated];
  ReplaceStr := '';
end;

procedure TPasItem.HandleSeeAlsoTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
var
  Pair: TStringPair;
begin
  Pair := TStringPair.CreateExtractFirstWord(TagParameter);

  if Pair.Name = '' then
  begin
    FreeAndNil(Pair);
    ThisTag.TagManager.DoMessage(2, pmtWarning,
      '@seealso tag doesn''t specify any name to link to, skipped', []);
  end else
  begin
    SeeAlso.Add(Pair);
  end;

  ReplaceStr := '';
end;

{ TODO for StoreRaisesTag and StoreParamTag:
  splitting TagParameter using ExtractFirstWord should be done
  inside TTagManager.Execute, working with raw text, instead
  of here, where the TagParameter is already expanded and converted.

  Actually, current approach works for now perfectly,
  but only because neighter html generator nor LaTeX generator
  change text in such way that first word of the text
  (assuming it's a normal valid Pascal identifier) is changed.

  E.g. '@raises(EFoo with some link @link(Blah))'
  is expanded to 'EFoo with some link <a href="...">Blah</a>'
  so the 1st word ('EFoo') is preserved.

  But this is obviously unclean approach. }

procedure TPasItem.StoreRaisesTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
var
  Pair: TStringPair;
begin
  Pair := TStringPair.CreateExtractFirstWord(TagParameter);

  if Pair.Name = '' then
  begin
    FreeAndNil(Pair);
    ThisTag.TagManager.DoMessage(2, pmtWarning,
      '@raises tag doesn''t specify exception name, skipped', []);
  end else
  begin
    FRaises.Add(Pair);
  end;

  ReplaceStr := '';
end;

procedure TPasItem.StoreParamTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
var
  Pair: TStringPair;
begin
  Pair := TStringPair.CreateExtractFirstWord(TagParameter);

  if Name = '' then
  begin
    FreeAndNil(Pair);
    ThisTag.TagManager.DoMessage(2, pmtWarning,
      '@param tag doesn''t specify parameter name, skipped', []);
  end else
  begin
    FParams.Add(Pair);
  end;

  ReplaceStr := '';
end;

procedure TPasItem.RegisterTags(TagManager: TTagManager);
begin
  inherited;
  TTopLevelTag.Create(TagManager, 'abstract',
    nil, {$IFDEF FPC}@{$ENDIF} StoreAbstractTag,
    [toParameterRequired, toRecursiveTags, toAllowOtherTagsInsideByDefault,
     toAllowNormalTextInside]);
  TTag.Create(TagManager, 'deprecated',
    nil, {$ifdef FPC}@{$endif} HandleDeprecatedTag, []);
  TTopLevelTag.Create(TagManager, 'seealso',
    nil, {$ifdef FPC}@{$endif} HandleSeeAlsoTag,
    [toParameterRequired, toFirstWordVerbatim]);
  TTopLevelTag.Create(TagManager, 'raises',
    nil, {$IFDEF FPC}@{$ENDIF} StoreRaisesTag,
    [toParameterRequired, toRecursiveTags, toAllowOtherTagsInsideByDefault,
     toAllowNormalTextInside, toFirstWordVerbatim]);
  TTopLevelTag.Create(TagManager, 'param',
    nil, {$IFDEF FPC}@{$ENDIF} StoreParamTag,
    [toParameterRequired, toRecursiveTags, toAllowOtherTagsInsideByDefault,
     toAllowNormalTextInside, toFirstWordVerbatim]);
end;

function TPasItem.HasDescription: Boolean;
begin
  HasDescription := (AbstractDescription <> '') or (DetailedDescription <> '');
end;

function TPasItem.HasOptionalInfo: boolean;
begin
  Result :=
    (not ObjectVectorIsNilOrEmpty(Params)) or
    (not ObjectVectorIsNilOrEmpty(Raises));
end;

procedure TPasItem.Sort(const SortSettings: TSortSettings);
begin
  { Nothing to sort in TPasItem }
end;

function TPasItem.QualifiedName: String;
begin
  Result := UnitRelativeQualifiedName;
  if MyUnit <> nil then
    Result := MyUnit.Name + '.' + Result;
end;

function TPasItem.UnitRelativeQualifiedName: String;
var
  LItem: TPasItem;
begin
  if FCachedUnitRelativeQualifiedName <> '' then
    Result := FCachedUnitRelativeQualifiedName
  else begin
    Result := FName;
    LItem := Self;
    while LItem.MyObject <> nil do
    begin
      Result := LItem.MyObject.Name + '.' + Result;
      LItem := LItem.MyObject;
    end;
    FCachedUnitRelativeQualifiedName := Result;
  end;
end;

procedure TPasItem.Deserialize(const ASource: TStream);
begin
  inherited;
  ASource.Read(FVisibility, SizeOf(Visibility));
  ASource.Read(FHintDirectives, SizeOf(FHintDirectives));
  DeprecatedNote := LoadStringFromStream(ASource);
  FullDeclaration := LoadStringFromStream(ASource);
  Attributes.LoadFromBinaryStream(ASource);

  { No need to serialize, because it's not generated by parser:
  AbstractDescription := LoadStringFromStream(ASource);
  ASource.Read(FAbstractDescriptionWasAutomatic,
    SizeOf(FAbstractDescriptionWasAutomatic));
  SeeAlso
  Params.LoadFromBinaryStream(ASource);
  FRaises.LoadFromBinaryStream(ASource);
  }
end;

procedure TPasItem.Serialize(const ADestination: TStream);
begin
  inherited;
  ADestination.Write(FVisibility, SizeOf(Visibility));
  ADestination.Write(FHintDirectives, SizeOf(FHintDirectives));
  SaveStringToStream(DeprecatedNote, ADestination);
  SaveStringToStream(FullDeclaration, ADestination);
  FAttributes.SaveToBinaryStream(ADestination);

  { No need to serialize, because it's not generated by parser:
  SaveStringToStream(AbstractDescription, ADestination);
  ADestination.Write(FAbstractDescriptionWasAutomatic,
    SizeOf(FAbstractDescriptionWasAutomatic));
  SeeAlso
  Params.SaveToBinaryStream(ADestination);
  FRaises.SaveToBinaryStream(ADestination);
  }
end;

procedure TPasItem.SetAttributes(var Value: TStringPairVector);
begin
  if Value.Count > 0 then begin
    FreeAndNil(FAttributes);
    FAttributes := Value;
    Value := TStringPairVector.Create(true);
  end;
end;

function TPasItem.BasePath: string;
begin
  if MyUnit <> nil then
    Result := MyUnit.BasePath
  else
    Result := inherited BasePath; //required by D7
end;

{ TPasEnum ------------------------------------------------------------------- }

constructor TPasEnum.Create;
begin
  inherited Create;
  FMembers := TPasItems.Create(True);
end;

procedure TPasEnum.Deserialize(const ASource: TStream);
begin
  inherited;
  Members.Deserialize(ASource);
end;

destructor TPasEnum.Destroy;
begin
  FMembers.Free;
  inherited;
end;

procedure TPasEnum.RegisterTags(TagManager: TTagManager);
begin
  inherited;
  { Note that @value tag does not have toRecursiveTags,
    and it shouldn't: parameters of this tag will be copied
    verbatim to appropriate member's RawDescription,
    and they will be expanded when this member will be expanded
    by TDocGenerator.ExpandDescriptions.
    This way they will be expanded exactly once, as they should be. }
  TTag.Create(TagManager, 'value',
    nil, {$IFDEF FPC}@{$ENDIF} StoreValueTag,
    [toParameterRequired]);
end;

procedure TPasEnum.Serialize(const ADestination: TStream);
begin
  inherited;
  Members.Serialize(ADestination);
end;

procedure TPasEnum.StoreValueTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
var
  ValueName: String;
  ValueDesc: String;
  Value: TPasItem;
begin
  ReplaceStr := '';
  ValueDesc := TagParameter;
  ValueName := ExtractFirstWord(ValueDesc);

  Value := Members.FindListItem(ValueName);
  if Assigned(Value) then
  begin
    if Value.RawDescription = '' then
      Value.RawDescription := ValueDesc else
      ThisTag.TagManager.DoMessage(1, pmtWarning,
        '@value tag specifies description for a value "%s" that already' +
        ' has one description.', [ValueName]);
  end else
    ThisTag.TagManager.DoMessage(1, pmtWarning,
      '@value tag specifies unknown value "%s"', [ValueName]);
end;

function TPasEnum.FindItem(const ItemName: string): TBaseItem;
begin
  Result := FMembers.FindListItem(ItemName);
end;

{ TPasFieldVariable ---------------------------------------------------------- }

procedure TPasFieldVariable.Deserialize(const ASource: TStream);
begin
  inherited;
  ASource.Read(FIsConstant, SizeOf(FIsConstant));
end;

procedure TPasFieldVariable.Serialize(const ADestination: TStream);
begin
  inherited;
  ADestination.Write(FIsConstant, SizeOf(FIsConstant));
end;

{ TBaseItems ----------------------------------------------------------------- }

constructor TBaseItems.Create(const AOwnsObject: Boolean);
begin
  inherited;
  FHash := TObjectHash.Create;
end;

destructor TBaseItems.Destroy;
begin
  FHash.Free;
  FHash := nil;
  inherited;
end;

procedure TBaseItems.Delete(const AIndex: Integer);
var
  LObj: TBaseItem;
begin
  LObj := TBaseItem(Items[AIndex]);
  FHash.Delete(LowerCase(LObj.Name));
  inherited Delete(AIndex);
end;

function TBaseItems.FindListItem(const AName: string): TBaseItem;
begin
  Result := nil;
  if Length(AName) > 0 then begin
    result := TPasItem(FHash.Items[LowerCase(AName)]);
  end;
end;

procedure TBaseItems.Add(const AObject: TBaseItem);
begin
  inherited Add(AObject);
  FHash.Items[LowerCase(AObject.Name)] := AObject;
end;

procedure TBaseItems.InsertItems(const c: TBaseItems);
var
  i: Integer;
begin
  if ObjectVectorIsNilOrEmpty(c) then Exit;
  for i := 0 to c.Count - 1 do
    Add(TBaseItem(c.Items[i]));
end;

procedure TBaseItems.Clear;
begin
  if Assigned(FHash) then begin
    // not assigned if destroying
    FHash.Free;
    FHash := TObjectHash.Create;
  end;
  inherited;
end;

procedure TBaseItems.Deserialize(const ASource: TStream);
var
  LCount, i: Integer;
begin
  Clear;
  ASource.Read(LCount, SizeOf(LCount));
  for i := 0 to LCount - 1 do
    Add(TBaseItem(TSerializable.DeserializeObject(ASource)));
end;

procedure TBaseItems.Serialize(const ADestination: TStream);
var
  LCount, i: Integer;
begin
  LCount := Count;
  ADestination.Write(LCount, SizeOf(LCount));
  { Remember to always serialize and deserialize items in the
    same order -- this is e.g. checked by ../../tests/scripts/check_cache.sh }
  for i := 0 to Count - 1 do
    TSerializable.SerializeObject(TBaseItem(Items[i]), ADestination);
end;

procedure TBaseItems.ClearAndAdd(const AObject: TBaseItem);
begin
  Clear;
  Add(AObject);
end;

{ TPasItems ------------------------------------------------------------------ }

function TPasItems.FindListItem(const AName: string): TPasItem;
begin
  Result := TPasItem(inherited FindListItem(AName));
end;

procedure TPasItems.CopyItems(const c: TPasItems);
var
  i: Integer;
begin
  if ObjectVectorIsNilOrEmpty(c) then Exit;
  for i := 0 to c.Count - 1 do
    Add(TPasItem(c.GetPasItemAt(i)));
end;

procedure TPasItems.CountCIO(var c, i, o: Integer);
var
  j: Integer;
begin
  c := 0;
  i := 0;
  o := 0;

  for j := 0 to Count - 1 do
    case TPasCio(GetPasItemAt(j)).MyType of
      CIO_CLASS, CIO_PACKEDCLASS:
        Inc(c);
      CIO_INTERFACE:
        Inc(i);
      CIO_OBJECT, CIO_PACKEDOBJECT:
        Inc(o);
    end;
end;

function TPasItems.GetPasItemAt(const AIndex: Integer): TPasItem;
begin
  Result := TPasItem(Items[AIndex]);
end;

procedure TPasItems.RemovePrivateItems;
var
  i: Integer;
  Item: TPasItem;
begin
  i := 0;
  while (i < Count) do begin
    Item := PasItemAt[i];
    if Assigned(Item) and (Item.Visibility = viPrivate) then
      Delete(i)
    else
      Inc(i);
  end;
end;

procedure TPasItems.SetPasItemAt(const AIndex: Integer; const Value:
  TPasItem);
begin
  Items[AIndex] := Value;
end;

procedure TPasItems.SortShallow;
begin
  Sort( {$IFDEF FPC}@{$ENDIF} ComparePasItemsByName);
end;

procedure TPasItems.SortOnlyInsideItems(const SortSettings: TSortSettings);
var i: Integer;
begin
  for i := 0 to Count - 1 do
    PasItemAt[i].Sort(SortSettings);
end;

procedure TPasItems.SortDeep(const SortSettings: TSortSettings);
begin
  SortShallow;
  SortOnlyInsideItems(SortSettings);
end;

procedure TPasItems.SetFullDeclaration(PrefixName: boolean; const Suffix: string);
var i: Integer;
begin
  if PrefixName then
  begin
    for i := 0 to Count - 1 do
      PasItemAt[i].FullDeclaration := PasItemAt[i].Name + Suffix;
  end else
  begin
    for i := 0 to Count - 1 do
      PasItemAt[i].FullDeclaration := Suffix;
  end;
end;

{ TPasMethods ----------------------------------------------------------------- }

function TPasMethods.FindListItem(const AName: string; Index: Integer): TPasMethod;
var i, Counter: Integer;
begin
  Counter := -1;
  for i := 0 to Count - 1 do
    if AnsiSameText(PasItemAt[i].Name, AName) then
    begin
      Inc(Counter);
      if Counter = Index then
      begin
        Result := PasItemAt[i] as TPasMethod;
        Exit;
      end;
    end;
  Result := nil;
end;

{ TPasNestedCios ------------------------------------------------------------- }

constructor TPasNestedCios.Create;
begin
  inherited Create(True);
end;

{ TPasCio -------------------------------------------------------------------- }

constructor TPasCio.Create;
begin
  inherited;
  FClassDirective := CT_NONE;
  FFields := TPasItems.Create(True);
  FMethods := TPasMethods.Create(True);
  FProperties := TPasProperties.Create(True);
  FAncestors := TStringPairVector.Create(True);
  FCios := TPasNestedCios.Create;
  FTypes := TPasTypes.Create(True);
end;

destructor TPasCio.Destroy;
begin
  Ancestors.Free;
  Fields.Free;
  Methods.Free;
  Properties.Free;
  FCios.Free;
  FTypes.Free;
  inherited;
end;

procedure TPasCio.Deserialize(const ASource: TStream);
begin
  inherited;
  FFields.Deserialize(ASource);
  FMethods.Deserialize(ASource);
  FProperties.Deserialize(ASource);
  Ancestors.LoadFromBinaryStream(ASource);
  ASource.Read(FMyType, SizeOf(FMyType));
  ASource.Read(FClassDirective, SizeOf(FClassDirective));
  FHelperTypeIdentifier := LoadStringFromStream(ASource);
  FTypes.Deserialize(ASource);
  FCios.Deserialize(ASource);
  FNameWithGeneric := LoadStringFromStream(ASource);

  { No need to serialize, because it's not generated by parser:
  FOutputFileName := LoadStringFromStream(ASource); }
end;

procedure TPasCio.Serialize(const ADestination: TStream);
begin
  inherited;
  FFields.Serialize(ADestination);
  FMethods.Serialize(ADestination);
  FProperties.Serialize(ADestination);
  Ancestors.SaveToBinaryStream(ADestination);
  ADestination.Write(FMyType, SizeOf(FMyType));
  ADestination.Write(FClassDirective, SizeOf(FClassDirective));
  SaveStringToStream(HelperTypeIdentifier, ADestination);
  FTypes.Serialize(ADestination);
  FCios.Serialize(ADestination);
  SaveStringToStream(NameWithGeneric, ADestination);

  { No need to serialize, because it's not generated by parser:
  SaveStringToStream(FOutputFileName, ADestination); }
end;

function TPasCio.FindItem(const ItemName: string): TBaseItem;
begin
  if Fields <> nil then begin
    Result := Fields.FindListItem(ItemName);
    if Result <> nil then Exit;
  end;

  if Methods <> nil then begin
    Result := Methods.FindListItem(ItemName);
    if Result <> nil then Exit;
  end;

  if Properties <> nil then begin
    Result := Properties.FindListItem(ItemName);
    if Result <> nil then Exit;
  end;

  if FTypes <> nil then begin
    Result := FTypes.FindListItem(ItemName);
    if Result <> nil then Exit;
  end;

  if FCios <> nil then begin
    Result := FCios.FindListItem(ItemName);
    if Result <> nil then Exit;
  end;

  Result := inherited FindItem(ItemName);
end;

function TPasCio.FindItemMaybeInAncestors(const ItemName: string):
  TBaseItem;
begin
  Result := inherited FindItemMaybeInAncestors(ItemName);
  if Result = nil then
    Result := FindItemInAncestors(ItemName);
end;

procedure TPasCio.Sort(const SortSettings: TSortSettings);
begin
  inherited;

  if Fields <> nil then
  begin
    if MyType in CIORecordType then
    begin
      if ssRecordFields in SortSettings then
        Fields.SortShallow;
    end else
    begin
      if ssNonRecordFields in SortSettings then
        Fields.SortShallow;
    end;
  end;

  if (Methods <> nil) and (ssMethods in SortSettings) then
    Methods.Sort( {$IFDEF FPC}@{$ENDIF} ComparePasMethods);

  if (Properties <> nil) and (ssProperties in SortSettings) then
    Properties.SortShallow;

  if (FCios <> nil) then
    FCios.SortDeep(SortSettings);

  if (FTypes <> nil) then
    FTypes.SortDeep(SortSettings);

end;

procedure TPasCio.RegisterTags(TagManager: TTagManager);
begin
  inherited;
  { Note that @member tag does not have toRecursiveTags,
    and it shouldn't: parameters of this tag will be copied
    verbatim to appropriate member's RawDescription,
    and they will be expanded when this member will be expanded
    by TDocGenerator.ExpandDescriptions.

    This way they will be expanded exactly once, as they should be.

    Moreover, this allows you to correctly use tags like @param
    and @raises inside @member for a method. }
  TTag.Create(TagManager, 'member',
    nil, {$IFDEF FPC}@{$ENDIF} StoreMemberTag,
    [toParameterRequired]);
end;

procedure TPasCio.StoreMemberTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
var
  MemberName: String;
  MemberDesc: String;
  Member: TBaseItem;
begin
  ReplaceStr := '';
  MemberDesc := TagParameter;
  MemberName := ExtractFirstWord(MemberDesc);

  Member := FindItem(MemberName);
  if Assigned(Member) then
  begin
    { Only replace the description if one wasn't specified for it
      already }
    if Member.RawDescription = '' then
      Member.RawDescription := MemberDesc else
      ThisTag.TagManager.DoMessage(1, pmtWarning,
        '@member tag specifies description for member "%s" that already' +
        ' has one description.', [MemberName]);
  end else
    ThisTag.TagManager.DoMessage(1, pmtWarning,
      '@member tag specifies unknown member "%s".', [MemberName]);
end;

function TPasCio.ShowVisibility: boolean;
begin
  // Result := not (MyType in CIORecordType);

  { This is always true now, because with "advanced records",
    records have meaningful visibility sections too.
    In the future, maybe we should auto-detect this smarter,
    so that for records (CIORecordType) we only show visibility
    if something is not public.
    (But maybe not, maybe for consistency visibility should be always shown?)
  }

  Result := true;
end;

function TPasCio.FirstAncestor: TPasItem;
begin
  if Ancestors.Count <> 0 then
    Result := TObject(Ancestors[0].Data) as TPasItem else
    Result := nil;
end;

function TPasCio.FirstAncestorName: string;
begin
  if Ancestors.Count <> 0 then
    Result := Ancestors[0].Name else
    Result := '';
end;

function TPasCio.FindItemInAncestors(const ItemName: string): TPasItem;
var Ancestor: TBaseItem;
begin
  Ancestor := FirstAncestor;
  Result := nil;
  while (Result = nil) and (Ancestor <> nil) and (Ancestor is TPasCio) do
  begin
    { TPasCio.FindItem always returns some TPasItem, so the cast below
      of Ancestor.FindItem to TPasItem should always be OK. }
    Result := Ancestor.FindItem(ItemName) as TPasItem;
    Ancestor := TPasCio(Ancestor).FirstAncestor;
  end;
end;

{ TPasUnit ------------------------------------------------------------------- }

constructor TPasUnit.Create;
begin
  inherited Create;
  FTypes := TPasTypes.Create(True);
  FVariables := TPasItems.Create(True);
  FCIOs := TPasItems.Create(True);
  FConstants := TPasItems.Create(True);
  FFuncsProcs := TPasMethods.Create(True);
  FUsesUnits := TStringVector.Create;
end;

destructor TPasUnit.Destroy;
begin
  FCIOs.Free;
  FConstants.Free;
  FFuncsProcs.Free;
  FTypes.Free;
  FUsesUnits.Free;
  FVariables.Free;
  inherited;
end;

procedure TPasUnit.AddCIO(const i: TPasCio);
begin
  CIOs.Add(i);
end;

procedure TPasUnit.AddConstant(const i: TPasItem);
begin
  Constants.Add(i);
end;

procedure TPasUnit.AddType(const i: TPasItem);
begin
  Types.Add(i);
end;

procedure TPasUnit.AddVariable(const i: TPasItem);
begin
  Variables.Add(i);
end;

function TPasUnit.FindInsideSomeClass(const AClassName, ItemInsideClass: string): TPasItem;
var
  po: TPasCio;
begin
  Result := nil;
  if CIOs = nil then Exit;

  po := TPasCio(CIOs.FindListItem(AClassName));
  if Assigned(po) then
    Result := TPasItem(po.FindItem(ItemInsideClass));
end;

function TPasUnit.FindInsideSomeEnum(const EnumName, EnumMember: string): TPasItem;
var
  TypeItem: TPasItem;
begin
  Result := nil;
  if Types = nil then Exit;

  TypeItem := Types.FindListItem(EnumName);
  if Assigned(TypeItem) and (TypeItem is TPasEnum) then
    Result := TPasItem(TPasEnum(TypeItem).FindItem(EnumMember));
end;

function TPasUnit.FindItem(const ItemName: string): TBaseItem;
begin
  if Constants <> nil then begin
    Result := Constants.FindListItem(ItemName);
    if Result <> nil then Exit;
  end;

  if Types <> nil then begin
    Result := Types.FindListItem(ItemName);
    if Result <> nil then Exit;
  end;

  if Variables <> nil then begin
    Result := Variables.FindListItem(ItemName);
    if Result <> nil then Exit;
  end;

  if FuncsProcs <> nil then begin
    Result := FuncsProcs.FindListItem(ItemName);
    if Result <> nil then Exit;
  end;

  if CIOs <> nil then begin
    Result := CIOs.FindListItem(ItemName);
    if Result <> nil then Exit;
  end;

  Result := inherited FindItem(ItemName);
end;

function TPasUnit.FileNewerThanCache(const FileName: string): boolean;
begin
{$IFDEF COMPILER_10_UP}
  Result := WasDeserialized and FileExists(FileName) and
    (CacheDateTime < CheckGetFileDate(FileName));
{$ELSE}
  Result := WasDeserialized and FileExists(FileName) and
    (CacheDateTime < FileDateToDateTime(FileAge(FileName)));
{$ENDIF}
end;

procedure TPasUnit.Sort(const SortSettings: TSortSettings);
begin
  inherited;

  if CIOs <> nil then
  begin
    if ssCIOs in SortSettings then
      CIOs.SortShallow;
    CIOs.SortOnlyInsideItems(SortSettings);
  end;

  if (Constants <> nil) and (ssConstants in SortSettings) then
    Constants.SortShallow;

  if (FuncsProcs <> nil) and (ssFuncsProcs in SortSettings) then
    FuncsProcs.SortShallow;

  if (Types <> nil) and (ssTypes in SortSettings) then
    Types.SortShallow;

  if (Variables <> nil) and (ssVariables in SortSettings) then
    Variables.SortShallow;

  if (UsesUnits <> nil) and (ssUsesClauses in SortSettings) then
    UsesUnits.Sort;
end;

procedure TPasUnit.Deserialize(const ASource: TStream);
begin
  inherited;
  FTypes.Deserialize(ASource);
  FVariables.Deserialize(ASource);
  FCIOs.Deserialize(ASource);
  FConstants.Deserialize(ASource);
  FFuncsProcs.Deserialize(ASource);
  FUsesUnits.LoadFromBinaryStream(ASource);
  ASource.Read(FIsUnit, SizeOf(FIsUnit));
  ASource.Read(FIsProgram, SizeOf(FIsProgram));

  { No need to serialize, because it's not generated by parser:
  FOutputFileName := LoadStringFromStream(ASource);
  FSourceFilename := LoadStringFromStream(ASource);
  SourceFileDateTime := LoadDoubleFromStream(ASource);}
end;

procedure TPasUnit.Serialize(const ADestination: TStream);
begin
  inherited;
  FTypes.Serialize(ADestination);
  FVariables.Serialize(ADestination);
  FCIOs.Serialize(ADestination);
  FConstants.Serialize(ADestination);
  FFuncsProcs.Serialize(ADestination);
  FUsesUnits.SaveToBinaryStream(ADestination);
  ADestination.Write(FIsUnit, SizeOf(FIsUnit));
  ADestination.Write(FIsProgram, SizeOf(FIsProgram));

  { No need to serialize, because it's not generated by parser:
  SaveStringToStream(FOutputFileName, ADestination);
  SaveStringToStream(FSourceFilename, ADestination);
  SaveDoubleToStream(SourceFileDateTime, ADestination); }
end;

function TPasUnit.BasePath: string;
begin
  Result := ExtractFilePath(ExpandFileName(SourceFileName));
end;

{ TPasTypes ------------------------------------------------------------------ }

function TPasTypes.FindListItem(const AName: string): TPasItem;
var
  I: Integer;
begin
  Result := inherited;

  if Result = nil then
  begin
    for I := 0 to Count - 1 do
      if PasItemAt[I] is TPasEnum then
      begin
        Result := TPasEnum(PasItemAt[I]).FindItem(AName) as TPasItem;
        if Result <> nil then
          Exit;
      end;
  end;
end;

{ TPasUnits ------------------------------------------------------------------ }

function TPasUnits.ExistsUnit(const AUnit: TPasUnit): Boolean;
begin
  Result := FindListItem(AUnit.Name) <> nil;
end;

function TPasUnits.GetUnitAt(const AIndex: Integer): TPasUnit;
begin
  Result := TPasUnit(Items[AIndex]);
end;

procedure TPasUnits.SetUnitAt(const AIndex: Integer; const Value: TPasUnit);
begin
  Items[AIndex] := Value;
end;

{ TPasMethod ----------------------------------------------------------------- }

constructor TPasMethod.Create;
begin
  inherited;
end;

destructor TPasMethod.Destroy;
begin
  inherited Destroy;
end;

procedure TPasMethod.StoreReturnsTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  if TagParameter = '' then exit;
  FReturns := TagParameter;
  ReplaceStr := '';
end;

function TPasMethod.HasOptionalInfo: boolean;
begin
  Result :=
    (inherited HasOptionalInfo) or
    (Returns <> '');
end;

procedure TPasMethod.Deserialize(const ASource: TStream);
begin
  inherited;
  ASource.Read(FWhat, SizeOf(FWhat));

  { No need to serialize, because it's not generated by parser:
  FReturns := LoadStringFromStream(ASource);
  }
end;

procedure TPasMethod.Serialize(const ADestination: TStream);
begin
  inherited;
  ADestination.Write(FWhat, SizeOf(FWhat));

  { No need to serialize, because it's not generated by parser:
  SaveStringToStream(FReturns, ADestination);
  }
end;

procedure TPasMethod.RegisterTags(TagManager: TTagManager);
begin
  inherited;
  TTopLevelTag.Create(TagManager, 'returns',
    nil, {$IFDEF FPC}@{$ENDIF} StoreReturnsTag,
    [toParameterRequired, toRecursiveTags, toAllowOtherTagsInsideByDefault,
     toAllowNormalTextInside]);
  TTopLevelTag.Create(TagManager, 'return',
    nil, {$IFDEF FPC}@{$ENDIF} StoreReturnsTag,
    [toParameterRequired, toRecursiveTags, toAllowOtherTagsInsideByDefault,
     toAllowNormalTextInside]);
end;

{ TPasProperty --------------------------------------------------------------- }

procedure TPasProperty.Deserialize(const ASource: TStream);
begin
  inherited;
  ASource.Read(FDefault, SizeOf(FDefault));
  ASource.Read(FNoDefault, SizeOf(FNoDefault));
  FIndexDecl := LoadStringFromStream(ASource);
  FStoredID := LoadStringFromStream(ASource);
  FDefaultID := LoadStringFromStream(ASource);
  FWriter := LoadStringFromStream(ASource);
  FPropType := LoadStringFromStream(ASource);
  FReader := LoadStringFromStream(ASource);
end;

procedure TPasProperty.Serialize(const ADestination: TStream);
begin
  inherited;
  ADestination.Write(FDefault, SizeOf(FDefault));
  ADestination.Write(FNoDefault, SizeOf(FNoDefault));
  SaveStringToStream(FIndexDecl, ADestination);
  SaveStringToStream(FStoredID, ADestination);
  SaveStringToStream(FDefaultID, ADestination);
  SaveStringToStream(FWriter, ADestination);
  SaveStringToStream(FPropType, ADestination);
  SaveStringToStream(FReader, ADestination);
end;

{ TExternalItem ---------------------------------------------------------- }

procedure TExternalItem.AddAnchor(const AnchorItem: TAnchorItem);
begin
  FAnchors.Add(AnchorItem);
end;

function TExternalItem.AddAnchor(const AnchorName: string): TAnchorItem;
begin
  if FindItem(AnchorName) = nil then
  begin
    Result := TAnchorItem.Create;
    Result.Name := AnchorName;
    Result.ExternalItem := Self;
    AddAnchor(Result);
  end else
    raise EAnchorAlreadyExists.CreateFmt(
      'Within "%s" there already exists anchor "%s"',
      [Name, AnchorName]);
end;

constructor TExternalItem.Create;
begin
  inherited;
  FAnchors := TBaseItems.Create(true);
end;

destructor TExternalItem.Destroy;
begin
  FAnchors.Free;
  inherited;
end;

function TExternalItem.FindItem(const ItemName: string): TBaseItem;
begin
  result := nil;
  if FAnchors <> nil then begin
    Result := FAnchors.FindListItem(ItemName);
    if Result <> nil then Exit;
  end;
end;

procedure TExternalItem.HandleShortTitleTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  if ShortTitle <> '' then
    ThisTag.TagManager.DoMessage(1, pmtWarning,
      '@shorttitle tag was already specified for this item. ' +
      'It was specified as "%s"', [ShortTitle]);
  ShortTitle := TagParameter;
  ReplaceStr := '';
end;

procedure TExternalItem.HandleTitleTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  if Title <> '' then
    ThisTag.TagManager.DoMessage(1, pmtWarning,
      '@title tag was already specified for this item. ' +
      'It was specified as "%s"', [Title]);
  Title := TagParameter;
  ReplaceStr := '';
end;

procedure TExternalItem.RegisterTags(TagManager: TTagManager);
begin
  inherited;
  TTopLevelTag.Create(TagManager, 'title',
    nil, {$IFDEF FPC}@{$ENDIF} HandleTitleTag,
    [toParameterRequired]);
  TTopLevelTag.Create(TagManager, 'shorttitle',
    nil, {$IFDEF FPC}@{$ENDIF} HandleShortTitleTag,
    [toParameterRequired]);
end;

procedure TExternalItem.SetOutputFileName(const Value: string);
begin
  FOutputFileName := Value;
end;

function TExternalItem.BasePath: string;
begin
  Result := ExtractFilePath(ExpandFileName(SourceFileName));
end;

{ TExternalItemList ---------------------------------------------------------- }

function TExternalItemList.Get(Index: Integer): TExternalItem;
begin
  Result := inherited Items[Index] as TExternalItem;
end;

{ global things ------------------------------------------------------------ }

function MethodTypeToString(const MethodType: TMethodType): string;
const
  { Maps @link(TMethodType) value to @link(TKeyWord) value.
    When given TMethodType value doesn't correspond to any keyword,
    it maps it to KEY_INVALIDKEYWORD. }
  MethodTypeToKeyWord: array[TMethodType] of TKeyWord =
  ( KEY_CONSTRUCTOR,
    KEY_DESTRUCTOR,
    KEY_FUNCTION,
    KEY_PROCEDURE,
    KEY_INVALIDKEYWORD );
begin
  if MethodType = METHOD_OPERATOR then
    Result := StandardDirectiveArray[SD_OPERATOR] else
    Result := KeyWordArray[MethodTypeToKeyWord[MethodType]];
  Result := LowerCase(Result);
end;

function VisToStr(const Vis: TVisibility): string;
begin
  result := StringReplace(string(VisibilityStr[Vis]), ' ', '', [rfReplaceAll]);
end;

function VisibilitiesToStr(const Visibilities: TVisibilities): string;
var Vis: TVisibility;
begin
  Result := '';
  for Vis := Low(Vis) to High(Vis) do
    if Vis in Visibilities then
    begin
      if Result <> '' then Result := Result + ',';
      Result := Result + VisToStr(Vis);
    end;
end;

initialization
  TSerializable.Register(TPasItem);
  TSerializable.Register(TPasConstant);
  TSerializable.Register(TPasFieldVariable);
  TSerializable.Register(TPasType);
  TSerializable.Register(TPasEnum);
  TSerializable.Register(TPasMethod);
  TSerializable.Register(TPasProperty);
  TSerializable.Register(TPasCio);
  TSerializable.Register(TPasUnit);
end.
