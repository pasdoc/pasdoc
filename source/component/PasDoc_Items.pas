{ @abstract(defines all items that can appear within a Pascal unit's interface)
  @created(11 Mar 1999)
  @cvs($Date$)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Ralf Junker (delphi@zeitungsjunge.de))
  @author(Marco Schmidt (marcoschmidt@geocities.com))
  @author(Michalis Kamburelis)
  @author(Richard B. Winston <rbwinst@usgs.gov>)
  @author(Damien Honeyford)

  For each item (type, variable, class etc.) that may appear in a Pascal
  source code file and can thus be taken into the documentation, this unit
  provides an object type which will store name, unit, description and more
  on this item. }

unit PasDoc_Items;

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
  PasDoc_StringPairVector;

type
  { Visibility of a field/method. }
  TVisibility = (
    viUnknown,
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

var
//common visibility settings of current project
  ShowVisibilities: TVisibilities;

const
  VisibilityStr: array[TVisibility] of string = (
   '',
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

{ Returns VisibilityStr for each value in Visibilities,
  delimited by commas. }
function VisibilitiesToStr(const Visibilities: TVisibilities): string;

function VisToStr(const Vis: TVisibility): string;

type
  { enumeration type to determine type of @link(TPasCio) item }
  TCIOType = (CIO_CLASS, CIO_SPINTERFACE, CIO_INTERFACE, CIO_OBJECT,
    CIO_RECORD, CIO_PACKEDRECORD);

  TCIONames = array[TCIOType] of string;
const
  CIO_NAMES: TCIONames = (
    'class',
    'dispinterface',
    'interface',
    'object',
    'record',
    'packed record');
//for ShowVisisbility
  CioClassTypes = [CIO_CLASS, CIO_SPINTERFACE, CIO_INTERFACE, CIO_OBJECT];
  CIORecordTypes = [CIO_RECORD, CIO_PACKEDRECORD];
  CIONonHierarchy = CIORecordTypes;

type
  { Methodtype for @link(TPasMethod) }
  TMethodType = (METHOD_CONSTRUCTOR, METHOD_DESTRUCTOR,
    METHOD_FUNCTION, METHOD_PROCEDURE, METHOD_OPERATOR);

{ Returns lowercased keyword associated with given method type. }
function MethodTypeToString(const MethodType: TMethodType): string;

type
  TRawDescriptionInfo = class(TStringList)
  public
    destructor Destroy; override;
  end;
  PRawDescriptionInfo = TRawDescriptionInfo;

  TPasItem = class;
  TPasScope = class;
  TPasCio = class;
  TPasMethod = class;
  TPasProperty = class;
  TPasUnit = class;
  TAnchorItem = class;

  TBaseItems = class;
  TPasItems = class;

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
    FRawDescription: string;
    function GetRawDescription: string;
  //allow doc generator to add more descriptions
    procedure WriteRawDescription(const Value: string);

    procedure SetFullLink(const Value: string);
    procedure SetAuthors(const Value: TStringVector);

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
  protected
  { Owner shall be the item, of which this item is a member.
    Usage: construct the fully qualified name...
  }
    FMyOwner: TPasScope;
    FItems: TPasItems; //<FMembers?
  // The declarative token, "unit", "class", "type" etc.
    FKind: TTokenType;
  // All attributes, modifiers etc.
    FAttributes: TPasItemAttributes;
  // position of identifier in the declaration.
    FNamePosition: TTextStreamPos;
    FNameStream: string;
  public
    function  IsKey(AKey: TTokenType): boolean;
    property Kind: TTokenType read FKind;
    property Attributes: TPasItemAttributes read FAttributes write FAttributes;
    property NamePosition: TTextStreamPos read FNamePosition write FNamePosition;
    property NameStream: string read FNameStream write FNameStream;
    property Members: TPasItems read FItems;
    property MyOwner: TPasScope read FMyOwner;  // write FMyOwner;
  public
    constructor Create; override;
    destructor Destroy; override;

    { It registers @link(TTag)s that init @link(Authors),
      @link(Created), @link(LastMod) and remove relevant tags from description.
      You can override it to add more handlers. }
    procedure RegisterTags(TagManager: TTagManager); virtual;

  //add member, override for specialized lists
    procedure AddMember(item: TPasItem); virtual;

    { This searches for item with ItemName @italic(inside this item).
      This means that e.g. for units it checks whether
      there is some item declared in this unit (like procedure, or class).
      For classes this means that some item is declared within the class
      (like method or property).

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

      Implementation in this class searches the Members list.
      Override for items with ancestors, to extend the search
      into the ancestors as well. }
    function FindItem(const ItemName: string): TBaseItem; virtual;

    { This does all it can to resolve link specified by NameParts.

      While searching this tries to mimic ObjectPascal identifier scope
      as much as it can. It seaches within this item,
      but also within class enclosing this item,
      within ancestors of this class,
      within unit enclosing this item, then within units used by unit
      of this item. }
    function FindName(const NameParts: TNameParts; index: integer = -1): TBaseItem; virtual;

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
    }
    property RawDescription: string
      read GetRawDescription write WriteRawDescription;
    //for use by parser, only!
    property Descriptions: TRawDescriptionInfo read FRawDescriptionInfo;

    { Full info about @link(RawDescription) of this item,
      including it's filename and position.

      This is intended to be initialized by parser, and used by an editor.
      Detailed information is not preserved during serialization.
    }
    procedure AddRawDescription(const Value: string; const AStream: string;
      APos: TTextStreamPos); overload;
    //for use by parser
    procedure AddRawDescription(t: TToken); overload;

    { a full link that should be enough to link this item from anywhere else }
    property FullLink: string read FFullLink write SetFullLink;
    //property FullLink: string read FFullLink write FFullLink;

    { Contains '' or string with date of last modification.
      This string is already in the form suitable for final output
      format (i.e. already processed by TDocGenerator.ConvertString). }
    property LastMod: string read FLastMod write FLastMod;

    { name of the item }
    property Name: string read FName write FName;

    { Returns the qualified name of the item.
      This is intended to return a concise and not ambigous name.
      E.g. in case of TPasItem it is overriden to return Name qualified
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
    FFullDeclaration: string;
    FSeeAlso: TStringPairVector;

    procedure StoreAbstractTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
    procedure HandleDeprecatedTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
    procedure HandleSeeAlsoTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
  protected
    procedure Serialize(const ADestination: TStream); override;
    procedure Deserialize(const ASource: TStream); override;

    function  GetAttribute(attr: TPasItemAttribute): boolean;
    procedure SetAttribute(attr: TPasItemAttribute; OnOff: boolean);
    function  GetMyObject: TPasCio;
    procedure SetMyObject(o: TPasCio);
    function  GetMyUnit: TPasUnit;
    procedure SetMyUnit(U: TPasUnit);
  public
    constructor Create(AOwner: TPasScope; AKind: TTokenType;
      const AName: string); reintroduce; virtual;
    destructor Destroy; override;

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

    { pointer to unit this item belongs to }
    property MyUnit: TPasUnit read GetMyUnit write SetMyUnit;

    { if this item is part of an object or class, the corresponding
      info object is stored here, nil otherwise }
    property MyObject: TPasCio read GetMyObject write SetMyObject;

    property HasAttribute[attr: TPasItemAttribute]: boolean
      read GetAttribute write SetAttribute;

  {$IFDEF new}
  //Delphi only, FPC doesn't compile these index directives :-(
    { is this item deprecated? }
    property IsDeprecated: boolean index SD_DEPRECATED
      read GetAttribute write SetAttribute;

    { Is this item platform specific?
      This is decided by "platform" hint directive after an item. }
    property IsPlatformSpecific: boolean index SD_PLATFORM
      read GetAttribute write SetAttribute;

    { Is this item specific to a library ?
      This is decided by "library" hint directive after an item. }
    property IsLibrarySpecific: boolean index SD_LIBRARY
      read GetAttribute write SetAttribute;
  {$ELSE}
  {$ENDIF}

    property Visibility: TVisibility read FVisibility write FVisibility;

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
       generate appropriate FullDeclaration.

       Spaces could be trimmed.
       }
    property FullDeclaration: string read FFullDeclaration write FFullDeclaration;

    { Items here are collected from @@seealso tags.

      Name of each item is the 1st part of @@seealso parameter.
      Value is the 2nd part of @@seealso parameter. }
    property SeeAlso: TStringPairVector read FSeeAlso;

    function BasePath: string; override;
  end;

  TPasItemClass = class of TPasItem;

  { Container class to store a list of @link(TBaseItem)s. }
  TBaseItems = class(TObjectVector)
  private
    FHash: TObjectHash;
    procedure Serialize(const ADestination: TStream);
    procedure Deserialize(const ASource: TStream);
  public
    constructor Create(const AOwnsObject: Boolean); override;
    destructor Destroy; override;

    { Compares each element's name field with Name and returns the item on
      success, nil otherwise.
      Name's case is not regarded. }
    function FindName(const AName: string): TBaseItem;

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
    { Do a FindItem, even if the name suggests something different!
      This is a comfortable routine that just calls inherited
      (ending up in @link(THash.GetObject)), and casts result to TPasItem,
      since every item on this list must be always TPasItem. }
    function FindName(const AName: string): TPasItem;

    { Copies all Items from c to this object, not changing c at all. }
    procedure CopyItems(const c: TPasItems);

    { Counts classes, interfaces and objects within this collection. }
    procedure CountCIO(var c, i, o: Integer);

    // Get last added item
    function LastItem: TPasItem;

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

    function Text(const NameValueSepapator, ItemSeparator: string): string;
  end;

(* Item with members.
*)
  TPasScope = class(TPasItem)
  protected
  //renamed from TPasCio.FAncestors, TPasUnit.FUsesUnits
    FHeritage: TStringVector;
    procedure Serialize(const ADestination: TStream); override;
    procedure Deserialize(const ASource: TStream); override;

    { This searches for item (field, method or property) defined
      in ancestor of this cio. I.e. searches within the FirstAncestor,
      then within FirstAncestor.FirstAncestor, and so on.
      Returns nil if not found. }
    function FindItemInAncestors(const ItemName: string): TPasItem; virtual;

  public
  //remember visibility while parsing
    CurVisibility: TVisibility;
    constructor Create(AOwner: TPasScope; AKind: TTokenType;
      const AName: string); override;
    destructor Destroy; override;

    { If this class (or interface or object) contains a field, method or
      property with the name of ItemName, the corresponding item pointer is
      returned.

      If none is found, and the item has ancestors, the search continues
      in the ancestors.
      }
    function FindItem(const ItemName: string): TBaseItem; override;
  end;

//-------------- unused classes ------------------
  { @Name holds a collection of methods. It introduces no
    new methods compared to @link(TPasItems), but this may be
    implemented in a later stage. }
  TPasMethods = TPasItems;  //class(TPasItems)  end;

  { @Name holds a collection of properties. It introduces no
    new methods compared to @link(TPasItems), but this may be
    implemented in a later stage. }
  TPasProperties = TPasItems; //class(TPasItems)  end;

  { @abstract(Pascal constant.)

    Precise definition of "constant" for pasdoc purposes is
    "a name associated with a value".
    Optionally, constant type may also be specified in declararion.
    Well, Pascal constant always has some type, but pasdoc is too weak
    to determine the implicit type of a constant, i.e. to unserstand that
    constand @code(const A = 1) is of type Integer. }
  TPasConstant = TPasItem;  // class(TPasItem) end;

  { @abstract(Pascal global variable or field of CIO.)

    Precise definition is "a name with some type".
    And optionally with some initial value, for global variables.

    In the future we may introduce here some property like Type: TPasType. }
  TPasFieldVariable = TPasItem; //class(TPasItem) end;

  { @abstract(Pascal type) }
  TPasType = TPasScope;  //class(TPasItem)  end;

//--------------------------------------------------

  { @abstract(Enumerated type.) }
  TPasEnum = class(TPasScope) //(TPasType)
  protected
    procedure StoreValueTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
  public
    procedure RegisterTags(TagManager: TTagManager); override;
  end;

  { This represents:
    @orderedList(
      @item global function/procedure,
      @item method (function/procedure of a class/interface/object),
      @item pointer type to one of the above (in this case Name is the type name).
    ) }
  TPasMethod = class(TPasScope)
  protected
    FReturns: string;
    FRaises: TStringPairVector;
    FWhat: TMethodType;
    procedure Serialize(const ADestination: TStream); override;
    procedure Deserialize(const ASource: TStream); override;
    procedure StoreRaisesTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
    procedure StoreParamTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
    procedure StoreReturnsTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
  public
    constructor Create(AOwner: TPasScope; AKind: TTokenType;
      const AName: string); override;
    destructor Destroy; override;

    { In addition to inherited, this also registers @link(TTag)s
      that init @link(Params), @link(Returns) and @link(Raises)
      and remove according tags from description. }
    procedure RegisterTags(TagManager: TTagManager); override;

    { obsolete }
    property What: TMethodType read FWhat write FWhat;

    { Note that Params, Returns, Raises are already in the form processed by
      @link(TTagManager.Execute), i.e. with links resolved,
      html characters escaped etc. So @italic(don't) convert them (e.g. before
      writing to the final docs) once again (by some ExpandDescription or
      ConvertString or anything like that). }
    { }

    { Name of each item is the name of parameter (without any surrounding
      whitespace), Value of each item is users description for this item
      (in already-expanded form). }
    property Params: TPasItems read FItems;

    //Result could be added as a parameter
    property Returns: string read FReturns;

    { Name of each item is the name of exception class (without any surrounding
      whitespace), Value of each item is users description for this item
      (in already-expanded form). }
    property Raises: TStringPairVector read FRaises;

    { Are some optional properties (i.e. the ones that may be empty for
      TPasMethod after parsing unit and expanding tags --- currently this
      means @link(Params), @link(Returns) and @link(Raises)) specified ? }
    function HasMethodOptionalInfo: boolean;
  end;

  TPasProperty = class(TPasItem)
  protected
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
    { keeps default value specifier }
    property DefaultID: string read FDefaultID write FDefaultID;
    { true if Nodefault property }
  {$IFDEF new}
  //Delphi only, FPC doesn't compile these index directives :-(
    { true if the property is the default property }
    property Default: Boolean //read FDefault write FDefault;
      index SD_DEFAULT read GetAttribute write SetAttribute;
    property NoDefault: Boolean //read FNoDefault write FNoDefault;
      index SD_NODEFAULT read GetAttribute write SetAttribute;
  {$ELSE}
  {$ENDIF}
    { keeps Stored specifier }
    property StoredId: string read FStoredID write FStoredID;
  end;

  TClassDirective = (CT_NONE, CT_ABSTRACT, CT_SEALED);

  { @abstract(Extends @link(TPasScope) to store all items in
    a class / an object, e.g. fields.) }
  TPasCio = class(TPasType)
  protected
    //FClassDirective: TClassDirective;
    //FAncestors: TStringVector;
    FFields: TPasItems;
    FMethods: TPasMethods;
    FProperties: TPasProperties;
    FOutputFileName: string;
    function  GetCioType: TCIOType;
    function  GetClassDirective: TClassDirective;
  protected
    procedure StoreMemberTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
  public
    constructor Create(AOwner: TPasScope; AKind: TTokenType;
      const AName: string); override;
    destructor Destroy; override;

  //add item to members and to appropriate list
    procedure AddMember(item: TPasItem); override;

    { This searches for item (field, method or property) defined
      in ancestor of this cio. I.e. searches within the FirstAncestor,
      which in turn searches in his ancestors.
      Returns nil if not found. }
    function FindItemInAncestors(const ItemName: string): TPasItem; override;

    procedure Sort(const SortSettings: TSortSettings); override;

    procedure RegisterTags(TagManager: TTagManager); override;
  public

    { Name of the ancestor class / object.
      Objects[] of this vector are assigned in TDocGenerator.BuildLinks to
      TPasItem instances of ancestors (or nil if such ancestor is not found).

      Note that they are TPasItem, @italic(not necessarily) TPasCio.
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
      setting Ancestors when user didn't specify ancestor name
      at cio declaration. E.g. if this cio is a class,
      and user didn't specify ancestor name at class declaration,
      and this class name is not 'TObject' (in case pasdoc parses the RTL),
      the Ancestors[0] will be set to 'TObject'. }
    property Ancestors: TStringVector read FHeritage;
    //property Ancestors: TStringVector read FAncestors;

    {@name is used to indicate whether a class is sealed or abstract.}
    property ClassDirective: TClassDirective //read FClassDirective write FClassDirective;
      read GetClassDirective;

    { This returns Ancestors.Objects[0], i.e. instance of the first
      ancestor of this Cio (or nil if it couldn't be found),
      or nil if Ancestors.Count = 0. }
    function FirstAncestor: TPasCio;

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
    
    { list of all methods }
    property Methods: TPasMethods read FMethods;
    
    { list of properties }
    property Properties: TPasProperties read FProperties;
    
    { determines if this is a class, an interface or an object }
    property MyType: TCIOType read GetCioType;  // FMyType write FMyType;

    { name of documentation output file (if each class / object gets
      its own file, that's the case for HTML, but not for TeX) }
    property OutputFileName: string read FOutputFileName write FOutputFileName;

    { Is Visibility of items (Fields, Methods, Properties) important ? }
    function ShowVisibility: boolean;
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
  TPasUnit = class(TPasScope)
  protected
    FTypes: TPasItems;
    FVariables: TPasItems;
    FCIOs: TPasItems;
    FConstants: TPasItems;
    FFuncsProcs: TPasMethods;
    //FUsesUnits: TStringVector;
    FSourceFilename: string;
    FOutputFileName: string;
    FCacheDateTime: TDateTime;
    FSourceFileDateTime: TDateTime;

    { This searches for item (field, method or property) defined
      in ancestor of this cio. I.e. searches within the FirstAncestor,
      then within FirstAncestor.FirstAncestor, and so on.
      Returns nil if not found. }
    function FindItemInAncestors(const ItemName: string): TPasItem; override;
  public
    constructor Create(AOwner: TPasScope; AKind: TTokenType;
      const AName: string); override;
    destructor Destroy; override;
  //add item to members and to appropriate list
    procedure AddMember(item: TPasItem); override;

    function FindFieldMethodProperty(const S1, S2: string): TPasItem;

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
      obtain given unit's instance, as parsed by pasdoc.

      Members should become TStringPairs, which can hold a description?
    }
    property UsesUnits: TStringVector read FHeritage;
    //property UsesUnits: TStringVector read FUsesUnits;

    { list of types defined in this unit }
    property Types: TPasItems read FTypes;
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
    property IsUnit: boolean index KEY_UNIT read IsKey;
    property IsProgram: boolean index KEY_PROGRAM read IsKey;

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

  { @abstract(Holds a collection of units.) }
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

implementation

uses PasDoc_Utils;

type
//serialization of count fields, fixed to 4 bytes
  TCountField = LongInt;

function ComparePasItemsByName(PItem1, PItem2: Pointer): Integer;
var
  Item1: TPasItem absolute PItem1;
  Item2: TPasItem absolute PItem2;
begin
  Result := CompareText(TPasItem(PItem1).Name, TPasItem(PItem2).Name);
  if Result = 0 then
    Result := CompareText(Item1.QualifiedName, Item2.QualifiedName);
end;

function ComparePasMethods(PItem1, PItem2: Pointer): Integer;
var
  P1: TPasMethod absolute PItem1;
  P2: TPasMethod absolute PItem2;
begin
  { compare 'method type', order is
    constructor > destructor > function > procedure > operator
    then by visibility }
  Result := ord(P1.What) - ord(P2.What);
  if Result <> 0 then
    exit;
  Result := ord(P1.Visibility) - ord(P2.Visibility);

//What's this? Can result in endless comparison loop!
  //if Result = 0 then Result := 1;
end;

{ TBaseItem ------------------------------------------------------------------- }

constructor TBaseItem.Create;
begin
  inherited Create;
  FAuthors := TStringVector.Create;
  AutoLinkHereAllowed := true;
  FRawDescriptionInfo := TRawDescriptionInfo.Create;
{$IFDEF old}
  if ord(Kind) > 0 then begin
    if self.FMyOwner = nil then
      assert(self.Kind = KEY_UNIT, 'Non-Unit without owner: ' + TokenNames[kind]);
  end;
{$ELSE}
  //only PasItems must have an owner
{$ENDIF}
end;

destructor TBaseItem.Destroy;
begin
  FreeAndNil(FAuthors);
  FreeAndNil(FItems);
  FreeAndNil(FRawDescriptionInfo);
  inherited;
end;

function  TBaseItem.IsKey(AKey: TTokenType): boolean;
begin
  Result := FKind = AKey;
end;

procedure TBaseItem.AddMember(item: TPasItem);
begin
{ TODO : Only PasItems can have Members }
  //if FItems = nil then FItems := TPasItems.Create(True);
  Members.Add(item);
  item.FMyOwner := self as TPasScope;
end;

function TBaseItem.FindItem(const ItemName: string): TBaseItem;
begin
//find immediate member
  if assigned(Members) then
    Result := Members.FindName(ItemName)
  else
    Result := nil;
end;

function TBaseItem.FindName(const NameParts: TNameParts; index: integer = -1): TBaseItem;
begin
(* Find immediate member in members, parents and class ancestors.
  Index=-1 means FindName[0], continue search upwards.
  Index>0 means match name[i] in immediate members,
    recurse until all names matched.

FindItem should be used for specific parts of the NameParts.
*)
  if assigned(Members) and (Members.Count > 0) then begin
    if index < 0 then begin
    //unbounded search, try find first part in immediate members
      Result := FindName(NameParts, 0);
    end else begin
      //Result := Members.FindName(NameParts[index]);
      Result := Members.FindName(NameParts[index]);
        //this effectively is a FindItem! (rename?)
      if assigned(Result) then begin
      //anchored search for the remaining parts of the name
        if (index+1 < Length(NameParts)) then
          Result := Result.FindName(NameParts, index+1);
        exit; //no retry, after a part has been matched
      end;
    end;
  end else
    Result := nil;
//recover?
  if (Result = nil) and (index < 0) and assigned(FMyOwner) then
  //unbounded search up in containers
    Result := FMyOwner.FindName(NameParts, -1);
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
var
  s: string;
begin
  if Length(TagParameter)>1 then begin
    case TagParameter[2] of
      'D': begin
             if Copy(TagParameter,1,7) = '$Date: ' then begin
               LastMod := Trim(Copy(TagParameter, 7, Length(TagParameter)-7-1)) + ' UTC';
               ReplaceStr := '';
             end;
           end;
      'A': begin
             if Copy(TagParameter,1,9) = '$Author: ' then begin
               s := Trim(Copy(TagParameter, 9, Length(TagParameter)-9-1));
               if Length(s) > 0 then begin
                 if not Assigned(Authors) then
                   FAuthors := NewStringVector;
                 Authors.AddNotExisting(s);
                 ReplaceStr := '';
               end;
             end;
           end;
      else begin
      end;
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

procedure TBaseItem.SetFullLink(const Value: string);
begin
  FFullLink := Value;
  if Pos('.html', Value) < 1 then
    FFullLink := Value + '.html';
end;

function TBaseItem.QualifiedName: String;
begin
  if assigned(FMyOwner) then
    Result := FMyOwner.QualifiedName + QualIdSeparator + self.Name
  else
    Result := {QualIdSeparator +} Name; //flag absolute name path?
end;

procedure TBaseItem.Deserialize(const ASource: TStream);
var
  HaveItems: boolean;
begin
  inherited;
  Name := LoadStringFromStream(ASource);
//throw away token list
  FreeAndNil(FRawDescriptionInfo);
  FRawDescription := LoadStringFromStream(ASource);
  FNameStream := LoadStringFromStream(ASource);
  FNamePosition := LoadIntegerFromStream(ASource);
  ASource.Read(FKind, sizeof(FKind));
  ASource.Read(FAttributes, sizeof(FAttributes));
//allow for missing item list
  ASource.Read(HaveItems, 1);
  if HaveItems then begin
    FItems := TPasItems.Create(True); //owns items!
    FItems.Deserialize(ASource);
  end;

  { No need to serialize, because it's not generated by parser:
  DetailedDescription := LoadStringFromStream(ASource);
  FullLink := LoadStringFromStream(ASource);
  LastMod := LoadStringFromStream(ASource);
  Authors.LoadFromBinaryStream(ASource);
  FCreated := LoadStringFromStream(ASource);
  AutoLinkHereAllowed }
end;

procedure TBaseItem.Serialize(const ADestination: TStream);
var
  HaveItems: boolean;
begin
  inherited;
  SaveStringToStream(Name, ADestination);
  SaveStringToStream(RawDescription, ADestination);

  SaveStringToStream(FNameStream, ADestination);
  SaveIntegerToStream(FNamePosition, ADestination);
  ADestination.Write(FKind, sizeof(FKind));
  ADestination.Write(FAttributes, sizeof(FAttributes));
  HaveItems := assigned(FItems);
  ADestination.Write(HaveItems, 1);
  if HaveItems then
    FItems.Serialize(ADestination);

  { No need to serialize, because it's not generated by parser:
  SaveStringToStream(DetailedDescription, ADestination);
  SaveStringToStream(FullLink, ADestination);
  SaveStringToStream(LastMod, ADestination);
  Authors.SaveToBinaryStream(ADestination);
  SaveStringToStream(Created, ADestination);
  AutoLinkHereAllowed }
end;

function TBaseItem.GetRawDescription: string;
begin
//concatenate all description sections.
//but only if a token list has been created by the parser!
  if assigned(FRawDescriptionInfo) then
    Result := FRawDescriptionInfo.Text
  else
    Result := FRawDescription;
end;

procedure TBaseItem.AddRawDescription(t: TToken);
begin
  FRawDescriptionInfo.AddObject(t.CommentContent, t);
end;

procedure TBaseItem.AddRawDescription(const Value, AStream: string;
  APos: TTextStreamPos);
var
  c: TToken;
begin
//add qualified description
  if Assigned(FRawDescriptionInfo) then begin
    c := TToken.Create(TOK_COMMENT_EXT);
    c.CommentContent := Value;
    c.Mark := '#';  //mark external (linked) comment
    c.StreamName := AStream;
    c.BeginPosition := APos;
    FRawDescriptionInfo.AddObject(Value, c);
  end else
    WriteRawDescription(Value);
end;

procedure TBaseItem.WriteRawDescription(const Value: string);
begin
//append (more) descriptive text
{$IFDEF old}
  FRawDescriptionInfo.Text := Value;  //.Add(Value);
{$ELSE}
  if Assigned(FRawDescriptionInfo) then //add new entry, without token
    FRawDescriptionInfo.Add(Value)
  else if FRawDescription <> '' then  //append as new paragraph
    FRawDescription  := FRawDescription + LineEnding + LineEnding + Value
  else //add first description
    FRawDescription := Value;
{$ENDIF}
end;

function TBaseItem.BasePath: string;
begin
  Result := IncludeTrailingPathDelimiter(GetCurrentDir);
end;

{ TPasItem ------------------------------------------------------------------- }

constructor TPasItem.Create(AOwner: TPasScope; AKind: TTokenType;
  const AName: string);
begin
  FMyOwner := AOwner;
  FKind := AKind;
  FName := AName;
  FSeeAlso := TStringPairVector.Create(true);
  inherited Create();
  if assigned(AOwner) then
    AOwner.AddMember(self)
  else if ord(Kind) > 0 then begin
    assert(self is TPasUnit, 'Non-Unit without owner: ' + TokenNames[kind]);
  end;
end;

destructor TPasItem.Destroy;
begin
  FreeAndNil(FSeeAlso);
  inherited;
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
  Include(FAttributes, SD_Library_);
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
end;

function TPasItem.HasDescription: Boolean;
begin
  HasDescription := (AbstractDescription <> '') or (DetailedDescription <> '');
end;

procedure TPasItem.Sort(const SortSettings: TSortSettings);
begin
  { Nothing to sort in TPasItem }
end;

function TPasItem.GetMyUnit: TPasUnit;
begin
  if assigned(FMyOwner) then
  //units don't have an owner
    Result := FMyOwner.MyUnit
  else if Kind = KEY_UNIT then
    Result := self as TPasUnit
  else
    Result := nil;
end;

procedure TPasItem.SetMyUnit(U: TPasUnit);
begin
  if FMyOwner = nil then
    FMyOwner := U
  else
    assert(U = MyUnit, 'bad unit');
end;

function TPasItem.GetMyObject: TPasCio;
begin
//owner should always be assigned, except for units!
  if assigned(FMyOwner) and (FMyOwner.Kind in CioTypes) then
    Result := FMyOwner as TPasCio
  else
    Result := nil;
end;

procedure TPasItem.SetMyObject(o: TPasCio);
begin
  if FMyOwner = nil then
    FMyOwner := o
  else if o <> nil then
    assert(o = FMyOwner, 'bad owner');
end;

procedure TPasItem.Deserialize(const ASource: TStream);
begin
  inherited;
  ASource.Read(FVisibility, SizeOf(FVisibility));
  FullDeclaration := LoadStringFromStream(ASource);

  { No need to serialize, because it's not generated by parser:
  AbstractDescription := LoadStringFromStream(ASource);
  ASource.Read(FAbstractDescriptionWasAutomatic,
    SizeOf(FAbstractDescriptionWasAutomatic));
  SeeAlso }
end;

procedure TPasItem.Serialize(const ADestination: TStream);
begin
  inherited;
  ADestination.Write(FVisibility, SizeOf(Visibility));
  SaveStringToStream(FullDeclaration, ADestination);

  { No need to serialize, because it's not generated by parser:
  SaveStringToStream(AbstractDescription, ADestination);
  ADestination.Write(FAbstractDescriptionWasAutomatic,
    SizeOf(FAbstractDescriptionWasAutomatic));
  SeeAlso }
end;

function TPasItem.BasePath: string;
begin
  Result := MyUnit.BasePath;
end;

function TPasItem.GetAttribute(attr: TPasItemAttribute): boolean;
begin
  Result := attr in FAttributes;
end;

procedure TPasItem.SetAttribute(attr: TPasItemAttribute; OnOff: boolean);
begin
  if OnOff then
    Include(FAttributes, attr)
  else
    Exclude(FAttributes, attr);
end;

{ TPasScope }

constructor TPasScope.Create(AOwner: TPasScope; AKind: TTokenType;
      const AName: string);
begin
  inherited;
//always create member list
  if FItems = nil then
    FItems := TPasItems.Create(True);
    //destruction occurs in inherited destructor
  FHeritage := TStringVector.Create;
end;

destructor TPasScope.Destroy;
begin
  FreeAndNil(FHeritage);
  inherited;
end;

procedure TPasScope.Deserialize(const ASource: TStream);
begin
  inherited;
  Members.Deserialize(ASource);
  FHeritage.LoadFromBinaryStream(ASource);
end;

procedure TPasScope.Serialize(const ADestination: TStream);
begin
  inherited;
  Members.Serialize(ADestination);
  FHeritage.SaveToBinaryStream(ADestination);
end;

function TPasScope.FindItem(const ItemName: string): TBaseItem;
begin
(* FindItem should search in members first.
  If nothing was found, the ancestors are searched, if any exist.

  Note: Ancestor search differs for units (sequential) and CIOs (recursive).
*)
  Result := inherited FindItem(ItemName);
    //search only in members
  if Result = nil then
    Result := FindItemInAncestors(ItemName);
end;

function TPasScope.FindItemInAncestors(const ItemName: string): TPasItem;
begin
//override if scope has ancestors!
  Result := nil;
end;

{ TPasEnum ------------------------------------------------------------------- }

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

procedure TPasEnum.StoreValueTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
var
  ValueName: String;
  ValueDesc: String;
  Value: TPasItem;
begin
(* Special case: prefix enum member description with value specification.
*)
  ReplaceStr := '';
  ValueDesc := TagParameter;
  ValueName := ExtractFirstWord(ValueDesc);

  Value := Members.FindName(ValueName);
  if Assigned(Value) then begin
    if Value.RawDescription = '' then
      Value.FRawDescription := ValueDesc
    else begin
    {$IFDEF old}
      ThisTag.TagManager.DoMessage(1, pmtWarning,
        '@value tag specifies description for a value "%s" that already' +
        ' has one description.', [ValueName]);
    {$ELSE}
    //concatenate value and description, but only for the first time!
      Value.FRawDescription := ValueDesc + Value.RawDescription;
      FreeAndNil(Value.FRawdescriptionInfo);
    {$ENDIF}
    end;
  end else
    ThisTag.TagManager.DoMessage(1, pmtWarning,
      '@value tag specifies unknown value "%s"', [ValueName]);
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

function TBaseItems.FindName(const AName: string): TBaseItem;
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
  LCount: TCountField;
  i: Integer;
begin
  Clear;
  ASource.Read(LCount, SizeOf(LCount));
  for i := 0 to LCount - 1 do
    Add(TBaseItem(TSerializable.DeserializeObject(ASource)));
end;

procedure TBaseItems.Serialize(const ADestination: TStream);
var
  LCount: TCountField;
  i: Integer;
begin
  LCount := Count;
  ADestination.Write(LCount, SizeOf(LCount));
  { DONE : sizeof(integer) is compiler specific, not constant! }
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

function TPasItems.FindName(const AName: string): TPasItem;
begin
  Result := TPasItem(inherited FindName(AName));
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
      CIO_CLASS:
        Inc(c);
      CIO_INTERFACE:
        Inc(i);
      CIO_OBJECT:
        Inc(o);
    end;
end;

function TPasItems.GetPasItemAt(const AIndex: Integer): TPasItem;
begin
  Result := TPasItem(Items[AIndex]);
end;

function TPasItems.LastItem: TPasItem;
begin
  TObject(Result) := Last;
end;

//function TStringPairVector.Text(
function TPasItems.Text(const NameValueSepapator, ItemSeparator: string): string;
var
  i: Integer;
begin
  if Count > 0 then begin
    Result := PasItemAt[0].FullDeclaration;  //.Name + NameValueSepapator + PasItemAt[0].Value;
    for i := 1 to Count - 1 do
      Result := Result + ItemSeparator +
        //Items[i].Name + NameValueSepapator + Items[i].Value;
        PasItemAt[i].FullDeclaration;
  end;
end;

procedure TPasItems.SetPasItemAt(const AIndex: Integer; const Value:
  TPasItem);
begin
  Items[AIndex] := Value;
end;

procedure TPasItems.SortShallow;
begin
  if Count > 1 then //Bug in D7, comparing 1 item against nil!
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


{ TPasCio -------------------------------------------------------------------- }

constructor TPasCio.Create(AOwner: TPasScope; AKind: TTokenType;
      const AName: string);
const
  owns = False;
begin
  inherited;
  FFields := TPasItems.Create(owns);
  FMethods := TPasMethods.Create(owns);
  FProperties := TPasProperties.Create(owns);
end;

destructor TPasCio.Destroy;
begin
  //Ancestors.Free;
  Fields.Free;
  Methods.Free;
  Properties.Free;
  inherited;
end;

function  TPasCio.GetCioType: TCIOType;
begin
  case FKind of
  KEY_CLASS: Result := CIO_CLASS;
  KEY_DISPINTERFACE: Result := CIO_SPINTERFACE;
  KEY_INTERFACE: Result := CIO_INTERFACE;
  KEY_OBJECT: Result := CIO_OBJECT;
  KEY_RECORD: Result := CIO_RECORD; //could check for "packed"
  else
    Result := CIO_PACKEDRECORD;
    assert(False, 'unexpected CIO type');
  end;
end;

function  TPasCio.GetClassDirective: TClassDirective;
begin
  if SD_ABSTRACT in FAttributes then
    Result := CT_ABSTRACT
  else if SD_SEALED in FAttributes then
    Result := CT_SEALED
  else
    Result := CT_NONE;
end;

procedure TPasCio.AddMember(item: TPasItem);
begin
  inherited;
//check visibility
  if item.Visibility = viUnknown then
    item.Visibility := CurVisibility; //just created?
  if not (item.Visibility in ShowVisibilities) then
    exit; //don't add to the specialized (generators) lists
//add to specialized list
  case item.FKind of
  KEY_PROPERTY: Properties.Add(item);
  Key_Operator_,  //converted where?
  KEY_CONSTRUCTOR, KEY_DESTRUCTOR, KEY_PROCEDURE, KEY_FUNCTION:
    Methods.Add(item);
{ ancestors do not become members
  KEY_CLASS, KEY_DISPINTERFACE, KEY_INTERFACE:
    Ancestors.AddObject(item.Name, item);
}
  else //case
    Fields.Add(item);
  end;
end;


procedure TPasCio.Sort(const SortSettings: TSortSettings);
begin
  inherited;

  if Fields <> nil then begin
    if MyType in CIORecordTypes then begin
      if ssRecordFields in SortSettings then
        Fields.SortShallow;
    end else if ssNonRecordFields in SortSettings then
        Fields.SortShallow;
  end;

  if (Methods <> nil) and (ssMethods in SortSettings) then
    Methods.Sort( {$IFDEF FPC}@{$ENDIF} ComparePasMethods);

  if (Properties <> nil) and (ssProperties in SortSettings) then
    Properties.SortShallow;
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
  if Assigned(Member) then begin
  {$IFDEF old}
  { Only replace the description if one wasn't specified for it already }
    if Member.RawDescription = '' then
      Member.FRawDescription := MemberDesc
    else //concatenate instead?
      ThisTag.TagManager.DoMessage(1, pmtWarning,
        '@member tag specifies description for member "%s" that already' +
        ' has one description.', [MemberName]);
  {$ELSE}
    Member.WriteRawDescription(MemberDesc);
  {$ENDIF}
  end else
    ThisTag.TagManager.DoMessage(1, pmtWarning,
      '@member tag specifies unknown member "%s".', [MemberName]);
end;

function TPasCio.ShowVisibility: boolean;
begin
  Result := MyType in CIOClassTypes;
end;

function TPasCio.FirstAncestor: TPasCio;
begin
  if Ancestors.Count <> 0 then
    Result := Ancestors.Objects[0] as TPasCio
  else
    Result := nil;
end;

function TPasCio.FirstAncestorName: string;
begin
  if Ancestors.Count <> 0 then
    Result := Ancestors[0]
  else
    Result := '';
end;

function TPasCio.FindItemInAncestors(const ItemName: string): TPasItem;
begin
//ancestor also searches in it's ancestor(s) (auto recursion)
  Result := FirstAncestor;
  if Result <> nil then
    Result := Result.FindItem(ItemName) as TPasItem;
end;

{ TPasUnit ------------------------------------------------------------------- }

constructor TPasUnit.Create(AOwner: TPasScope; AKind: TTokenType;
  const AName: string);
const
  owns = False;
begin
  inherited;
  FTypes := TPasItems.Create(owns);
  FVariables := TPasItems.Create(owns);
  FCIOs := TPasItems.Create(owns);
  FConstants := TPasItems.Create(owns);
  FFuncsProcs := TPasMethods.Create(owns);
  //FUsesUnits := TStringVector.Create;
end;

destructor TPasUnit.Destroy;
begin
  FCIOs.Free;
  FConstants.Free;
  FFuncsProcs.Free;
  FTypes.Free;
  //FUsesUnits.Free;
  FVariables.Free;
  inherited;
end;

function TPasUnit.FindItemInAncestors(const ItemName: string): TPasItem;
var
  i: integer;
  //obj: TObject absolute Result;
begin
  for i := 0 to UsesUnits.Count - 1 do begin
    Result := UsesUnits.Objects[i] as TPasScope;
    if Result <> nil then begin
      Result := Result.FindItem(ItemName) as TPasItem;
      if Result <> nil then
        exit;
    end;
  end;
//if nothing searched and found
  Result := nil;
end;

procedure TPasUnit.AddMember(item: TPasItem);
begin
  inherited;
  case item.Kind of
  KEY_CONST:  FConstants.Add(item);
  KEY_TYPE:   FTypes.Add(item);
  KEY_VAR:    FVariables.Add(item);
  KEY_UNIT:   UsesUnits.AddObject(item.Name, item);
  else
    if item.Kind in CioTypes then
      FCIOs.Add(item)
    else
      FFuncsProcs.Add(item);
  end;
end;

function TPasUnit.FindFieldMethodProperty(const S1, S2: string): TPasItem;
var
  po: TPasItem;
begin
  Result := nil;
  if CIOs = nil then Exit;

  po := CIOs.FindName(S1);
  if Assigned(po) then
    Result := po.FindItem(S2) as TPasItem;
end;

function TPasUnit.FileNewerThanCache(const FileName: string): boolean;
begin
  Result := WasDeserialized and FileExists(FileName) and
    (CacheDateTime < FileDateToDateTime(FileAge(FileName)));
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

function TPasUnit.BasePath: string;
begin
  Result := ExtractFilePath(ExpandFileName(SourceFileName));
end;

{ TPasUnits ------------------------------------------------------------------ }

function TPasUnits.ExistsUnit(const AUnit: TPasUnit): Boolean;
begin
  Result := FindName(AUnit.Name) <> nil;
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

constructor TPasMethod.Create(AOwner: TPasScope; AKind: TTokenType;
  const AName: string);
begin
  inherited;
  //FParams := TStringPairVector.Create(true);
  FRaises := TStringPairVector.Create(true);
//init what
  case AKind of
  KEY_CONSTRUCTOR: FWhat := METHOD_CONSTRUCTOR;
  KEY_DESTRUCTOR: FWhat := METHOD_DESTRUCTOR;
  KEY_FUNCTION:   FWhat := METHOD_FUNCTION;
  KEY_PROCEDURE:  FWhat := METHOD_PROCEDURE;
  else            FWhat := METHOD_OPERATOR;
  end;
end;

destructor TPasMethod.Destroy;
begin
  FRaises.Free;
  inherited Destroy;
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

procedure TPasMethod.StoreRaisesTag(
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

procedure TPasMethod.StoreParamTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
var
  //name: string;
  Pair: TStringPair;
  param: TPasItem;
begin
  Pair := TStringPair.CreateExtractFirstWord(TagParameter);

  if Pair.Name = '' then begin
    FreeAndNil(Pair);
    ThisTag.TagManager.DoMessage(2, pmtWarning,
      '@param tag doesn''t specify parameter name, skipped', []);
  end else begin
    TBaseItem(param) := FindItem(Pair.Name);
    if param = nil then
      param := TPasItem.Create(self, KEY_VAR, Pair.Name);
    param.DetailedDescription := Pair.Value;
  end;

  ReplaceStr := '';
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

function TPasMethod.HasMethodOptionalInfo: boolean;
begin
  Result :=
    (Returns <> '')
    //or (not ObjectVectorIsNilOrEmpty(Params))
    or (not ObjectVectorIsNilOrEmpty(Raises));
end;

procedure TPasMethod.Deserialize(const ASource: TStream);
begin
  inherited;
  ASource.Read(FWhat, SizeOf(FWhat));

  { No need to serialize, because it's not generated by parser:
  Params.LoadFromBinaryStream(ASource);
  FReturns := LoadStringFromStream(ASource);
  FRaises.LoadFromBinaryStream(ASource); }
end;

procedure TPasMethod.Serialize(const ADestination: TStream);
begin
  inherited;
  ADestination.Write(FWhat, SizeOf(FWhat));
  
  { No need to serialize, because it's not generated by parser:
  Params.SaveToBinaryStream(ADestination);
  SaveStringToStream(FReturns, ADestination);
  FRaises.SaveToBinaryStream(ADestination); }  
end;

procedure TPasMethod.RegisterTags(TagManager: TTagManager);
begin
  inherited;
  TTopLevelTag.Create(TagManager, 'raises',
    nil, {$IFDEF FPC}@{$ENDIF} StoreRaisesTag,
    [toParameterRequired, toRecursiveTags, toAllowOtherTagsInsideByDefault, 
     toAllowNormalTextInside, toFirstWordVerbatim]);
  TTopLevelTag.Create(TagManager, 'param', 
    nil, {$IFDEF FPC}@{$ENDIF} StoreParamTag,
    [toParameterRequired, toRecursiveTags, toAllowOtherTagsInsideByDefault, 
     toAllowNormalTextInside, toFirstWordVerbatim]);
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
    Result := FAnchors.FindName(ItemName);
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

{ global things ------------------------------------------------------------ }

function MethodTypeToString(const MethodType: TMethodType): string;
const
  { Maps @link(TMethodType) value to @link(TKeyWord) value.
    When given TMethodType value doesn't correspond to any keyword,
    it maps it to KEY_INVALIDKEYWORD. }
  MethodTypeToKeyWord: array[TMethodType] of TTokenType =
  ( KEY_CONSTRUCTOR,
    KEY_DESTRUCTOR,
    KEY_FUNCTION,
    KEY_PROCEDURE,
    KEY_INVALIDKEYWORD
  );
begin
  if MethodType = METHOD_OPERATOR then
    Result := DirectiveNames[SD_OPERATOR]
  else
    Result := TokenNames[MethodTypeToKeyWord[MethodType]];
  Result := LowerCase(Result);
end;

function VisToStr(const Vis: TVisibility): string;
begin
  result := StringReplace(VisibilityStr[Vis], ' ', '', [rfReplaceAll]);
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

{ TRawDescriptionInfo }

destructor TRawDescriptionInfo.Destroy;
var
  i: integer;
begin
//destroy owned tokens
  for i := 0 to Count - 1 do begin
    Objects[i].Free;
  end;
  inherited;
end;

initialization
  TSerializable.Register(TPasItem);
{$IFDEF old}
  TSerializable.Register(TPasConstant);
  TSerializable.Register(TPasFieldVariable);
  TSerializable.Register(TPasType);
{$ELSE}
  TSerializable.Register(TPasScope);
{$ENDIF}
  TSerializable.Register(TPasEnum);
  TSerializable.Register(TPasMethod);
  TSerializable.Register(TPasProperty);
  TSerializable.Register(TPasCio);
  TSerializable.Register(TPasUnit);
end.
