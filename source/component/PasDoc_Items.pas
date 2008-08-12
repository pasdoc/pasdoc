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

{-$DEFINE delegates} //not yet
{-$DEFINE RawBase}  //all items have a non-nil RawDescriptionInfo?
{-$DEFINE item}

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
  PasDoc_Languages,
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
    CIO_RECORD  //, CIO_PACKEDRECORD
  );

  TCIONames = array[TCIOType] of string;
const
  CIO_NAMES: TCIONames = (
    'class',
    'dispinterface',
    'interface',
    'object',
    'record'  //, 'packed record'
  );
//for ShowVisisbility
  CioClassTypes = [CIO_CLASS, CIO_SPINTERFACE, CIO_INTERFACE, CIO_OBJECT];
  CIORecordTypes = [CIO_RECORD //, CIO_PACKEDRECORD
  ];
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

(* Organization (=intended layout?) of descriptive elements.
  Should match the expectations of the generators!
*)
  eDescriptionKind = (
  //simple entries, without any objects in Data
    dkText, //< simple text, in Value(?)
    dkNameDesc, //< Name and Value=Description
  // StringList, without objects (Authors)
    dkStrings,
  //list of TPasItem's (=TPasItems), which are NOT description items!
  //the list is owned, but not the members - which are owned by Members[]!
    dkPasItems,
  //lists, tables(?) Data is description list
    dkItemList
  );

(* Become new class? Keep TStringPair ancestor for now, for compatibility.
  Considerations for lists:

  The lists themselves must be unique, either by ID or name.

  Lists of TPasItems can be created by the parser (unit.Variables)
  or by the generator (unit.UsedUnits).

  Parser generated lists (TPasItems) must be serialized, and consequently
  must be deserialized into their proper place. This may change, when
  sub-lists of Members are created on demand (Events...).
  A descriptive entry can hold that list, but can not retrieve items
  by ID. No dupecheck is required, the lists may be sorted,
  access is assumed sequential (by index).

  Delegates for TPasItems are required for e.g. UsedUnits, where the
  TPasUnit objects are added later, based on the item.Name.
*)
  TDescriptionItem = class(TStringPair)
{$IFDEF delegates}
  protected
  //Load all text from stream.
    procedure LoadFromBinaryStream(Stream: TStream);
    { This saves our contents in a format readable by
      @link(LoadFromBinaryStream). }
    procedure SaveToBinaryStream(Stream: TStream);
{$ELSE}
{$ENDIF}
  public
  //possible section title, if unnamed
    ID: TTranslationID;
  {$IFDEF old}
  //entry has an name, different from ID
    HasName: boolean;
  {$ELSE}
  {$ENDIF}
  //case kind of???
    kind: eDescriptionKind;
  //get list count
    function  GetCount: integer;
  //make sure that all fields are initialized
    constructor Create(tid: TTranslationID; AKind: eDescriptionKind;
      const AName: string = ''; const AValue: string = '');
    destructor Destroy; override;
  //add an item
    function Add(AItem: TDescriptionItem): integer;
  //add an descriptor
    function  AddNew(tid: TTranslationID; AKind: eDescriptionKind;
      const AName: string = ''; const AValue: string = ''): TDescriptionItem;
  //create a pair of first word and remainder. Returns Nil if no first word is found.
    function AddExtractFirstWord(tid: TTranslationID; const s: string): TDescriptionItem;
  //add an string (as Description)
    function AddString(tid: TTranslationID; const s: string): TDescriptionItem;
  //add string to list
    function AddToStrings(tid: TTranslationID; const s: string): integer;

  //create a string list
    function AddStrings(tid: TTranslationID; lst: TStrings = nil): TDescriptionItem;

  //get description item from list, or Nil
    function  ItemAt(index: integer): TDescriptionItem;
  //get descriptive element by ID, or Nil
    function  FindID(tid: TTranslationID): TDescriptionItem;
  //get PasItem from list - really?
    function  PasItemAt(index: integer): TPasItem;
  //find item by name
    function  Find(const AName: string): TDescriptionItem;
  //get Data as TPasItem, or Nil.
    function  PasItem: TPasItem;
  //get Data as object, or Nil
    function  GetObject: TObject;

    { Returns all items Names and Values glued together.
      For every item, string Name + NameValueSeparator + Value is
      constructed. Then all such strings for every item are
      concatenated with ItemSeparator. }
    function Text(const NameValueSeparator: string = ' '; const ItemSeparator: string = ' '): string;

    property Count: integer read GetCount;
    property Items[index: integer]: TDescriptionItem read ItemAt; default;
    property Description: string read Value;
  end;
  TStringPair = TDescriptionItem;
  TStringPairVector = TDescriptionItem;

  { This is a basic item class, that is linkable,
    and has some @link(RawDescription). }
  TBaseItem = class(TSerializable)
  private
    FName: string;
    FDetailedDescription: string; //<to become Item[trDescription]?
    FFullLink: string;
    FAutoLinkHereAllowed: boolean;
  //all descriptive items.
  {$IFDEF items}
    FItems: TDescriptionItem;
    property FOptItems: TDescriptionItem read FItems;
    property NeedItems: TDescriptionItem read FItems;
  {$ELSE}
  // Create on demand?
    FOptItems: TDescriptionItem;
  protected
    function NeedItems: TDescriptionItem;
  {$ENDIF}

  private
  //these apply only to TPasItem?
    FRawDescriptionInfo: TRawDescriptionInfo;
    FRawDescription: string;
    function  GetRawDescription: string;
    function  GetFirstDescription: TToken;
  //allow doc generator to add more descriptions
    procedure WriteRawDescription(const Value: string);

    procedure SetFullLink(const Value: string);

  //FPC requires INTEGER for indexed properties, where Delphi also allows for enums.
    function GetItemFPC(intID: integer): TDescriptionItem;

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

    { Search for ItemName in external file (Introduction), or in a TPasItem.

      This searches for item with ItemName @italic(inside this item).
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
    function FindName(const NameParts: TNameParts; index: integer = -1): TPasItem; virtual;

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
    //get first description token. Nil if no description found.
    property FirstDescription: TToken read GetFirstDescription;

    { Full info about @link(RawDescription) of this item,
      including it's filename and position.

      This is intended to be initialized by parser, and used by an editor.
      Detailed information is not preserved during serialization.
    }
    procedure AddRawDescription(const Value: string; const AStream: string;
      APos: TTextStreamPos); overload;
    //for use by parser
    procedure AddRawDescription(t: TToken); overload;

    function GetItem(id: TTranslationID): TDescriptionItem;
    property Items: TDescriptionItem
      read {$IFDEF items} FItems {$ELSE} FOptItems {$ENDIF} ;

    { a full link that should be enough to link this item from anywhere else }
    property FullLink: string read FFullLink write SetFullLink;

    { name of the item }
    property Name: string read FName write FName;

    { Returns the qualified name of the item.
      This is intended to return a concise and not ambigous name.
      E.g. in case of TPasItem it is overriden to return Name qualified
      by class name and unit name.

      In this class this simply returns Name. }
    function QualifiedName: String; virtual;

    { Is auto-link mechanism allowed to create link to this item ?
      This may be set to @false by @@noAutoLinkHere tag in item's description. }
    property AutoLinkHereAllowed: boolean
      read FAutoLinkHereAllowed write FAutoLinkHereAllowed default true;

    { The full (absolute) path used to resolve filenames in this item's descriptions.
      Must always end with PathDelim.
      In this class, this simply returns GetCurrentDir (with PathDelim added if needed). }
    function BasePath: string; virtual;

  { Get any description. Nil if not existing
  }
    property Description[tid: TTranslationID]: TDescriptionItem read GetItem;

    { list of strings, each representing one author of this item }
    //property Authors: TStringVector read FAuthors write SetAuthors;
    property Authors: TDescriptionItem index ord(trAuthors) read GetItemFPC;

    { Contains '' or string with date of creation.
      This string is already in the form suitable for final output
      format (i.e. already processed by TDocGenerator.ConvertString). }
    //property Created: string read FCreated;
    property Created: TDescriptionItem index ord(trCreated) read GetItemFPC;

    { Contains '' or string with date of last modification.
      This string is already in the form suitable for final output
      format (i.e. already processed by TDocGenerator.ConvertString). }
    //property LastMod: string read FLastMod write FLastMod;
    property LastMod: TDescriptionItem index ord(trLastModified) read GetItemFPC;

    property SeeAlso: TDescriptionItem index ord(trSeeAlso) read GetItemFPC;
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
  { TODO : convert into items }
    FAbstractDescription: string; //trAbstract?
    FAbstractDescriptionWasAutomatic: boolean;
    FFullDeclaration: string; //trDeclaration?
    FVisibility: TVisibility;

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

  private
  // The declarative token, "unit", "class", "type" etc.
    FKind: TTokenType;
  // All attributes, modifiers etc.
    FAttributes: TPasItemAttributes;
  // position of identifier in the declaration.
    FNamePosition: TTextStreamPos;
    FNameStream: string;
  { Owner shall be the item, of which this item is a member.
    Usage: construct the fully qualified name...
  }
    FMyOwner: TPasScope;
  //obsolete?
    function  GetMyObject: TPasCio;
    procedure SetMyObject(o: TPasCio);
    function  GetMyUnit: TPasUnit;
    procedure SetMyUnit(U: TPasUnit);
  public
    function  IsKey(AKey: TTokenType): boolean;
    function  GetAttributeFP(attr: integer): boolean;
    procedure SetAttributeFP(attr: integer; OnOff: boolean);
    function  GetAttribute(attr: TPasItemAttribute): boolean;
    procedure SetAttribute(attr: TPasItemAttribute; OnOff: boolean);

    property Kind: TTokenType read FKind;
    property Attributes: TPasItemAttributes read FAttributes write FAttributes;
    property NamePosition: TTextStreamPos read FNamePosition write FNamePosition;
    property NameStream: string read FNameStream write FNameStream;
    property MyOwner: TPasScope read FMyOwner;  // write FMyOwner;

  public
    constructor Create(AOwner: TPasScope; AKind: TTokenType;
      const AName: string); reintroduce; virtual;

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
    property MyUnit: TPasUnit read GetMyUnit; // write SetMyUnit;

    { if this item is part of an object or class, the corresponding
      info object is stored here, nil otherwise }
    property MyObject: TPasCio read GetMyObject;  // write SetMyObject;

    property HasAttribute[attr: TPasItemAttribute]: boolean
      read GetAttribute write SetAttribute;

    { is this item deprecated? }
    property IsDeprecated: boolean index ord(SD_DEPRECATED)
      read GetAttributeFP write SetAttributeFP;

    { Is this item platform specific?
      This is decided by "platform" hint directive after an item. }
    property IsPlatformSpecific: boolean index ord(SD_PLATFORM)
      read GetAttributeFP write SetAttributeFP;

    { Is this item specific to a library ?
      This is decided by "library" hint directive after an item. }
    property IsLibrarySpecific: boolean index ord(SD_Library_)
      read GetAttributeFP write SetAttributeFP;

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

    { Returns the qualified name of the item.
      This is intended to return a concise and not ambigous name.
      E.g. in case of TPasItem it is overriden to return Name qualified
      by class name and unit name.

      In this class this simply returns Name. }
    function QualifiedName: String; override;

    { The full (absolute) path used to resolve filenames in this item's descriptions.
      Must always end with PathDelim.
      In this class returns MyUnit.BasePath. }
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
      Name's case is not regarded.

      Bad name, should read FindItem. FindName is used to retrieve TPasItems.
      Corrected bad cast, from TPasItem into TBaseItem.
    }
    function FindName(const AName: string): TBaseItem;

    { Inserts all items of C into this collection.
      Disposes C and sets it to nil. }
    procedure InsertItems(const c: TBaseItems);

    { During Add, AObject is associated with AObject.Name using hash table,
      so remember to set AObject.Name @italic(before) calling Add(AObject). }
    procedure Add(const AObject: TBaseItem);

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
    FMembers: TPasItems;
  {$IFnDEF delegates}
  //renamed from TPasCio.FAncestors, TPasUnit.FUsesUnits
    FHeritage: TStringVector;
  {$ELSE}
  {$ENDIF}
    procedure Serialize(const ADestination: TStream); override;
    procedure Deserialize(const ASource: TStream); override;

    { This searches for ItemName in an ancestor.
      For units it searches in used units.
      For CIOs it searches in the ancestors, recursively.
      Returns nil if not found. }
    function FindItemInAncestors(const ItemName: string): TPasItem; virtual;

  public
  //remember visibility while parsing
    CurVisibility: TVisibility;
    constructor Create(AOwner: TPasScope; AKind: TTokenType;
      const AName: string); override;
    destructor Destroy; override;

  //add member, override for specialized lists
    procedure AddMember(item: TPasItem); virtual;

    { This does all it can to resolve link specified by NameParts.

      While searching this tries to mimic ObjectPascal identifier scope
      as much as it can. It seaches within this item,
      but also within class enclosing this item,
      within ancestors of this class,
      within unit enclosing this item, then within units used by unit
      of this item. }
    function FindName(const NameParts: TNameParts; index: integer = -1): TPasItem; override;

    { If this class (or interface or object) contains a field, method or
      property with the name of ItemName, the corresponding item pointer is
      returned.

      If none is found, and the item has ancestors, the search continues
      in the ancestors.
      }
    function FindItem(const ItemName: string): TBaseItem; override;

    property Members: TPasItems read FMembers;
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
    FReturns, //string;
    FRaises,  //: TStringPairVector;
    FParams: TDescriptionItem;
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
    //property Params: TPasItems read FItems;
    property Params: TDescriptionItem read FParams;

    //Result could be added as a parameter
    //property Returns: string read FReturns;
    property Returns: TDescriptionItem read FReturns;

    { Name of each item is the name of exception class (without any surrounding
      whitespace), Value of each item is users description for this item
      (in already-expanded form). }
    //property Raises: TStringPairVector read FRaises;
    property Raises: TDescriptionItem read FRaises;

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
    { true if the property is the default property }
    property Default: Boolean //read FDefault write FDefault;
      index ord(SD_DEFAULT) read GetAttributeFP write SetAttributeFP;
    property NoDefault: Boolean //read FNoDefault write FNoDefault;
      index ord(SD_NODEFAULT) read GetAttributeFP write SetAttributeFP;
    { keeps Stored specifier }
    property StoredId: string read FStoredID write FStoredID;
  end;

  TClassDirective = (CT_NONE, CT_ABSTRACT, CT_SEALED);

  { @abstract(Extends @link(TPasScope) to store all items in
    a class / an object, e.g. fields.) }
  TPasCio = class(TPasType)
  protected
    //FClassDirective: TClassDirective;
  {$IFDEF old}
    //FAncestors: TStringVector;
  {$ELSE}
    FAncestors: TDescriptionItem;
  {$ENDIF}
    FFields: TPasItems;
    FMethods: TPasMethods;
    FProperties: TPasProperties;
    FOutputFileName: string;
    function  GetCioType: TCIOType;
    function  GetClassDirective: TClassDirective;
  protected
  {$IFDEF new}
    procedure HandleClassnameTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
  {$ELSE}
  {$ENDIF}
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
  {$IFDEF delegates}
    property Ancestors: TDescriptionItem read FAncestors;
  {$ELSE}
    //property Ancestors: TStringVector read FAncestors;
    property Ancestors: TStringVector read FHeritage;
  {$ENDIF}

    {@name is used to indicate whether a class is sealed or abstract.}
    property ClassDirective: TClassDirective //read FClassDirective write FClassDirective;
      read GetClassDirective;

    { This returns Ancestors.Objects[0], i.e. instance of the first
      ancestor of this Cio (or nil if it couldn't be found),
      or nil if Ancestors.Count = 0. }
  {$IFDEF delegates}
    function FirstAncestor: TDescriptionItem;
  {$ELSE}
    function FirstAncestor: TPasCio;
  {$ENDIF}

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

  { extends @link(TPasScope) to store anything about a unit, its constants,
    types etc.; also provides methods for parsing a complete unit.

    Note: Remember to always set @link(CacheDateTime) after
    deserializing this unit. }
  TPasUnit = class(TPasScope)
  protected
  {$IFDEF delegates}
    FTypes,
    FVariables,
    FCIOs,
    FConstants,
    FFuncsProcs,
    FUsesUnits: TDescriptionItem;
  {$ELSE}
    FTypes: TPasItems;
    FVariables: TPasItems;
    FCIOs: TPasItems;
    FConstants: TPasItems;
    FFuncsProcs: TPasMethods;
    FUsesUnits: TStringVector;
  {$ENDIF}
    FSourceFilename: string;
    FOutputFileName: string;
    FCacheDateTime: TDateTime;
    FSourceFileDateTime: TDateTime;

    { This searches for item in all used units.
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
  {$IFnDEF delegates}
    { list of classes, interfaces, objects, and records defined in this unit }
    property CIOs: TPasItems read FCIOs;
    { list of constants defined in this unit }
    property Constants: TPasItems read FConstants;
    { list of functions and procedures defined in this unit }
    property FuncsProcs: TPasMethods read FFuncsProcs;
    { list of types defined in this unit }
    property Types: TPasItems read FTypes;
    { list of variables defined in this unit }
    property Variables: TPasItems read FVariables;
  {$ELSE}
    { list of classes, interfaces, objects, and records defined in this unit }
    property CIOs: TDescriptionItem read FCIOs;
    { list of constants defined in this unit }
    property Constants: TDescriptionItem read FConstants;
    { list of functions and procedures defined in this unit }
    property FuncsProcs: TDescriptionItem read FFuncsProcs;
    { list of types defined in this unit }
    property Types: TDescriptionItem read FTypes;
    { list of variables defined in this unit }
    property Variables: TDescriptionItem read FVariables;
  {$ENDIF}

    { The names of all units mentioned in a uses clause in the interface
      section of this unit.

      This is never nil.

      After @link(TDocGenerator.BuildLinks), for every i:
      UsesUnits.Objects[i] will point to TPasUnit object with
      Name = UsesUnits[i] (or nil, if pasdoc's didn't parse such unit).
      In other words, you will be able to use UsesUnits.Objects[i] to
      obtain given unit's instance, as parsed by pasdoc.

      Members should become TStringPairs, which can hold a description?
      Like with parameters etc., which (currently) are not provided
      by the parser.
    }
  {$IFDEF delegates}
    property UsesUnits: TDescriptionItem read FUsesUnits;
  {$ELSE}
    property UsesUnits: TStringVector read FHeritage;
  {$ENDIF}

    { name of documentation output file
      THIS SHOULD NOT BE HERE!
      Right, it should be in TPasScope, if ever - DoDi.
    }
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

function IsEmpty(item: TDescriptionItem): boolean; overload;

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
  AutoLinkHereAllowed := true;
//really pre-create?
  {$IFDEF RawBase}
  FRawDescriptionInfo := TRawDescriptionInfo.Create;
  {$ELSE}
    //leave Nil
  {$ENDIF}
{$IFDEF items}
  FItems := TDescriptionItem.Create(trNoTrans, dkItemList);
{$ELSE}
{$ENDIF}
end;

destructor TBaseItem.Destroy;
begin
{$IFDEF items}
  FreeAndNil(FItems);
{$ELSE}
  FreeAndNil(FOptItems);
{$ENDIF}
  FreeAndNil(FRawDescriptionInfo);
  inherited;
end;

{$IFDEF items}
{$ELSE}
function TBaseItem.NeedItems: TDescriptionItem;
begin
  if FOptItems = nil then
    FOptItems := TDescriptionItem.Create(trNoTrans, dkItemList);
  Result := FOptItems;
end;
{$ENDIF}

function TBaseItem.GetItemFPC(intID: integer): TDescriptionItem;
var
  id: TTranslationID absolute intID;
begin
  Result := FOptItems;
  if Result <> nil then
    Result := Result.FindID(id);
end;

function TBaseItem.GetItem(id: TTranslationID): TDescriptionItem;
begin
  Result := FOptItems;
  if Result <> nil then
    Result := Result.FindID(id);
end;

function TBaseItem.FindItem(const ItemName: string): TBaseItem;
begin
//override in TExternalItem and TPasItem descendants (unit, cio, enum)
  Result := nil;
end;

function TBaseItem.FindName(const NameParts: TNameParts; index: integer = -1): TPasItem;
begin
//override in TPasScope.
  Result := nil;
end;

procedure TBaseItem.StoreAuthorTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  if TagParameter = '' then exit;
  NeedItems.AddToStrings(trAuthors, TagParameter);
  ReplaceStr := '';
end;

procedure TBaseItem.StoreCreatedTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  if TagParameter = '' then exit;
  NeedItems.AddString(trCreated, TagParameter);
  ReplaceStr := '';
end;

procedure TBaseItem.StoreLastModTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  if TagParameter = '' then exit;
  NeedItems.AddString(trLastModified, TagParameter);
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
    'D':
      if Copy(TagParameter,1,7) = '$Date: ' then begin
        NeedItems.AddString(trLastModified, Trim(Copy(TagParameter, 7, Length(TagParameter)-7-1)) + ' UTC');
        ReplaceStr := '';
      end;
    'A':
      if Copy(TagParameter,1,9) = '$Author: ' then begin
        s := Trim(Copy(TagParameter, 9, Length(TagParameter)-9-1));
        if s <> '' then begin
          NeedItems.AddToStrings(trAuthors, s);
          ReplaceStr := '';
        end;
      end;
    //else //ignore
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
//handled in PreHandle...
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

procedure TBaseItem.SetFullLink(const Value: string);
begin
  FFullLink := Value;
  if Pos('.html', Value) < 1 then
    FFullLink := Value + '.html';
end;

function TBaseItem.QualifiedName: String;
begin
  Result := Name;
end;

procedure TBaseItem.Deserialize(const ASource: TStream);
//var HaveItems: boolean;
begin
  inherited;
  Name := LoadStringFromStream(ASource);
//throw away token list
  FreeAndNil(FRawDescriptionInfo);
  FRawDescription := LoadStringFromStream(ASource);

  { No need to serialize, because it's not generated by parser:
  FItems
  DetailedDescription := LoadStringFromStream(ASource);
  FullLink := LoadStringFromStream(ASource);
  LastMod := LoadStringFromStream(ASource);
  Authors.LoadFromBinaryStream(ASource);
  FCreated := LoadStringFromStream(ASource);
  AutoLinkHereAllowed }
end;

procedure TBaseItem.Serialize(const ADestination: TStream);
//var HaveItems: boolean;
begin
  inherited;
  SaveStringToStream(Name, ADestination);
  SaveStringToStream(RawDescription, ADestination);

  { No need to serialize, because it's not generated by parser:
  FItems
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

function TBaseItem.GetFirstDescription: TToken;
begin
  if assigned(FRawDescriptionInfo) and (FRawDescriptionInfo.Count > 0) then
    Result := FRawDescriptionInfo.Objects[0] as TToken
  else
    Result := nil;
end;

procedure TBaseItem.AddRawDescription(t: TToken);
begin
//called by parser, requires a list of raw descriptions!
  if FRawDescriptionInfo = nil then
    FRawDescriptionInfo := TRawDescriptionInfo.Create;
  FRawDescriptionInfo.AddObject(t.CommentContent, t);
end;

procedure TBaseItem.AddRawDescription(const Value, AStream: string;
  APos: TTextStreamPos);
var
  c: TToken;
begin
//add qualified description, from external file
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
  if Assigned(FRawDescriptionInfo) then //add new entry, without token
    FRawDescriptionInfo.Add(Value)
  else if FRawDescription <> '' then  //append as new paragraph
    FRawDescription  := FRawDescription + LineEnding + LineEnding + Value
  else //add first description
    FRawDescription := Value;
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
  inherited Create();
//not always used, but required even for parser
  if FRawDescriptionInfo = nil then
    FRawDescriptionInfo := TRawDescriptionInfo.Create;
//all but TPasUnit's must have an valid owner
  if assigned(AOwner) then
    AOwner.AddMember(self)
  else if ord(Kind) > 0 then begin
    assert(self is TPasUnit, 'Non-Unit without owner: ' + TokenNames[kind]);
  end;
end;

function TPasItem.IsKey(AKey: TTokenType): boolean;
begin
  Result := FKind = AKey;
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
begin
  if NeedItems.AddExtractFirstWord(trSeeAlso, TagParameter) = nil then
    ThisTag.TagManager.DoMessage(2, pmtWarning,
      '@seealso tag doesn''t specify any name to link to, skipped', []);

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

  FNameStream := LoadStringFromStream(ASource);
  FNamePosition := LoadIntegerFromStream(ASource);

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

  SaveStringToStream(FNameStream, ADestination);
  SaveIntegerToStream(FNamePosition, ADestination);

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

function TPasItem.QualifiedName: String;
begin
//FMyOwner should always be assigned, except for units.
  if assigned(FMyOwner) then
    Result := FMyOwner.QualifiedName + QualIdSeparator + self.Name
  else
    Result := {QualIdSeparator +} Name; //flag absolute name path?
end;

function TPasItem.GetAttribute(attr: TPasItemAttribute): boolean;
begin
  Result := attr in FAttributes;
end;

function TPasItem.GetAttributeFP(attr: integer): boolean;
var
  id: TPasItemAttribute absolute attr;
begin
  Result := id in FAttributes;
end;

procedure TPasItem.SetAttributeFP(attr: integer; OnOff: boolean);
var
  id: TPasItemAttribute absolute attr;
begin
  if OnOff then
    Include(FAttributes, id)
  else
    Exclude(FAttributes, id);
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
  if FMembers = nil then
    FMembers := TPasItems.Create(True);
    //destruction occurs in inherited destructor
{$IFDEF delegates}
{$ELSE}
  FHeritage := TStringVector.Create;
{$ENDIF}
end;

destructor TPasScope.Destroy;
begin
  FreeAndNil(FMembers);
{$IFDEF delegates}
{$ELSE}
  FreeAndNil(FHeritage);
{$ENDIF}
  inherited;
end;

procedure TPasScope.Deserialize(const ASource: TStream);
begin
  inherited;
  Members.Deserialize(ASource);
{$IFDEF delegates}
  ...
{$ELSE}
  FHeritage.LoadFromBinaryStream(ASource);
{$ENDIF}
end;

procedure TPasScope.Serialize(const ADestination: TStream);
begin
  inherited;
  Members.Serialize(ADestination);
{$IFDEF delegates}
  ...
{$ELSE}
  FHeritage.SaveToBinaryStream(ADestination);
{$ENDIF}
end;

procedure TPasScope.AddMember(item: TPasItem);
begin
  Members.Add(item);
  item.FMyOwner := self as TPasScope;
end;

function TPasScope.FindName(const NameParts: TNameParts; index: integer = -1): TPasItem;
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

function TPasScope.FindItem(const ItemName: string): TBaseItem;
begin
(* FindItem should search in members first.
  If nothing was found, the ancestors are searched, if any exist.

  Note: Ancestor search differs for units (sequential) and CIOs (recursive).
*)
  if assigned(Members) then
    Result := Members.FindName(ItemName)
  else
    Result := nil;
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
  Deprecated!!!
  <name> <description>
  ==> <description> + <RawDescription>
*)
  ReplaceStr := '';
  ValueDesc := TagParameter;
  ValueName := ExtractFirstWord(ValueDesc);

  Value := Members.FindName(ValueName);
  if Assigned(Value) then begin
    if Value.RawDescription = '' then
      Value.FRawDescription := ValueDesc
    else begin
    //concatenate value and description, but only for the first time!
    //separator?
      Value.FRawDescription := ValueDesc + Value.RawDescription;
      FreeAndNil(Value.FRawdescriptionInfo);
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
    result := TObject(FHash.Items[LowerCase(AName)]) as TBaseItem;
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
{$IFDEF delegates}
  FFields := TPasItems.Create(owns); //interfaces have no fields!
  case AKind of
  KEY_RECORD: //has no ancestors
    begin
    end;
  else
    FAncestors := NeedItems.AddNew(trHierarchy, dkItemList, AName);
    FMethods := TPasMethods.Create(owns);
    FProperties := TPasProperties.Create(owns);
  end;
{$ELSE}
//simple: create all
  FFields := TPasItems.Create(owns);
  FMethods := TPasMethods.Create(owns);
  FProperties := TPasProperties.Create(owns);
{$ENDIF}
end;

destructor TPasCio.Destroy;
begin
{$IFDEF delegates}
  FAncestors.Free;
{$ELSE}
  Fields.Free;
  Methods.Free;
  Properties.Free;
{$ENDIF}
  inherited;
end;

function  TPasCio.GetCioType: TCIOType;
begin
  case FKind of
  KEY_CLASS: Result := CIO_CLASS;
  KEY_DISPINTERFACE: Result := CIO_SPINTERFACE;
  KEY_INTERFACE: Result := CIO_INTERFACE;
  KEY_OBJECT: Result := CIO_OBJECT;
  //KEY_RECORD: Result := CIO_RECORD; //could check for "packed"
  else
    Result := CIO_RECORD;
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
{$IFDEF new}
  TTag.Create(TagManager, 'classname',
    nil, {$IFDEF FPC}@{$ENDIF} HandleClassnameTag, []);
  TTag.Create(TagManager, 'inheritedclass',
    nil, {$IFDEF FPC}@{$ENDIF} HandleInheritedClassTag, []);
  TTag.Create(TagManager, 'inherited',
    nil, {$IFDEF FPC}@{$ENDIF} HandleInheritedTag, []);
{$ELSE}
(* We should only provide means to access the names,
  so that the generator handlers can format and link the tags freely.
*)
{$ENDIF}
end;

{$IFDEF new}
procedure TPasCio.HandleClassnameTag(ThisTag: TTag;
  var ThisTagData: TObject; EnclosingTag: TTag;
  var EnclosingTagData: TObject; const TagParameter: string;
  var ReplaceStr: string);
begin
(* test, for move handlers into items.
  It works, so far :-)
*)
  ReplaceStr := Name;
end;
{$ELSE}
{$ENDIF}

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
  if not assigned(Members) then
    exit;
  MemberDesc := TagParameter;
  MemberName := ExtractFirstWord(MemberDesc);

  //Member := FindItem(MemberName); //should only search immediate members?
  Member := Members.FindName(MemberName);
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

{$IFDEF delegates}
function TPasCio.FirstAncestor: TDescriptionItem;
begin
  if IsEmpty(FAncestors) then
    Result := nil
  else
    Result := FAncestors.ItemAt(0);
end;

function TPasCio.FirstAncestorName: string;
var
  item: TDescriptionItem;
begin
  item := FirstAncestor;
  if item = nil then
    Result := ''
  else
    Result := item.Name;
end;

function TPasCio.FindItemInAncestors(const ItemName: string): TPasItem;
var
  item: TDescriptionItem;
begin
//ancestor also searches in it's ancestor(s) (auto recursion)
  item := FirstAncestor;
  if item = nil then
    Result := nil
  else
    Result := item.PasItem.FindItem(ItemName) as TPasItem;
end;
{$ELSE}
function TPasCio.FirstAncestor: TPasCio;
begin
  Result := FHeritage.Objects[0] as TPasCio; //may be nil
end;

function TPasCio.FirstAncestorName: string;
var
  item: TPasCio;
begin
  item := FirstAncestor;
  if item = nil then
    Result := ''
  else
    Result := item.Name;
end;

function TPasCio.FindItemInAncestors(const ItemName: string): TPasItem;
var
  item: TPasCio;
begin
//ancestor also searches in it's ancestor(s) (auto recursion)
  item := FirstAncestor;
  if item = nil then
    Result := nil
  else
    Result := item.FindItem(ItemName) as TPasItem;
end;
{$ENDIF}

{ TPasUnit ------------------------------------------------------------------- }

constructor TPasUnit.Create(AOwner: TPasScope; AKind: TTokenType;
  const AName: string);
const
  owns = False;
begin
  inherited;
{$IFnDEF delegates}
  FTypes := TPasItems.Create(owns);
  FVariables := TPasItems.Create(owns);
  FCIOs := TPasItems.Create(owns);
  FConstants := TPasItems.Create(owns);
  FFuncsProcs := TPasMethods.Create(owns);
  //FUsesUnits := TStringVector.Create;
{$ELSE}
  //create all on demand
{$ENDIF}
end;

destructor TPasUnit.Destroy;
begin
{$IFnDEF delegates}
  FCIOs.Free;
  FConstants.Free;
  FFuncsProcs.Free;
  FTypes.Free;
  //FUsesUnits.Free;
  FVariables.Free;
{$ELSE}
  //free all with items
{$ENDIF}
  inherited;
end;

function TPasUnit.FindItemInAncestors(const ItemName: string): TPasItem;
var
  i: integer;
  uitem: TPasUnit;
begin
  for i := 0 to UsesUnits.Count - 1 do begin
  {$IFnDEF delegates}
    Result := UsesUnits.Objects[i] as TPasScope;
    if Result <> nil then begin
      Result := Result.FindItem(ItemName) as TPasItem;
      if Result <> nil then
        exit;
    end;
  {$ELSE}
    uitem := UsesUnits.PasItemAt(i) as TPasUnit;
    if uitem <> nil then begin
      Result := uitem.FindItem(ItemName) as TPasItem;
      if Result <> nil then
        exit;
    end;
  {$ENDIF}
  end;
//if nothing searched and found
  Result := nil;
end;

procedure TPasUnit.AddMember(item: TPasItem);
begin
  inherited;  //add to Members
{$IFnDEF delegates}
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
{$ELSE}
  case item.Kind of
  KEY_CONST: NeedItems.AddNew(trConstants, ...)
  end;
{$ENDIF}
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
//init what - very obsolete!
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
  //FRaises.Free;
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
  item: TDescriptionItem;
begin
  Fraises := NeedItems.AddNew(trExceptionsRaised, dkItemList);
  item := Fraises.AddExtractFirstWord(trNoTrans, TagParameter);
//AddExtractFirstWord returns Nil if no name could be extracted
  if item = nil then
    ThisTag.TagManager.DoMessage(2, pmtWarning,
      '@raises tag doesn''t specify exception name', [])
  else  //?
    ReplaceStr := '';
end;

procedure TPasMethod.StoreParamTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
var
  param: TDescriptionItem;
begin
  FParams := NeedItems.AddNew(trParameters, dkItemList);
//add as NoTrans, to force unique Name
  param := FParams.AddExtractFirstWord(trNoTrans, TagParameter);
  if param = nil then
    ThisTag.TagManager.DoMessage(2, pmtWarning,
      '@param tag doesn''t specify parameter name, skipped', [])
  else //?
    ReplaceStr := '';
end;

procedure TPasMethod.StoreReturnsTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  if TagParameter = '' then exit;
//Name could hold the function type!
  FReturns := NeedItems.AddString(trReturns, TagParameter);
  ReplaceStr := '';
end;

function TPasMethod.HasMethodOptionalInfo: boolean;
begin
  Result := FOptItems <> nil;
    //(Returns <> '')
    //or (not ObjectVectorIsNilOrEmpty(Params))
    //or (not ObjectVectorIsNilOrEmpty(Raises));

    //or (Items.FindID(trParameters) <> nil)
    //or (Items.FindID(trExceptionsRaised) <> nil);
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

{ TDescriptionItem }

constructor TDescriptionItem.Create(tid: TTranslationID;
  AKind: eDescriptionKind; const AName, AValue: string); //virtual?
begin
(* This can become a class method, that creates e.g. specialized lists?
*)
  inherited Create(AName, AValue);
  ID := tid;
  kind := AKind;
//create object?
  case kind of
  dkStrings:  Data := TStringVector.Create;
  dkItemList: Data := TObjectVector.Create(True);
  dkPasItems: Data := TPasItems.Create(False);
  end;
end;

destructor TDescriptionItem.Destroy;
var
  obj: TObject;
begin
  obj := TObject(Data);
  if obj is TPasItem then
    //never destroy
  else if obj is TObject then
    FreeAndNil(TObject(Data));
  inherited;
end;

{$IFDEF delegates}
procedure TDescriptionItem.LoadFromBinaryStream(Stream: TStream);
var
  i, n: Integer;
begin
  ID := TTr TSerializable.LoadIntegerFromStream(Stream);
  TSerializable.SaveIntegerToStream(ord(kind), Stream);
  Clear;
  n := TSerializable.LoadIntegerFromStream(Stream);
  Capacity := n;
  for i := 0 to n - 1 do
    Append(TSerializable.LoadStringFromStream(Stream));
  ...
end;

procedure TDescriptionItem.SaveToBinaryStream(Stream: TStream);
var
  i: Integer;
  sl: TStrings;
  //il: TObjectVector;
  item: TDescriptionItem;
begin
(* Needed for ancestors.
  Do not save objects, i.e. no TPasItems in dkPasItems.
*)
  TSerializable.SaveIntegerToStream(ord(ID), Stream);
  TSerializable.SaveIntegerToStream(ord(kind), Stream);
  case self.kind of
  dkText:
    TSerializable.SaveStringToStream(Description, Stream);
  dkNameDesc:
    begin
      TSerializable.SaveStringToStream(item.Name, Stream);
      TSerializable.SaveStringToStream(item.Value, Stream);
    end;
  dkStrings:
    begin
      sl := self.GetObject as TStrings; //maybe Nil!?
      TSerializable.SaveIntegerToStream(Count, Stream);
      for i := 0 to Count - 1 do
        TSerializable.SaveStringToStream(sl[i], Stream);
    end;
  //dkPasItem: - nothing stored!
  dkItemList:
    begin
      //il := self.GetObject as TObjectVector; //maybe Nil!?
      TSerializable.SaveIntegerToStream(Count, Stream);
      for i := 0 to Count - 1 do begin
        item := ItemAt(i);
        item.SaveToBinaryStream(Stream);
      end;
    end;
  end;
end;
{$ELSE}
{$ENDIF}

function TDescriptionItem.AddExtractFirstWord(tid: TTranslationID;
  const s: string): TDescriptionItem;
var
  n, desc: string;
begin
  desc := s;
  n := ExtractFirstWord(desc);
  if n = '' then begin
    Result := nil;
    exit;
  end;
  Result := AddNew(tid, dkNameDesc, n, desc);
end;

function TDescriptionItem.GetObject: TObject;
begin
  if TObject(Data) is TObject then
    Result := TObject(Data)
  else
    Result := nil;
end;

function TDescriptionItem.GetCount: integer;
begin
  if TObject(Data) is TObjectVector then
    Result := TObjectVector(Data).Count
  else
    Result := 0;  //or -1, to signal NoList?
end;

function TDescriptionItem.ItemAt(index: integer): TDescriptionItem;
begin
  if index < GetCount then
    Result := TObjectVector(Data).Items[index] as TDescriptionItem
  else
    Result := nil;
end;

function TDescriptionItem.FindID(tid: TTranslationID): TDescriptionItem;
var
  i: integer;
begin
  for i := 0 to GetCount - 1 do begin
    //Result := ItemAt(i);
    Result := TObjectVector(Data).Items[i] as TDescriptionItem;
    if assigned(Result) and (Result.ID = tid) then
      exit;
  end;
  Result := nil;
end;

function TDescriptionItem.PasItem: TPasItem;
begin
  if TObject(Data) is TPasItem then
    Pointer(Result) := Data
  else
    Result := nil;
end;

function TDescriptionItem.PasItemAt(index: integer): TPasItem;
begin
  TObject(Result) := ItemAt(index);
  if not (Result is TPasItem) then
    Result := nil;
end;

function TDescriptionItem.Add(AItem: TDescriptionItem): integer;
var
  lst: TObjectVector;
begin
  lst := TObject(Data) as TObjectVector;
  Result := lst.Add(AItem);
end;

function TDescriptionItem.AddNew(tid: TTranslationID; AKind: eDescriptionKind;
  const AName, AValue: string): TDescriptionItem;
var
  lst: TObjectVector;
begin
(* Prevent dupes!
  Find either by id or by name?
*)
  if Data = nil then
    Data := TObjectVector.Create(True);
  if TObject(Data) is TObjectVector then begin
    lst := TObjectVector(Data);
  //find entry in list, in detail when a contained list is requested!
  {$IFDEF old}
    case AKind of
    dkStrings,  //Authors...
      //split Author into name and description (URL...)?
    dkItemList,
    dkNameDesc: //prevent duplicate ID, for all lists
      if tid = trNoTrans then
        Result := nil //allow any number of dupes (where???)
        //does this mean: dupecheck for names is not required???
      else
        Result := FindID(tid);
    dkPasItems:  //prevent duplicate name
      Result := Find(AName);
    else //add whatsoever
      Result := nil;
    end;
  {$ELSE}
    if tid = trNoTrans then
      Result := Find(AName)
    else
      Result := FindID(tid);
  {$ENDIF}
    if Result = nil then begin
    //create item
      Result := TDescriptionItem.Create(tid, AKind, AName, AValue);
      lst.Add(Result);
    end else begin
    //update item(???)
      if (Result.Name = '') and (AName <> '') then
        Result.Name := AName;
      if (Result.Value = '') and (AValue <> '') then
        Result.Value := AValue;
    end;
  end else
    Result := nil;  //not a object list???
end;

function TDescriptionItem.AddString(tid: TTranslationID;
  const s: string): TDescriptionItem;
begin
  Result := AddNew(tid, dkText, '', s);
end;

function TDescriptionItem.AddToStrings(tid: TTranslationID;
  const s: string): integer;
var
  item: TDescriptionItem;
  sv: TStringVector;
begin
//add s to a string list(!)
  item := AddNew(tid, dkStrings);
  sv := TObject(item.Data) as TStringVector;
  Result := sv.AddNotExisting(s);
end;

function TDescriptionItem.AddStrings(tid: TTranslationID;
  lst: TStrings): TDescriptionItem;
var
  item: TDescriptionItem;
  sv: TStrings;
begin
  item := AddNew(tid, dkStrings);
  if lst <> nil then begin
    sv := TObject(item.Data) as TStrings;
    sv.AddStrings(lst);
  end;
  Result := item;
end;

function TDescriptionItem.Find(const AName: string): TDescriptionItem;
var
  i: integer;
begin
  if AName <> '' then begin
    for i := 0 to Count - 1 do begin
      Result := ItemAt(i);
      if CompareText(Result.Name, AName) = 0 then
        exit; //found
    end;
  end;
//not found
  Result := nil
end;

function TDescriptionItem.Text(const NameValueSeparator,
  ItemSeparator: string): string;
var
  i: Integer;
begin
  if Count > 0 then begin
    Result := Items[0].Name + NameValueSeparator + Items[0].Value;
    for i := 1 to Count - 1 do
      Result := Result + ItemSeparator +
        Items[i].Name + NameValueSeparator + Items[i].Value;
  end else begin //simple item
    if Name = '' then
      Result := Value
    else if Value = '' then
      Result := Name
    else
      Result := Name + NameValueSeparator + Value;
  end;
end;

function IsEmpty(item: TDescriptionItem): boolean;
begin
  Result := (item = nil) or (item.Count <= 0);
end;

initialization
  TSerializable.Register(TPasItem);
  TSerializable.Register(TPasScope);
  TSerializable.Register(TPasEnum);
  TSerializable.Register(TPasMethod);
  TSerializable.Register(TPasProperty);
  TSerializable.Register(TPasCio);
  TSerializable.Register(TPasUnit);
end.
