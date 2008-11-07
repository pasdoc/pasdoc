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

{-$DEFINE item}
{-$DEFINE DetailedProps}
{-$DEFINE paragraphs}
{$DEFINE groups}

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
  PasDoc_Languages;
  //PasDoc_StringPairVector;

type
  { Visibility of a field/method. }
  TItemVisibility = (
  //default visibility: always
    viShow,
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
    viImplicit,
  //unit in uses
    viUses,
  //never show
    viHide
    );

  TVisibility = viPublished .. viImplicit;

//would like to add viUses, to implement ShowUses
  TVisibilities = set of TItemVisibility;
const
  VisibilityAttributes: array[TVisibility] of TStandardDirective = (
    SD_PUBLISHED, SD_PUBLIC,
    SD_PROTECTED, SD_PROTECTED,
    SD_PRIVATE, SD_PRIVATE,
    SD_AUTOMATED, SD_INVALIDSTANDARDDIRECTIVE
  );
var
  ShowVisibilities: TVisibilities;

const
  VisibilityStr: array[TItemVisibility] of string = (
   'show',
   'published',
   'public',
   'protected',
   'strict protected',
   'private',
   'strict private',
   'automated',
   'implicit',
   'uses',
   'hide'
  );

  //AllVisibilities: TVisibilities = [Low(TVisibility) .. High(TVisibility)];
  AllVisibilities: TVisibilities = [viShow..viUses];
  DefaultVisibilities: TVisibilities =
    [viShow, viProtected, viPublic, viPublished, viAutomated];

{ Returns VisibilityStr for each value in Visibilities,
  delimited by commas. }
function VisibilitiesToStr(const Visibilities: TVisibilities): string;

function VisToStr(const Vis: TVisibility): string;

type
  TRawDescriptionInfo = class(TStringList)
  public
    destructor Destroy; override;
  end;
  PRawDescriptionInfo = TRawDescriptionInfo;

  TBaseItem = class;
  TPasItem = class;
  TPasScope = class;
  TPasCio = class;
  TPasMethod = class;
  TPasProperty = class;
  TPasUnit = class;
  TAnchorItem = class;

  TDescriptionList = class;
  TBaseItems = class;
  TPasItems = class;
  TPasUnits = class;

//Callback to assign FullLink to an item
  TLinkGenerator = function(const AItem: TBaseItem): string of object;

(* Organization (=intended layout?) of descriptive elements.
  Should match the expectations of the generators!

  Used only in the constructor, to create the appropriate list.
*)
  eDescriptionKind = (
  //simple entries, without any objects, no dupecheck
    dkNoList,
  //unique list entry (Authors...)
    dkUniqueString,
  //PasItem delegate (specialized class!)
    dkDelegate,
  //PasItems delegate (specialized class!) The list is not owned.
    dkListDelegate,
  //create TPasItems list. The list is owned, but the members are not.
    dkPasItems,
  //for used units and ancestors - become special members
    //dkDelegates,
  //lists, tables(?) Data is description list.
  //this is the default for all items with lists
    dkItemList
  );

(* Replacement of TStringPair. This is the base class of all items.
  Since base items are derived from TSerializable, this is our base class as well.

  Considerations for lists:

  The lists must be unique, either by ID or name.
  Members typically must not be unique (overloaded...).

  All Add... methods create a container list, if none exists.
  All list-related methods should check for Nil-lists!

  Lists of TPasItems can be created only by the parser.
  Delegates for TPasItems are required for e.g. UsedUnits, where the
  TPasUnit objects are added later, based on the item.Name.

  Parser generated lists (TPasItems) must be serialized, and consequently
  must be deserialized into their proper place. This may change, when
  sub-lists of Members are created on demand (Events...).

  Currently specialized member lists are filled while Members is filled.
  Only the Members list must be (de-)serialized.
  Delegates for member lists do not own the lists.
*)
  TDescriptionItem = class(TSerializable)
  protected
  //possible section title, if unnamed. for trNoTrans, Name is used.
    FTID: TTranslationID;
  //prevent destruction of the list
    fExternalList: boolean;
  //exclude flag
    FExclude: boolean;
  //intended use: sequence number for overloaded identifiers, <0 for excluded items.
    FSeqNum: Shortint;
  //description related kind (rename? remove!?)
    //kind: eDescriptionKind;
    FList: TDescriptionList;
  //get list count
    function  GetCount: integer;
    function  GetItemFPC(intID: integer): TDescriptionItem;

    function AddListDelegate(lst: TDescriptionItem): TDescriptionItem; overload;
    function AddListDelegate(tid: TTranslationID; lst: TDescriptionList): TDescriptionItem; overload;
{$IFDEF new}
  //Load all text from stream.
    procedure LoadFromBinaryStream(Stream: TStream); virtual;
    //class function CreateFromStream(Stream: TStream): TDescriptionItem;
    function AddFromStream(Stream: TStream): TDescriptionItem; //virtual;

    { This saves our contents in a format readable by
      @link(LoadFromBinaryStream). }
    procedure SaveToBinaryStream(Stream: TStream); virtual;
{$ELSE}
{$ENDIF}
    function GetRawDescription: string; virtual;
  public
  //name of this item
    Name: string;
  //multi-purpose field, typically description
    Value: string;
    property ID: TTranslationID read FTID;
  //make sure that all fields are initialized
    constructor Create(const AName: string = ''; const AValue: string = '';
      tid: TTranslationID = trNoTrans; AListKind: eDescriptionKind = dkNoList); virtual;
  //destructor also finalizes a possibly existing list
    destructor Destroy; override;

  //get Self as TPasItem, or Nil. Virtual for delegates.
    function  AsPasItem: TPasItem; //virtual;
    procedure SetPasItem(AItem: TPasItem); virtual;
    property  PasItem: TPasItem read AsPasItem write SetPasItem;

  //find item by name
    function  IndexOf(const AName: string): integer; //virtual;
  //find item by ID
    function  IndexOfID(tid: TTranslationID): integer; //virtual;

//list methods
  //add an item. Create FList if required.
    function Add(AItem: TDescriptionItem): integer; //virtual;
  //add an descriptor - general --> get/create item list and add new item to it.
    function  AddNew(tid: TTranslationID; AKind: eDescriptionKind;
      const AName: string = ''; const AValue: string = ''): TDescriptionItem;
  //create a pair of first word and remainder. Returns Nil if no first word is found.
    function AddExtractFirstWord(tid: TTranslationID; const s: string): TDescriptionItem;
  //add an string (as Name)
    function AddString(tid: TTranslationID; const s: string): TDescriptionItem; virtual;
    function AddUniqueString(tid: TTranslationID;
      const s: string): TDescriptionItem;
  //get string - obsolete?
  // We could compose Name+Value, for TStringList code compatibility.
    function  GetString(index: integer): string; //virtual;
    property Strings[index: integer]: string read GetString;
  {$IFDEF new}
  //create a string list - obsolete?
    function AddStrings(tid: TTranslationID; lst: TStrings = nil): TDescriptionItem; virtual;
  {$ELSE}
  {$ENDIF}
  //add unique string to one of our lists. tid is list ID, not string ID.
  //Mimic string vector behaviour.
    function AddToStrings(tblTid, itemTid: TTranslationID; const s: string): integer; //virtual;

  //get PasItem from list - really?
    function  PasItemAt(index: integer): TPasItem; //virtual;
  //get PasItems list, or Nil.
    function  PasItems: TPasItems; //virtual;
  //get description item from list, or Nil
    function  ItemAt(index: integer): TDescriptionItem; //virtual;
  //get descriptive element by ID, or Nil (using IndexOfID)
    function  FindID(tid: TTranslationID): TDescriptionItem;
  //find item by name. FList may use hash...
    function  Find(const AName: string): TDescriptionItem;

    { Returns all items Names and Values glued together.
      For every item, string Name + NameValueSeparator + Value is
      constructed. Then all such strings for every item are
      concatenated with ItemSeparator.

      Non-list items return Name+Value.
    }
    function Text(const NameValueSeparator: string = ' ';
      const ItemSeparator: string = ' '): string; //virtual;

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

    //open array!
    procedure SortByID(const weights: array of TTranslationID);

    property Count: integer read GetCount;
    property Items[index: integer]: TDescriptionItem read ItemAt; //default;
  {$IFDEF old}
    property Description: string read GetRawDescription; //Value;
  {$ELSE}
    property RawDescription: string read GetRawDescription;
    property Caption: string read Name;
  {$ENDIF}
  //Set by @@exclude, to exclude this item from the generated documentation.
    property ToBeExcluded: boolean read FExclude;
  end;

(* Placeholder for external references.
  Only overwrites the PasItem getter/setter. The item is not owned.
  Everything else is handled by the containers, i.e.
  in PasUnit for Uses, and PasCio for Ancestors.

  The methods are protected against Self=Nil, so that it does no harm
  to append .PasItem to a search or other method, that can return Nil.
*)
  TPasDelegate = class(TDescriptionItem)
  public
    MyItem: TPasItem;
  //get TPasItem, or Nil.
    procedure SetPasItem(AItem: TPasItem); override;
  end;

(* List of descriptions or other items.
  Only the search methods are implemented, virtual for possible optimizations.
  Shrink memory requirements by using a neutral base class, which can implement
    either an TObjectVector or TObjectHash?
*)
  TDescriptionList = class(TObjectVector)
  {$IFDEF new}
  private
    procedure LoadFromBinaryStream(Stream: TStream);
    procedure SaveToBinaryStream(Stream: TStream);
  {$ELSE}
  {$ENDIF}
  protected
  //how to sort this list.
    SortKind: TCollectionSortKind;
  public
  //add item, possibly into hash list.
    function  Add(const AObject: TDescriptionItem): integer; virtual;
  //find item by name. Override for hashed lists?
    function  Find(const AName: string): TDescriptionItem; virtual;
  //find item by name. Override for hashed lists?
    function  IndexOfName(const AName: string): integer; virtual;
  //find item by ID
    function  IndexOfID(tid: TTranslationID): integer;

  //append list of items
    procedure InsertItems(lst: TDescriptionList); deprecated;
    procedure AddItems(lst: TDescriptionItem);
  //get description item from list.
    function  ItemAt(index: integer): TDescriptionItem; //virtual;
    property Items[index: integer]: TDescriptionItem read ItemAt; default;
  end;

//for legacy code
  TStringPair = TDescriptionItem;
//should not be used any more!
  TStringPairVector = TDescriptionList;


  { This is a basic item class, that is linkable,
    and has some @link(RawDescription). }
  TBaseItem = class(TDescriptionItem)
  protected
  (* Full description, consisting of Name (abstract) and Value (description).
    More items (hints...) may be added.
    This is a shortcut to the description item, in the description list.

    The description should be separated into:
      ShortDescription (was: abstract, maybe automatic)
      Abstract (only if explicitly defined!)
      DetailedDescription (as defined)

    We could use Name=abstract, Value=detailed
  *)
    FFullDescription: TDescriptionItem;
  //create the description item, if not already done.
    function  NeedDescription: TDescriptionItem;
    function  GetAbstract: string;
    procedure SetAbstract(const s: string);
  //first sentence of description
    function  GetShortDescription: string;
    function  GetDetailedDescription: string;
  //currently needed for external items
    procedure SetDetailedDescription(const s: string);
  protected
  {$IFDEF old}
  {$ELSE}
    FFullLink: string;
    FOutputFileName: string;
  {$ENDIF}
    FDisallowAutoLink: boolean;
    function AutoLinkAllowed: boolean;
  private
  (* RawDescriptionInfo is required for editing.
    While it should only apply to PasItems, delegates also should be editable.
    All detailed information is lost in serialization, the deserialized items
    only contain the concatenated strings.
  *)
    FRawDescriptionInfo: TRawDescriptionInfo;
  //the deserialized description.
    FRawDescription: string;
  protected
  //return either from FRawDescriptionInfo or, if Nil, from FRawDescription.
    function  GetRawDescription: string; override;
  //return first description token. Usage?
    function  GetFirstDescription: TToken;
  //allow doc generator to add more descriptions.
    procedure WriteRawDescription(const AValue: string);
  private
    procedure StoreAbstractTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
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
    procedure HandleSeeAlsoTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);


    procedure HandleExcludeTag(ThisTag: TTag; var ThisTagData: TObject;
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
    destructor Destroy; override;

    { It registers @link(TTag)s that init @link(Authors),
      @link(Created), @link(LastMod) and remove relevant tags from description.
      You can override it to add more handlers. }
    procedure RegisterTags(TagManager: TTagManager); virtual;

  //Links to basic items are created by their owners.
    procedure BuildLinks(AllUnits: TPasUnits; TheGenerator: TLinkGenerator); virtual;
  // Build sections in the right order. Here: Nothing? Description?.
    procedure BuildSections; virtual;
  // Build trDescription, from abstract and detailed description.
  // Override for hints and other specific descriptions.
  // Return description item, for overridden methods.
    function  BuildDescription: TDescriptionItem; virtual;

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

      Override for classes with Members (TPasScope), and again for classes with ancestors,
      to implement/extend the search in these lists.
    }
    function FindItem(const ItemName: string): TBaseItem; virtual;

    { This does all it can to resolve link specified by NameParts.

      While searching this tries to mimic ObjectPascal identifier scope
      as much as it can. It seaches within this item,
      but also within class enclosing this item,
      within ancestors of this class,
      within unit enclosing this item,
      then within units used by unit of this item.
      @param(index is used only for internal purposes)
    }
    function FindName(const NameParts: TNameParts; index: integer = -1): TPasItem; virtual;

    { Abstract description of this item.
      This is intended to be short (e.g. one sentence) description of
      this object.

      This will be inited from @@abstract tag in RawDescription,
      or cutted out from first sentence in RawDescription
      if @--auto-abstract was used.

      Note that this is already in the form suitable for final output,
      with tags expanded, chars converted etc. }
    property AbstractDescription: string
      read GetAbstract write SetAbstract;

    { Detailed description of this item.

      In case of TPasItem, this is something more elaborate
      than @link(TPasItem.AbstractDescription).

      This is already in the form suitable for final output,
      ready to be put inside final documentation.

      The description should be separated into paragraphs,
      in the Description list.
    }
  {$IFDEF old}
    property DetailedDescription: string
      read FDetailedDescription write FDetailedDescription;
  {$ELSE}
    property DetailedDescription: string
      read GetDetailedDescription  write SetDetailedDescription;
  {$ENDIF}

    property ShortDescription: string read GetShortDescription;
  {$IFDEF paragraphs}
    property Descriptions: TDescriptionItem read FDescriptionList;
  {$ELSE}
  {$ENDIF}

    { Returns true if there is a DetailedDescription or AbstractDescription
      available. }
    function HasDescription: Boolean;

    { This stores unexpanded version (as specified
      in user's comment in source code of parsed units)
      of description of this item.
    }
    property RawDescription: string
      read GetRawDescription write WriteRawDescription;
    //for use by parser, only!
  {$IFDEF old}
    property Descriptions: TRawDescriptionInfo read FRawDescriptionInfo;
  {$ELSE}
    property RawDescriptions: TRawDescriptionInfo read FRawDescriptionInfo;
  {$ENDIF}
    //get first description token. Nil if no description found. Usage???
    property FirstDescription: TToken read GetFirstDescription;

    { Full info about @link(RawDescription) of this item,
      including it's filename and position.

      This is intended to be initialized by parser, and used by an editor.
      Detailed information is not preserved during serialization.
    }
    procedure AddRawDescription(const AValue: string; const AStream: string;
      APos: TTextStreamPos); overload;
    //for use by parser
    procedure AddRawDescription(t: TToken); overload;

    { a full link that should be enough to link this item from anywhere else }
    property FullLink: string read FFullLink write FFullLink;

    { Returns the qualified name of the item.
      This is intended to return a concise and not ambigous name.
      E.g. in case of TPasItem it is overriden to return Name qualified
      by class name and unit name.

      In this class this simply returns Name. }
    function QualifiedName: String; virtual;

    { name of documentation output file (if each class / object gets
      its own file, that's the case for HTML, but not for TeX) }
    property OutputFileName: string read FOutputFileName write FOutputFileName;

    { Is auto-link mechanism allowed to create link to this item ?
      This may be set to @false by @@noAutoLinkHere tag in item's description. }
    property AutoLinkHereAllowed: boolean read AutoLinkAllowed;

    { The full (absolute) path used to resolve filenames in this item's descriptions.
      Must always end with PathDelim.
      In this class, this simply returns GetCurrentDir (with PathDelim added if needed). }
    function BasePath: string; virtual;

    { list of strings, each representing one author of this item }
    property Authors: TDescriptionItem index ord(trAuthors) read GetItemFPC;

    { Contains '' or string with date of creation.
      This string is already in the form suitable for final output
      format (i.e. already processed by TDocGenerator.ConvertString).
      Are you sure???
    }
    property Created: TDescriptionItem index ord(trCreated) read GetItemFPC;

    { Contains '' or string with date of last modification.
      This string is already in the form suitable for final output
      format (i.e. already processed by TDocGenerator.ConvertString). }
    property LastMod: TDescriptionItem index ord(trLastModified) read GetItemFPC;

    property SeeAlso: TDescriptionItem index ord(trSeeAlso) read GetItemFPC;
  end;


  { This is a @link(TBaseItem) descendant that is always declared inside
    some Pascal source file.

    Parser creates only items of this class
    (e.g. never some basic @link(TBaseItem) instance).
    This class introduces properties and methods pointing
    to parent unit (@link(MyUnit)) and parent class/interface/object/record
    (@link(MyObject)). }

  TPasItem = class(TBaseItem)
  private
    //FFullDeclaration: string; //trDeclaration?
    FVisibility: TItemVisibility;

    procedure HandleDeprecatedTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
    procedure GroupTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
    procedure GroupBegin(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
    procedure GroupEnd(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
  protected
    procedure Serialize(const ADestination: TStream); override;
    procedure Deserialize(const ASource: TStream); override;

  protected
  // The declarative token, "unit", "class", "type" etc.
    FKind: TTokenType;
  // All attributes, modifiers etc.
    FAttributes: TPasItemAttributes;
  //-FSeqNum here? (for excluded and overloaded items)
  // Position of identifier in the declaration.
    FNamePosition: TTextStreamPos;
  // File name with the declaration (may become file object?)
    FNameStream: string;
  (* Ancestor reference, depending on item kind.
    CIOs have a list of ancestors (trHierarchy). The first item is the base class, others are interfaces.
    Units have a list of used units (trUnits).
    The lists are owned by scope classes.
    Class members have a reference to their (virtual) ancestor item (tr?).
  *)
    FHeritage: TDescriptionItem;
  { Owner shall be the item, of which this item is a member.
    Usage: construct the fully qualified name...

    Derived: MyObject (read: MyCio), MyUnit.
    Initialized when added to the Members of the owner.
  }
    FMyOwner: TPasScope;
  //obsolete?
    function  GetMyObject: TPasCio;
    function  GetMyUnit: TPasUnit;
  public
    function  IsKey(AKey: TTokenType): boolean;
    function  GetAttributeFP(attr: integer): boolean;
    procedure SetAttributeFP(attr: integer; OnOff: boolean);
    function  GetAttribute(attr: TPasItemAttribute): boolean;
    procedure SetAttribute(attr: TPasItemAttribute; OnOff: boolean);
    procedure SetVisibility(attr: TItemVisibility);

    function  PasScope: TPasScope; virtual;
    property Kind: TTokenType read FKind;
    property Attributes: TPasItemAttributes read FAttributes write FAttributes;
    property NamePosition: TTextStreamPos read FNamePosition write FNamePosition;
    property NameStream: string read FNameStream write FNameStream;
    property MyOwner: TPasScope read FMyOwner;  //- write FMyOwner;

  public
    constructor Create(AOwner: TPasScope; AKind: TTokenType;
      const AName: string); reintroduce; virtual;

    procedure RegisterTags(TagManager: TTagManager); override;

  //Build description, standard + hints.
    function  BuildDescription: TDescriptionItem; override;
  // Build sections in the right order. Here: Overview.
    procedure BuildSections; override;

  //search enclosing scopes.
    function FindName(const NameParts: TNameParts; index: integer = -1): TPasItem; override;

    { pointer to unit this item belongs to }
    property MyUnit: TPasUnit read GetMyUnit; //- write SetMyUnit;

    { if this item is part of an object or class, the corresponding
      info object is stored here, nil otherwise }
    property MyObject: TPasCio read GetMyObject;  //- write SetMyObject;
  //Returns the first ancestor of this item. Override for CIOs
    function FirstAncestorItem: TDescriptionItem; virtual;

    property HasAttribute[attr: TPasItemAttribute]: boolean
      read GetAttribute write SetAttribute;

    { Is this item deprecated? }
    property IsDeprecated: boolean index ord(SD_DEPRECATED)
      read GetAttributeFP write SetAttributeFP;

    { Is this item platform specific?
      This is decided by "platform" hint directive after an item. }
    property IsPlatformSpecific: boolean index ord(SD_PLATFORM)
      read GetAttributeFP write SetAttributeFP;

    { Is this item specific to a library?
      This is decided by "library" hint directive after an item. }
    property IsLibrarySpecific: boolean index ord(SD_Library_)
      read GetAttributeFP write SetAttributeFP;

    property Visibility: TItemVisibility read FVisibility write SetVisibility;

     { Full declaration of the item.
       This is (full) parsed declaration of the given item.

       DoDi: Long initializers and declarations are abbreviated now,
       so that the declaration can be displayed as-is. Not for initialized arrays?

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

       DoDi: why should a generator ever try to construct a full declaration,
       that has been fully recognized by the parser???

       Spaces could be trimmed.
       }
  {$IFDEF old}
    property FullDeclaration: string read FFullDeclaration write FFullDeclaration;
  {$ELSE}
  //experimental
    property FullDeclaration: string read Value write Value;
  {$ENDIF}

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
    //return output file to use.
    function  GetOutputFileName: string;
end;

  TPasItemClass = class of TPasItem;

  { Container class to store a list of @link(TBaseItem)s.
    Essentially implements all the hash-related stuff.
  }
  TBaseItems = class(TDescriptionList)
  private
    FHash: TObjectHash;
    procedure Serialize(const ADestination: TStream);
    procedure Deserialize(const ASource: TStream);
  public
    constructor Create(AOwnsObject: Boolean); override;
    destructor Destroy; override;

    { Compares each element's name field with Name and returns the item on
      success, nil otherwise.
      Name's case is not regarded.
      Uses built-in hash table.

      Bad name, should read FindItem. FindName is used to retrieve TPasItems.
      Corrected bad cast, from TPasItem into TBaseItem.
    }
    function FindName(const AName: string): TBaseItem;

    { During Add, AObject is associated with AObject.Name using hash table,
      so remember to set AObject.Name @italic(before) calling Add(AObject). }
    function  Add(const AObject: TDescriptionItem): integer; override;

    procedure Delete(const AIndex: Integer);
    procedure Clear; override;
  end;

  { Container class to store a list of @link(TPasItem)s. }
  TPasItems = class(TBaseItems)
  private
    function GetPasItemAt(const AIndex: Integer): TPasItem;
  public
    { Do a FindItem, even if the name suggests something different!
      This is a comfortable routine that just calls inherited
      (ending up in THash.GetObject), and casts result to TPasItem,
      since every item on this list must be always TPasItem. }
    function FindName(const AName: string): TPasItem;

    { Copies all Items from c to this object, not changing c at all. }
    procedure CopyItems(const c: TPasItems);

    { Counts classes, interfaces and objects within this collection. }
    procedure CountCIO(var c, i, o: Integer);

    // Get last added item
    function LastItem: TPasItem;

    property PasItemAt[const AIndex: Integer]: TPasItem read GetPasItemAt;

    { This sorts all items on this list by their name,
      and also calls @link(TPasItem.Sort Sort(SortSettings))
      for each of these items.
      This way it sorts recursively everything in this list.

      This is equivalent to doing both
      @link(SortShallow) and @link(SortOnlyInsideItems).

      Override for Cios, which deserve even deeper sorting.
    }
    procedure SortDeep(const SortSettings: TSortSettings); //virtual;

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

    function Text(const NameValueSeparator, ItemSeparator: string): string;
  end;

(* Item with members.
  All members are owned by the global Members list.
  Specialized member lists can contain subsets of Members.
  For ease of coding, all expected member lists should be
  created during construction, and rememberd in the defined shortcuts,
  and in MemberLists, the owner of all member lists.
  Sorting assumes that ALL member lists are in MemberLists.

  For use by the generators, an Overview list is created from the non-empty
  member lists. This list can have different IDs, e.g.
    trOverview in units, or
    trValues in enums.
  Any number of such lists can occur, less with omitted empty lists, more by grouping.
*)

  eGroupAs = (
    gaSingle, //goup this item
    gaStart, gaEnd //start/end grouping of items
  );

  TPasScope = class(TPasItem)
  protected
  {$IFDEF old}
    FOutputFileName: string;
  {$ELSE}
    //allow for every PasItem to reside in it's own file.
  {$ENDIF}
  //List of all members, for internal use only.
  //Possibly exported as Values (enum...)?
    FMembers: TPasItems;
  {$IFDEF old}
  //Ancestors or used units. The list is owned by this class.
    FHeritage: TDescriptionItem;
  {$ELSE}
    //in TPasItem
  {$ENDIF}
  (* List of special member lists, for internal use only.
    This list contains sublists of selected members.
    The sublists must NOT own the members.
  *)
    FMemberLists: TDescriptionItem;
  //Overview item of non-empty memberlists, preprocessed for generators.
  //Special in enum (=Members), procs...
    FOverview: TDescriptionItem;
    function  NeedOverview: TDescriptionItem;
  protected
  //Group member(s). AParam is the full group spec (tag parameter).
    function DoGroup(how: eGroupAs; AItem: TPasItem;
      const AParam: string; lst: TDescriptionItem = nil): boolean; virtual;

    procedure Serialize(const ADestination: TStream); override;
    procedure Deserialize(const ASource: TStream); override;

    { This searches for ItemName in the ancestors.
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

  // Build sections in the right order. Here: Overview.
    procedure BuildSections; override;
  //build member lists, default: for Units, Cios.
    procedure BuildMemberLists; virtual;
    function  PasScope: TPasScope; override;

  //add member, override for specialized lists
    procedure AddMember(item: TPasItem); virtual;

    { Is Visibility of items (Fields, Methods, Properties) important ? }
    function ShowVisibility: boolean;

  //sort member lists
    procedure Sort(const SortSettings: TSortSettings); override;

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

  //All members
    property Members: TPasItems read FMembers;
  //Raw list of specialized member lists.
    property MemberLists: TDescriptionItem read FMemberLists;
  //Overview section, preprocessed: all non-empty member lists and other items.
    property Overview: TDescriptionItem read FOverview;
  end;


//-------------- unused classes ------------------

//Special scopes, having ancestors and output files
  TPasPrimaryScope = TPasScope;
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
  TPasEnum = class(TPasScope) //-(TPasType)
  protected
  {$IFDEF groups}
  //group member(s) from the first (default) Values list.
    function DoGroup(how: eGroupAs; AItem: TPasItem;
      const AParam: string; lst: TDescriptionItem = nil): boolean; override;
  {$ELSE}
  {$ENDIF}
    procedure StoreValueTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
  public
    constructor Create(AOwner: TPasScope; AKind: TTokenType;
      const AName: string); override;
    procedure BuildLinks(AllUnits: TPasUnits; TheGenerator: TLinkGenerator); override;
  {$IFDEF old}
    procedure BuildMemberLists; override;
  {$ELSE}
  {$ENDIF}

    procedure Sort(const SortSettings: TSortSettings); override;

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
  //these are section shortcuts
    FReturns, //string;
    FRaises,  //: TStringPairVector;
    FParams: TDescriptionItem;
  {$IFDEF old}
    FWhat: TMethodType;
    procedure Serialize(const ADestination: TStream); override;
    procedure Deserialize(const ASource: TStream); override;
  {$ELSE}
  {$ENDIF}
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
  {$IFDEF old}
    constructor Create(AOwner: TPasScope; AKind: TTokenType;
      const AName: string); override;
  {$ELSE}
  {$ENDIF}

    { In addition to inherited, this also registers @link(TTag)s
      that init @link(Params), @link(Returns) and @link(Raises)
      and remove according tags from description. }
    procedure RegisterTags(TagManager: TTagManager); override;

  {$IFDEF old}
    procedure BuildMemberLists; override;
  (* Build section list. Sort!
  *)
    procedure BuildSections; override;
  {$ELSE}
    //-obsolete, no members, no member lists!
  {$ENDIF}

    { obsolete }
  {$IFDEF old}
    property What: TMethodType read FWhat write FWhat;
  {$ELSE}
    property What: TTokenType read FKind;  // write FWhat;
  {$ENDIF}

    { Note that Params, Returns, Raises are already in the form processed by
      @link(TTagManager.Execute), i.e. with links resolved,
      html characters escaped etc. So @italic(don't) convert them (e.g. before
      writing to the final docs) once again (by some ExpandDescription or
      ConvertString or anything like that). }
    { }

    { Name of each item is the name of parameter (without any surrounding
      whitespace), Value of each item is users description for this item
      (in already-expanded form). }
    property Params: TDescriptionItem read FParams;

    //Result could be added as a parameter
    //property Returns: string read FReturns;
    property Returns: TDescriptionItem read FReturns;

    { Name of each item is the name of exception class (without any surrounding
      whitespace), Value of each item is users description for this item
      (in already-expanded form). }
    property Raises: TDescriptionItem read FRaises;

    { Are some optional properties (i.e. the ones that may be empty for
      TPasMethod after parsing unit and expanding tags --- currently this
      means @link(Params), @link(Returns) and @link(Raises)) specified ?
      Very obsolete! (use FList instead)
    }
    function HasMethodOptionalInfo: boolean;
  end;

(* Properties should have a Uses (like) list, for read/write attributes.
  They can own the related fields or methods.
  They can be subdivided into events and other properties (how? @@event?)

  Most properties are obsolete (of no use).
  Getters/Setters should be added to the description,
    eventually as SeeAlso?
    or as members, removed from the (fields/methods) lists?
*)
  TPasProperty = class(TPasItem)
  {$IFDEF DetailedProps}
  //these are unused
  protected
    FIndexDecl: string;
    FStoredID: string;
    FDefaultID: string;
    FPropType: string;
  {$ELSE}
  {$ENDIF}
  protected
  //these deserve special handling, lookup (also in ancestors!)
    FWriter: string;
    FReader: string;
    procedure Serialize(const ADestination: TStream); override;
    procedure Deserialize(const ASource: TStream); override;
  public
  {$IFDEF DetailedProps}
    { contains the optional index declaration, including brackets }
    property IndexDecl: string read FIndexDecl write FIndexDecl;
    { contains the type of the property }
    property Proptype: string read FPropType write FPropType;
    { keeps default value specifier }
    property DefaultID: string read FDefaultID write FDefaultID;
    { keeps Stored specifier - link? }
    property StoredId: string read FStoredID write FStoredID;
  {$ELSE}
  {$ENDIF}
    { read specifier - link? }
    property Reader: string read FReader write FReader;
    { write specifier - link? }
    property Writer: string read FWriter write FWriter;
    { true if the property is the default property }
    property Default: Boolean //read FDefault write FDefault;
      index ord(SD_DEFAULT) read GetAttributeFP write SetAttributeFP;
    { true if Nodefault property }
    property NoDefault: Boolean //read FNoDefault write FNoDefault;
      index ord(SD_NODEFAULT) read GetAttributeFP write SetAttributeFP;
  end;

  TClassDirective = (CT_NONE, CT_ABSTRACT, CT_SEALED);

  { @abstract(Extends @link(TPasScope) to store all items in
    a class / an object, e.g. fields.) }
  //-TPasCio = class(TPasType)
  TPasCio = class(TPasPrimaryScope)
  protected
    FFields,
    FMethods,
    FProperties: TPasItems;
    property FAncestors: TDescriptionItem read FHeritage write FHeritage;
    function  GetClassDirective: TClassDirective;
  //group member(s) from the appropriate list.
    function DoGroup(how: eGroupAs; AItem: TPasItem;
      const AParam: string; lst: TDescriptionItem = nil): boolean; override;

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

  (* Resolve links to inherited classes and interfaces.
    Init output filename (all scopes?)
  *)
    procedure BuildLinks(AllUnits: TPasUnits; TheGenerator: TLinkGenerator); override;
  (* Build section list
  *)
    procedure BuildSections; override;

    {@name is used to indicate whether a class is sealed or abstract.}
    property ClassDirective: TClassDirective //read FClassDirective write FClassDirective;
      read GetClassDirective;

  //add item to members and to appropriate list
    procedure AddMember(item: TPasItem); override;

    { This searches for item (field, method or property) defined
      in ancestor of this cio. I.e. searches within the FirstAncestor,
      which in turn searches in his ancestors.
      Returns nil if not found. }
    function FindItemInAncestors(const ItemName: string): TPasItem; override;

  {$IFDEF oldsort}
    procedure Sort(const SortSettings: TSortSettings); override;
  {$ELSE}
  {$ENDIF}

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
    property Ancestors: TDescriptionItem read FHeritage;

    { This returns Ancestors.Objects[0], i.e. instance of the first ancestor of this Cio,
      or nil if it couldn't be found or Ancestors.Count = 0. }
    function FirstAncestorItem: TDescriptionItem; override;
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
    property Methods: TPasItems read FMethods;

    { list of properties }
    property Properties: TPasItems read FProperties;

    { determines if this is a class, an interface or an object }
    property MyType: TTokenType read FKind;
  end;

  EAnchorAlreadyExists = class(Exception);

  { @name extends @link(TBaseItem) to store extra information about a project.
    @name is used to hold an introduction and conclusion to the project.

    Anchors contains a flat list of all sections and anchors, where anchors
    eventually must be copied from the section items.
  }
  TExternalItem = class(TBaseItem)
  private
    FSourceFilename: string;
  {$IFDEF old}
    FOutputFileName: string;
  {$ELSE}
  {$ENDIF}
    FShortTitle: string;
    // See @link(Anchors).
    FAnchors: TBaseItems;
    procedure SetOutputFileName(const AValue: string);
  protected
    procedure HandleTitleTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
    procedure HandleShortTitleTag(ThisTag: TTag; var ThisTagData: TObject;
      EnclosingTag: TTag; var EnclosingTagData: TObject;
      const TagParameter: string; var ReplaceStr: string);
  public
  {$IFDEF old}
  //why should FAnchors not be created and destroyed?
    Constructor Create; override;
    destructor Destroy; override;
  {$ELSE}
  {$ENDIF}
    procedure RegisterTags(TagManager: TTagManager); override;
    { name of documentation output file }
    property OutputFileName: string read FOutputFileName write SetOutputFileName;
    property SourceFileName: string read FSourceFilename write FSourceFilename;
    property ShortTitle: string read FShortTitle write FShortTitle;
  //CreateLink retrieves the anchor names from here.
    property Anchor: string read Name write Name;
    property Title: string read Value write Value;
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

    property DetailedDescription: string
      read GetDetailedDescription write SetDetailedDescription;

    function BasePath: string; override;
  end;

  TAnchorItem = class(TBaseItem)
  private
  //the introduction/conclusion item
    FExternalItem: TExternalItem;
  //0 for anchors, 1..6 for sections
    FSectionLevel: Integer;
  //sections only?
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

  TMemberListItem = TPasItems;

  { extends @link(TPasScope) to store anything about a unit, its constants,
    types etc.; also provides methods for parsing a complete unit.

    Note: Remember to always set @link(CacheDateTime) after
    deserializing this unit. }
  TPasUnit = class(TPasPrimaryScope)
  protected
    FSourceFilename: string;
    FCacheDateTime: TDateTime;
    FSourceFileDateTime: TDateTime;
  //all shortcuts to the lists of description items
    FTypes,
    FVariables,
    FCIOs,
    FConstants,
    FFuncsProcs: TMemberListItem; //TPasItems;?
    property FUsesUnits: TDescriptionItem read FHeritage write FHeritage;
  //group member(s) from the appropriate list.
    function DoGroup(how: eGroupAs; AItem: TPasItem;
      const AParam: string; lst: TDescriptionItem = nil): boolean; override;

    { This searches for item in all used units.
      Returns nil if not found. }
    function FindItemInAncestors(const ItemName: string): TPasItem; override;

    procedure Serialize(const ADestination: TStream); override;
    procedure Deserialize(const ASource: TStream); override;
  public
    constructor Create(AOwner: TPasScope; AKind: TTokenType;
      const AName: string); override;

  (* Resolve links to used units, inherited classes and interfaces.
    To be called by generator.
  *)
    procedure BuildLinks(AllUnits: TPasUnits; TheGenerator: TLinkGenerator); override;
  (* Build section list
  *)
    procedure BuildSections; override;

  //add item to members and to appropriate list
    procedure AddMember(item: TPasItem); override;

    function FindFieldMethodProperty(const S1, S2: string): TPasItem;

    procedure Sort(const SortSettings: TSortSettings); override;
  public
    { list of classes, interfaces, objects, and records defined in this unit }
    property CIOs: TMemberListItem read FCIOs;
    { list of constants defined in this unit }
    property Constants: TMemberListItem read FConstants;
    { list of functions and procedures defined in this unit }
    property FuncsProcs: TMemberListItem read FFuncsProcs;
    { list of types defined in this unit }
    property Types: TMemberListItem read FTypes;
    { list of variables defined in this unit }
    property Variables: TMemberListItem read FVariables;

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
    property UsesUnits: TDescriptionItem read FHeritage; //FUsesUnits;

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
  //allow to modify ID
    property ID: TTranslationID read FTID write FTID;

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
    procedure BuildSections; //override;
  end;

function IsEmpty(item: TDescriptionItem): boolean; overload;
function IsEmpty(item: TDescriptionList): boolean; overload;

implementation

uses PasDoc_Utils, Contnrs, StrUtils;

type
//serialization of count fields, fixed to 4 bytes
  TCountField = LongInt;

var
  ItemWeights: array[TTranslationID] of integer; //byte would be sufficient
//grouping
  FGroup: TDescriptionItem;
  gFirst, gLast, gScope: TDescriptionItem;

function CompareWeight(PItem1, PItem2: pointer): integer;
var
  Item1: TDescriptionItem absolute PItem1;
  Item2: TDescriptionItem absolute PItem2;
begin
  Result := ItemWeights[Item1.ID] - ItemWeights[Item2.ID];
end;

function CompareDescriptionItemsByName(PItem1, PItem2: Pointer): Integer;
var
  Item1: TDescriptionItem absolute PItem1;
  Item2: TDescriptionItem absolute PItem2;
begin
  Result := CompareText(Item1.Name, Item2.Name);
  //if Result = 0 then Result := CompareText(Item1.QualifiedName, Item2.QualifiedName);
end;

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
    then by visibility,
    finally by name, I assume?
  }
  Result := ord(P1.What) - ord(P2.What);
  if Result <> 0 then
    exit;
  Result := ord(P1.Visibility) - ord(P2.Visibility);
  if Result <> 0 then
    exit;
  Result := ComparePasItemsByName(PItem1, PItem2);
end;

{ TBaseItem ------------------------------------------------------------------- }

destructor TBaseItem.Destroy;
begin
  FreeAndNil(FRawDescriptionInfo);
  inherited;
end;

function TBaseItem.AutoLinkAllowed: boolean;
begin
  Result := not FDisallowAutoLink;
end;

function TBaseItem.FindItem(const ItemName: string): TBaseItem;
begin
//override in TExternalItem and TPasItem descendants (unit, cio, enum)
  Result := nil;
end;

function TBaseItem.FindName(const NameParts: TNameParts; index: integer = -1): TPasItem;
begin
//override in TPasItem, to search the whole scope.
  Result := nil;
end;

procedure TBaseItem.StoreAuthorTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  if TagParameter = '' then exit;
  AddToStrings(trAuthors, trAuthor, TagParameter);
  ReplaceStr := '';
end;

procedure TBaseItem.StoreCreatedTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  if TagParameter = '' then exit;
  AddString(trCreated, TagParameter);
  ReplaceStr := '';
end;

procedure TBaseItem.StoreLastModTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  if TagParameter = '' then exit;
  AddString(trLastModified, TagParameter);
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
        AddString(trLastModified, Trim(Copy(TagParameter, 7, Length(TagParameter)-7-1)) + ' UTC');
        ReplaceStr := '';
      end;
    'A':
      if Copy(TagParameter,1,9) = '$Author: ' then begin
        s := Trim(Copy(TagParameter, 9, Length(TagParameter)-9-1));
        if s <> '' then begin
          AddToStrings(trAuthors, trAuthor, s);
          ReplaceStr := '';
        end;
      end;
    //else //ignore
    end;
  end;
end;

procedure TBaseItem.StoreAbstractTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  if TagParameter <> '' then begin
    if AbstractDescription <> '' then
      ThisTag.TagManager.DoMessage(1, pmtWarning,
        '@abstract tag was already specified for this item. ' +
        'It was specified as "%s"', [AbstractDescription]);
    AbstractDescription := TagParameter;
  end;
  ReplaceStr := '';
end;

procedure TBaseItem.HandleSeeAlsoTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
var
  lst: TDescriptionItem;
begin
  lst := AddNew(trSeeAlso, dkItemList);
  if lst.AddExtractFirstWord(trSeeAlso, TagParameter) = nil then
    ThisTag.TagManager.DoMessage(2, pmtWarning,
      '@seealso tag doesn''t specify any name to link to, skipped', [])
  else
    ReplaceStr := '';
end;

procedure TBaseItem.PreHandleNoAutoLinkTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  { We set AutoLinkHereAllowed in the 1st pass of expanding descriptions
    (i.e. in PreHandleNoAutoLinkTag, not in HandleNoAutoLinkTag)
    because all information about AutoLinkHereAllowed must be collected
    before auto-linking happens in the 2nd pass of expanding descriptions. }
  FDisallowAutoLink := True;
  ReplaceStr := '';
end;

procedure TBaseItem.HandleNoAutoLinkTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
//handled in PreHandle...
  ReplaceStr := '';
end;

procedure TBaseItem.HandleExcludeTag(ThisTag: TTag;
  var ThisTagData: TObject; EnclosingTag: TTag;
  var EnclosingTagData: TObject; const TagParameter: string;
  var ReplaceStr: string);
begin
(* Mark item for exclude from the memberlists.
  Units deserve handling in the generator, FUnits list.
*)
  ThisTag.TagManager.DoMessage(1, pmtInformation, 'Excluded: %s', [self.Name]);
  FExclude := True;
  ReplaceStr := '';
end;

procedure TBaseItem.RegisterTags(TagManager: TTagManager);
begin
  TTag.Create(TagManager, 'exclude',
    {$ifdef FPC}@{$endif} HandleExcludeTag, nil, []);
  TTag.Create(TagManager, 'author', nil, {$IFDEF FPC}@{$ENDIF} StoreAuthorTag,
    [toParameterRequired]);
  TTag.Create(TagManager, 'created', nil, {$IFDEF FPC}@{$ENDIF} StoreCreatedTag,
    [toParameterRequired, toRecursiveTags, toAllowNormalTextInside]);
  TTag.Create(TagManager, 'lastmod', nil, {$IFDEF FPC}@{$ENDIF} StoreLastModTag,
    [toParameterRequired, toRecursiveTags, toAllowNormalTextInside]);
  TTag.Create(TagManager, 'cvs', nil, {$IFDEF FPC}@{$ENDIF} StoreCVSTag,
    [toParameterRequired]);
  TTopLevelTag.Create(TagManager, 'abstract',
    nil, {$IFDEF FPC}@{$ENDIF} StoreAbstractTag,
    [toParameterRequired, toRecursiveTags, toAllowOtherTagsInsideByDefault,
     toAllowNormalTextInside]);
  TTopLevelTag.Create(TagManager, 'seealso',
    nil, {$ifdef FPC}@{$endif} HandleSeeAlsoTag,
    [toParameterRequired, toFirstWordVerbatim]);
  TTopLevelTag.Create(TagManager, 'noautolinkhere',
    {$IFDEF FPC}@{$ENDIF} PreHandleNoAutoLinkTag,
    {$IFDEF FPC}@{$ENDIF} HandleNoAutoLinkTag, []);
end;

procedure TBaseItem.BuildLinks(AllUnits: TPasUnits;
  TheGenerator: TLinkGenerator);
begin
  //nop, here
end;

function TBaseItem.HasDescription: Boolean;
begin
  //HasDescription := (AbstractDescription <> '') or (DetailedDescription <> '');
  Result := FFullDescription <> nil;
end;

function TBaseItem.NeedDescription: TDescriptionItem;
begin
  if FFullDescription = nil then
    FFullDescription := AddNew(trDescription, dkNoList);
  Result := FFullDescription;
end;

function TBaseItem.GetAbstract: string;
begin
  if assigned(FFullDescription) then
    Result := FFullDescription.Name
  else
    Result := '';
end;

procedure TBaseItem.SetAbstract(const s: string);
begin
  NeedDescription.Name := s;
end;

function TBaseItem.GetDetailedDescription: string;
begin
  if assigned(FFullDescription) then
    Result := FFullDescription.Value
  else
    Result := '';
end;

procedure TBaseItem.SetDetailedDescription(const s: string);
begin
//description can start with NL!
  if s <> '' then
    NeedDescription.Value := s;
end;

function TBaseItem.BuildDescription: TDescriptionItem;
begin
(* Return the general description item, if created by generator or tag manager.
  Try to inherit missing descriptions.
*)
  Result := FFullDescription;
end;

procedure TBaseItem.BuildSections;
//var  desc: TDescriptionItem;
begin
(* FFullDescription holds abstract and detailed description.
  It's created by the TagManager, or in BuildDescription?
*)
  {desc :=} BuildDescription;
end;

function TBaseItem.GetShortDescription: string;
var
  i: integer;
const
  MaxShortDescription = 60; //limit description length - become parameter???
begin
(* Get abstract, or first sentence, or first part of description.
  Handling of line breaks???
  It seems to be good to also truncate excessive abstracts.
*)
{ ToDo: collect words, until end of sentence or limit reached. }

//try abstract, if found
  Result := AbstractDescription;
  if Result = '' then begin
  //try first sentence of full description
    Result := DetailedDescription;
  //extract first sentence - retry after '.'<non-white>?
    i := Pos('.', Result);
    if (i > 1) and (i < Length(Result)) and (Result[i+1] in WhiteSpace) then begin
      Result := Copy(Result, 1, i+1);
      //exit;
    end;
  end;
//fit as short description?
  if Length(Result) > MaxShortDescription then begin
  //trim description - could process words
    SetLength(Result, MaxShortDescription);
    Result := Result + '...';
  end; //else fits already
end;

function TBaseItem.QualifiedName: String;
begin
  Result := Name;
end;

procedure TBaseItem.Deserialize(const ASource: TStream);
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

procedure TBaseItem.AddRawDescription(const AValue, AStream: string;
  APos: TTextStreamPos);
var
  c: TToken;
begin
//add qualified description, from external file
  if Assigned(FRawDescriptionInfo) then begin
    c := TToken.Create(TOK_COMMENT_EXT);
    c.CommentContent := AValue;
    c.Mark := '#';  //mark external (linked) comment
    c.StreamName := AStream;
    c.BeginPosition := APos;
    FRawDescriptionInfo.AddObject(AValue, c);
  end else
    WriteRawDescription(AValue);
end;

procedure TBaseItem.WriteRawDescription(const AValue: string);
begin
//append (more) descriptive text
  if Assigned(FRawDescriptionInfo) then //add new entry, without token
    FRawDescriptionInfo.Add(AValue)
  else if FRawDescription <> '' then  //append as new paragraph
    FRawDescription  := FRawDescription + LineEnding + LineEnding + AValue
  else //add first description
    FRawDescription := AValue;
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
  inherited Create(AName, '', trNoTrans, dkNoList); //(name, value, tid?, dkKind?)
//not always used, but required even for parser
  if FRawDescriptionInfo = nil then
    FRawDescriptionInfo := TRawDescriptionInfo.Create;
//all but TPasUnit's must have an valid owner
  if assigned(AOwner) then
    AOwner.AddMember(self)
  else if ord(Kind) > 0 then begin
    assert(self is TPasUnit, 'Non-Unit without owner: ' + TokenNames[kind]);
  end;
//assign tid?
  case AKind of
  //KEY_ARRAY: ftid := trArray; - ???
  KEY_CLASS:  FTID := trClass;
  //KEY_CONST:  FTID := trConstants; - ???
  KEY_CONSTRUCTOR, KEY_DESTRUCTOR, KEY_FUNCTION, KEY_PROCEDURE: FTID := trSubroutine;
  KEY_DISPINTERFACE: FTID := trDispInterface;
  KEY_INTERFACE: FTID := trInterface;
  KEY_LIBRARY:  FTID := trLibrary;
  KEY_OBJECT:   FTID := trObject;
  KEY_PROGRAM:  FTID := trProgram;
  KEY_RECORD:   FTID := trRecord;
  //KEY_SET:    FTID := trSet; - ???
  KEY_TYPE:   FTID := trType;
  KEY_UNIT:   FTID := trUnit;
  //else: stay trNoTrans
  end;
end;

function TPasItem.IsKey(AKey: TTokenType): boolean;
begin
  Result := FKind = AKey;
end;

function TPasItem.PasScope: TPasScope;
begin
  Result := nil;
end;

procedure TPasItem.HandleDeprecatedTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  Include(FAttributes, SD_DEPRECATED);
  ReplaceStr := '';
end;

procedure TPasItem.GroupTag(ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  if MyOwner.DoGroup(gaSingle, self, TagParameter) then
    ReplaceStr := ''
  else //error...
    ThisTag.TagManager.DoMessage(1, pmtWarning,
      'Invalid tag: "%s"', [TagParameter]);
end;

procedure TPasItem.GroupBegin(ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  if MyOwner.DoGroup(gaStart, self, TagParameter) then
    ReplaceStr := ''
  else //error...
    ThisTag.TagManager.DoMessage(1, pmtWarning,
      'Invalid tag: "%s"', [TagParameter]);
end;

procedure TPasItem.GroupEnd(ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
begin
  if MyOwner.DoGroup(gaEnd, self, TagParameter) then
    ReplaceStr := ''
  else //error...
    ThisTag.TagManager.DoMessage(1, pmtWarning,
      'Invalid tag: "%s"', [TagParameter]);
end;

procedure TPasItem.RegisterTags(TagManager: TTagManager);
begin
  inherited RegisterTags(TagManager);
  TTag.Create(TagManager, 'deprecated',
    nil, {$ifdef FPC}@{$endif} HandleDeprecatedTag, []);
{$IFDEF old}
  TTag.Create(TagManager, 'exclude',
    nil, {$ifdef FPC}@{$endif} HandleExcludeTag, []);
{$ELSE}
{$ENDIF}
  TTag.Create(TagManager, 'group',
    nil, {$IFDEF FPC}@{$ENDIF} GroupTag,
    [toParameterRequired, toRecursiveTags, toAllowOtherTagsInsideByDefault, toAllowNormalTextInside, toFirstWordVerbatim]);
  TTag.Create(TagManager, 'groupbegin',
    nil, {$IFDEF FPC}@{$ENDIF} GroupBegin,
    [toParameterRequired, toRecursiveTags, toAllowOtherTagsInsideByDefault, toAllowNormalTextInside, toFirstWordVerbatim]);
  TTag.Create(TagManager, 'groupend',
    nil, {$IFDEF FPC}@{$ENDIF} GroupEnd,
    []);
end;

function TPasItem.FindName(const NameParts: TNameParts;
  index: integer): TPasItem;
begin
(* Find in all enclosing scopes.
*)
  if MyOwner <> nil then
    Result := MyOwner.FindName(NameParts, index)
  else
    Result := nil;
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

function TPasItem.GetMyObject: TPasCio;
begin
//owner should always be assigned, except for units!
  if assigned(FMyOwner) and (FMyOwner.Kind in CioTypes) then
    Result := FMyOwner as TPasCio
  else
    Result := nil;
end;

function TPasItem.FirstAncestorItem: TDescriptionItem;
begin
//this version returns the heritage item, if found during BuildLinks.
  Result := FHeritage.PasItem;
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

function TPasItem.GetOutputFileName: string;
begin
  Result := FOutputFileName;
  if Result = '' then
    Result := MyOwner.GetOutputFileName;
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
  aid: TPasItemAttribute absolute attr;
begin
  Result := aid in FAttributes;
end;

procedure TPasItem.SetAttributeFP(attr: integer; OnOff: boolean);
var
  aid: TPasItemAttribute absolute attr;
begin
  if OnOff then
    Include(FAttributes, aid)
  else
    Exclude(FAttributes, aid);
end;

procedure TPasItem.SetAttribute(attr: TPasItemAttribute; OnOff: boolean);
begin
  if OnOff then
    Include(FAttributes, attr)
  else
    Exclude(FAttributes, attr);
end;

procedure TPasItem.SetVisibility(attr: TItemVisibility);
begin
  FVisibility := attr;
  case attr of
  viShow, viImplicit, viUses, viHide: exit;
  viStrictProtected, viStrictPrivate:
    Include(FAttributes, SD_STRICT);
  end;
  Include(FAttributes, VisibilityAttributes[attr]);
end;

function TPasItem.BuildDescription: TDescriptionItem;
var
  anc: TPasItem;
  abst, det: string;
begin
(* Try to inherit missing descriptions.
  If either part (abstract/detailed) is empty, retrieve it from ancestor.
  The ancestor description may not have been built now!
*)
  Result := inherited BuildDescription; //returns FFullDescription only.
  if Result = nil then begin
    abst := '';
    det := '';
  end else begin
    abst := AbstractDescription;
    det := DetailedDescription;
  end;
//find descriptions
  anc := FHeritage.PasItem;
  while (anc <> nil) and ((abst = '') or (det = '')) do begin
    if abst = '' then
      abst := anc.AbstractDescription;
    if det = '' then
      det := anc.DetailedDescription;
    anc := anc.FHeritage.PasItem;
  end;
//exit if nothing found
  if ((abst = '') and (det = '')) then
    exit;
  Result := NeedDescription;
  AbstractDescription := abst;
  DetailedDescription := det;
end;

const
  DefaultSectionSortOrder: array[0..14] of TTranslationID = (
    trUnit, trDeclaration,
    trDescription,
    trUses, //unit only
    trHierarchy,  //classes only
    trParameters, trReturns, trExceptionsRaised, //procedures only
    trSeeAlso, //before overview, but after procedure lists
    trOverview, trDescriptions,
    trValues, //enums, constants?
    trAuthors, trCreated, trLastModified
  );
  MemberListSortOrder: array[0..7] of TTranslationID = (
  //units
    trClasses, trFunctionsAndProcedures, trTypes, trVariables, trConstants,
  //CIOs
    trFields, trMethods, trProperties
  );

procedure TPasItem.BuildSections;
begin
//add declaration - can not be empty
  AddNew(trDeclaration, dkNoList, FullDeclaration);
  inherited BuildSections;
//sort!
  SortByID(DefaultSectionSortOrder);
end;

{ TPasScope }

constructor TPasScope.Create(AOwner: TPasScope; AKind: TTokenType;
      const AName: string);
begin
  inherited;
//always create member list
  if FMembers = nil then
    FMembers := TPasItems.Create(True);
  FMemberLists := TDescriptionItem.Create('', '', trOverview, dkItemList);
  FHeritage := TDescriptionItem.Create('', '', trHierarchy, dkItemList);
    //overwrite in non-CIO classes
end;

destructor TPasScope.Destroy;
begin
  FreeAndNil(FMembers);
  FreeAndNil(FMemberLists);
  FreeAndNil(FHeritage);
  inherited;
end;

procedure TPasScope.Deserialize(const ASource: TStream);
begin
  inherited;
  Members.Deserialize(ASource);
end;

procedure TPasScope.Serialize(const ADestination: TStream);
begin
  inherited;
  Members.Serialize(ADestination);
end;

procedure TPasScope.AddMember(item: TPasItem);
begin
  Members.Add(item);
  item.FMyOwner := self;  // as TPasScope;
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

constructor TPasEnum.Create(AOwner: TPasScope; AKind: TTokenType;
  const AName: string);
begin
  inherited;
  FTID := trEnum;
end;

procedure TPasEnum.BuildLinks(AllUnits: TPasUnits;
  TheGenerator: TLinkGenerator);
var
  i: integer;
  item: TPasItem;
  lst: TDescriptionItem;
begin
(* Groups have not yet been specified!
  FMemberLists has been created in TPasScope constructor.
  We add the default member list.
*)
  FMemberLists.FTID := trValues; //assume: top level ignored!
  lst := FMemberLists.AddNew(trValues, dkPasItems); //members are not owned
  lst.FList.Assign(Members);
  for i := 0 to Members.Count - 1 do begin
    item := Members.PasItemAt[i];
    item.FFullLink := TheGenerator(item);
    //item.BuildSections; //here, unless BuildSections is overwritten
  end;
  //inherited BuildLinks(AllUnits, TheGenerator); //nop!?
end;

{$IFDEF old}
procedure TPasEnum.BuildMemberLists;
{$IFDEF old}
var
  i: integer;
  item: TPasItem;
{$ELSE}
var
  lst: TDescriptionItem;
{$ENDIF}
begin
(* Build overview from Members=Values.
  The default member list becomes a copy of Members, for later grouping.
  Called from BuildSections - propagate?
  MemberLists are required by ExpandDescription -> AddDescription
*)
{$IFDEF old}
  case FMemberLists.Count of
  0: exit;  //no members - should never happen
  1:  //simple list of Values
    FOverview := AddListDelegate(trValues, Members);
  else
    FOverview := AddListDelegate(FMemberLists);  //multiple lists
  end;
{$ELSE}
  //inherited BuildMemberLists;
(* Groups have not been specied yet!
  FMemberLists has been created in TPasScope constructor.
  We add the default member list.
*)
  FMemberLists.FTID := trValues; //assume: top level ignored!
  lst := FMemberLists.AddNew(trValues, dkPasItems); //members are not owned
{$IFDEF old}
  for i := 0 to Members.Count - 1 do begin
    item := Members.PasItemAt[i];
    lst.Add(item);
    item.BuildSections;
  end;
{$ELSE}
  lst.FList.Assign(Members);
  inherited BuildMemberLists;
  //FOverview.FTID := trValues; //if set to trOverview
{$ENDIF}
{$ENDIF}
end;
{$ELSE}
  //nothing to do here
{$ENDIF}

procedure TPasEnum.StoreValueTag(
  ThisTag: TTag; var ThisTagData: TObject;
  EnclosingTag: TTag; var EnclosingTagData: TObject;
  const TagParameter: string; var ReplaceStr: string);
var
  ValueName: String;
  ValueDesc: String;
  AValue: TPasItem;
begin
(* Special case: prefix enum member description with value specification.
  Deprecated!!!
  <name> <description>
  ==> <description> + <RawDescription>
*)
  ReplaceStr := '';
  ValueDesc := TagParameter;
  ValueName := ExtractFirstWord(ValueDesc);

  AValue := Members.FindName(ValueName);
  if Assigned(AValue) then begin
    //if AValue.ID = trNoTrans then AValue.FTID := trDummy;
    if AValue.RawDescription = '' then
      AValue.FRawDescription := ValueDesc
    else begin
    //concatenate value and description, but only for the first time!
    //separator?
      AValue.FRawDescription := ValueDesc + AValue.RawDescription;
      FreeAndNil(AValue.FRawdescriptionInfo);
    end;
  end else
    ThisTag.TagManager.DoMessage(1, pmtWarning,
      '@value tag specifies unknown member "%s"', [ValueName]);
end;

{$IFDEF groups}

function TPasEnum.DoGroup(how: eGroupAs; AItem: TPasItem;
  const AParam: string; lst: TDescriptionItem): boolean;
begin
(* Find value in default members list. Other scopes have to find the appropriate
  list from the item kind.
*)
  Result := inherited DoGroup(how, AItem, AParam, MemberLists.Items[0]);
end;
{$ELSE}
{$ENDIF}

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

procedure TPasEnum.Sort(const SortSettings: TSortSettings);
begin
//we should have distinct ssNever and ssAlways sorts!
  //do nothing!
end;

{ TBaseItems ----------------------------------------------------------------- }

constructor TBaseItems.Create(AOwnsObject: Boolean);
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
  FHash.DeleteKey(LowerCase(LObj.Name));
  inherited Delete(AIndex);
end;

function TBaseItems.FindName(const AName: string): TBaseItem;
begin
  Result := nil;
  if Length(AName) > 0 then begin
    //result := TObject(FHash.Items[LowerCase(AName)]) as TBaseItem;
    result := TObject(FHash.Objects[LowerCase(AName)]) as TBaseItem;
  end;
end;

function TBaseItems.Add(const AObject: TDescriptionItem): integer;
begin
  Result := inherited Add(AObject);
  //FHash.Items[LowerCase(AObject.Name)] := AObject;
  FHash.Objects[LowerCase(AObject.Name)] := AObject;
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
  Capacity := LCount;
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
    TSerializable.SerializeObject(Items[i], ADestination);
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
  if IsEmpty(c) then Exit;
  if Count + c.Count > Capacity then
    Capacity := (c.Count * 2) + (Capacity * 4 div 3); // Count + c.Count + 100;
  for i := 0 to c.Count - 1 do
    Add(c.GetPasItemAt(i));
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
    KEY_CLASS:
      Inc(c);
    KEY_INTERFACE, KEY_DISPINTERFACE:
      Inc(i);
    KEY_OBJECT:
      Inc(o);
    end;
end;

function TPasItems.GetPasItemAt(const AIndex: Integer): TPasItem;
begin
  Result := Items[AIndex].PasItem;
end;

function TPasItems.LastItem: TPasItem;
begin
  TObject(Result) := Last;
end;

function TPasItems.Text(const NameValueSeparator, ItemSeparator: string): string;
var
  i: Integer;
begin
  if Count > 0 then begin
    Result := PasItemAt[0].FullDeclaration;  //.Name + NameValueSeparator + PasItemAt[0].Value;
    for i := 1 to Count - 1 do
      Result := Result + ItemSeparator +
        //Items[i].Name + NameValueSeparator + Items[i].Value;
        PasItemAt[i].FullDeclaration;
  end else
    Result := '';
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
(* This is the general sort, called as FUnits.SortDeep.
  We delegate sorting to the items in the list.
  Check SortSettings for sorting Self and Items.
  Only CIOs may deserve sorting of their members.
*)
  if (SortKind = ssAlways) or (SortKind in SortSettings) then
    SortShallow;
  //if SortKind = ssCIOs then //We are Borg^w CIOs ;-)
  SortOnlyInsideItems(SortSettings);
end;


{ TPasCio -------------------------------------------------------------------- }

constructor TPasCio.Create(AOwner: TPasScope; AKind: TTokenType;
      const AName: string);
const
  owns = False;

  function NewList(tid: TTranslationID): TPasItems;
  begin
    Result := FMemberLists.AddNew(tid, dkPasItems).PasItems;
  end;

  function  GetTid(k: TTokenType): TTranslationID;
  begin
    case k of
    KEY_CLASS: Result := trClass;
    KEY_DISPINTERFACE: Result := trDispInterface;
    KEY_INTERFACE: Result := trInterface;
    KEY_OBJECT: Result := trObject;
    //KEY_RECORD: Result := CIO_RECORD; //could check for "packed"
    else
      Result := trRecord;
    end;
  end;

begin
  inherited;
  FTID := GetTid(AKind);
  case AKind of
  KEY_RECORD: //has no ancestors, but possibly methods?
    begin
      FFields := NewList(trFields);
    //fix sort for record fields
      FFields.SortKind := ssRecordFields;
    end;
  else //case
    FHeritage.FTID := trHierarchy;
    FFields := NewList(trFields);
    FMethods := NewList(trMethods);
    FProperties := NewList(trProperties);
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
  if ord(item.Visibility) = 0 then //just created?
    item.Visibility := CurVisibility;
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

function TPasCio.DoGroup(how: eGroupAs; AItem: TPasItem;
  const AParam: string; lst: TDescriptionItem): boolean;
var
  i: integer;
begin
(* Group item(s) from the appropriate item group.
*)
//safe way: find containing list
  for i := 0 to MemberLists.Count - 1 do begin
    lst := MemberLists.ItemAt(i);
    if (lst.FList <> nil) and (lst.FList.IndexOf(AItem) >= 0) then begin
      Result := inherited DoGroup(how, AItem, AParam, lst);
      exit;
    end;
  end;
//item not found in any memberlist
  Result := False;
end;

{$IFDEF oldsort}
procedure TPasCio.Sort(const SortSettings: TSortSettings);
begin
  inherited;

  if Fields <> nil then begin
    if MyType in CIORecordTypes then begin
      if ssRecordFields in SortSettings then begin
        (Fields.GetList as TPasItems).SortShallow;
      end;
    end else if ssNonRecordFields in SortSettings then begin
        (Fields.GetList as TPasItems).SortShallow;
    end;
  end;

  if (Methods <> nil) and (ssMethods in SortSettings) then begin
    (Methods.GetList as TPasItems).Sort( {$IFDEF FPC}@{$ENDIF} ComparePasMethods);
  end;

  if (Properties <> nil) and (ssProperties in SortSettings) then begin
    (Properties.GetList as TPasItems).SortShallow;
  end;
end;
{$ELSE}
{$ENDIF}

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
  @classname <-> [MyObject.]Name
  @inheritedclass <-> FirstAncestor.Name
  @inherited -> FindInAncestors().???
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
    Member.WriteRawDescription(MemberDesc);
  end else
    ThisTag.TagManager.DoMessage(1, pmtWarning,
      '@member tag specifies unknown member "%s".', [MemberName]);
end;

function TPasCio.FirstAncestor: TPasCio;
var
  item: TDescriptionItem;
begin
  item := FirstAncestorItem;
  if item = nil then
    Result := nil
  else
    Result := item.PasItem as TPasCio;
end;

function TPasCio.FirstAncestorItem: TDescriptionItem;
begin
//this version returns the first item (base class) from the heritage list.
  if IsEmpty(FAncestors) then
    Result := nil
  else
    Result := FAncestors.ItemAt(0);
end;

function TPasCio.FirstAncestorName: string;
var
  item: TDescriptionItem;
begin
  item := FirstAncestorItem;
  if item <> nil then
    Result := item.Name
  else
    Result := '';
end;

function TPasCio.FindItemInAncestors(const ItemName: string): TPasItem;
var
  item: TDescriptionItem;
begin
//ancestor also searches in it's ancestor(s) (auto recursion)
  item := FirstAncestorItem;
  if item.PasItem = nil then
    Result := nil
  else
    Result := item.PasItem.FindItem(ItemName).PasItem;
end;

procedure TPasCio.BuildLinks(AllUnits: TPasUnits; TheGenerator: TLinkGenerator);
var
  i: integer;
  item: TDescriptionItem;
  pi: TPasItem;

  procedure SearchAncestor;
  var
    iu: integer;
    found: TDescriptionItem;
  begin
    for iu := 0 to AllUnits.Count - 1 do begin
    //also search in own unit!
      found := AllUnits.UnitAt[iu].CIOs.FindName(item.Name);
      if found <> nil then begin
        item.PasItem := found.PasItem;
        break;
      end;
    end;
  end;

begin //BuildLinks
(* Resolve references to all ancestors.
  Exclude invisible members.
  Sort members?
  Assign OutputFileName.
  Expose member lists (or remove empty lists)
*)
//do we already have a valid link???
  FOutputFileName := FFullLink;
//resolve ancestry
  for i := 0 to Ancestors.Count - 1 do begin
    item := Ancestors.ItemAt(i);
    if item.PasItem = nil then
      SearchAncestor;
  end;
{$IFDEF new}
//exclude invisible members - in member lists only!
  for i := FMemberLists.Count - 1 downto 0 do begin
    lst := FMemberLists.Items[i];
    for m := lst.Count - 1 downto 0 do begin
      pi := lst.PasItemAt(m);
      pi.FFullLink := TheGenerator(pi);
      if not (pi.Visibility in ShowVisibilities) then
        lst.FList.Delete(m);
    end;
  end;
{$ELSE}
//link members
  for i := 0 to Members.Count - 1 do begin
    pi := Members.PasItemAt[i];
    pi.FFullLink := TheGenerator(pi);
    pi.FHeritage := FindItemInAncestors(pi.Name); //problem with overloaded ancestors!
  end;
{$ENDIF}
//add non-empty member lists to descriptions
  inherited BuildLinks(AllUnits, TheGenerator);
end;

procedure TPasCio.BuildSections;
var
  desc, anc: TDescriptionItem;
begin
{$IFDEF old}
  desc := AddNew(trUnit, dkDelegate, MyUnit.Name); //description???
  desc.PasItem := MyOwner;
{$ELSE}
  //automatic by generator, for every item in a separate file
{$ENDIF}
//hierarchy
  if not IsEmpty(Ancestors) then begin
    desc := AddNew(trHierarchy, dkItemList);
    desc.FList.OwnsObjects := False; //new list type: NoOwnItems?
    anc := FirstAncestorItem;
    while anc <> nil do begin
      //anc := anc.Items[0];
    //add ancestors in reverse order
      desc.FList.Insert(0, anc);
      if anc.PasItem is TPasCio then
        anc := TPasCio(anc.PasItem).FirstAncestorItem
      else
        break;
    end;
  //add self as last item
    desc.AddNew(ID, dkNoList, Name, Value);
  end;
  inherited BuildSections; //build overview
{$IFDEF old}
//sort
  SortByID([trUnit, trDeclaration, trDescription, trHierarchy,
    trSeeAlso, //???
    trOverview, trDescriptions, trAuthors, trCreated, trLastModified]);
{$ELSE}
{$ENDIF}
end;

{ TPasUnit ------------------------------------------------------------------- }

constructor TPasUnit.Create(AOwner: TPasScope; AKind: TTokenType;
  const AName: string);
const
  owns = False;
begin
  inherited;
//description
//uses
  //FUsesUnits := FMemberLists.AddNew(trUses, dkItemList);
  FHeritage.FTID := trUses;
  FHeritage.FList.SortKind := ssUsesClauses;
    //only add to descriptions if ShowUses!
//overview
  FCIOs := FMemberLists.AddNew(trClasses, dkPasItems).PasItems;
  FFuncsProcs := FMemberLists.AddNew(trFunctionsAndProcedures, dkPasItems).PasItems;
  FTypes := FMemberLists.AddNew(trTypes, dkPasItems).PasItems;
  FVariables := FMemberLists.AddNew(trVariables, dkPasItems).PasItems;
  FConstants := FMemberLists.AddNew(trConstants, dkPasItems).PasItems;
//authors
//dates: Created, LastMod
//footer
end;

procedure TPasUnit.Deserialize(const ASource: TStream);
begin
  inherited;
{$IFDEF new}
  FUsesUnits.LoadFromBinaryStream(ASource);
{$ELSE}
{$ENDIF}
end;

procedure TPasUnit.Serialize(const ADestination: TStream);
begin
  inherited;
{$IFDEF new}
  FUsesUnits.SaveToBinaryStream(ADestination);
{$ELSE}
{$ENDIF}
//if lists are created by parser, also save all other items!
//means: create lists when deserialized! (need all fields)
end;

procedure TPasUnit.Sort(const SortSettings: TSortSettings);
begin
  inherited; //sort standard member lists
//special case, of non-PasItems
  if (UsesUnits <> nil) and (ssUsesClauses in SortSettings) then
    UsesUnits.Sort(SortSettings);
end;

function TPasUnit.FindItemInAncestors(const ItemName: string): TPasItem;
var
  i: integer;
  uitem: TPasUnit;
begin
  for i := 0 to UsesUnits.Count - 1 do begin
    uitem := UsesUnits.Items[i].PasItem as TPasUnit;
    if uitem <> nil then begin
      Result := uitem.FindItem(ItemName) as TPasItem;
      if Result <> nil then
        exit;
    end;
  end;
//if nothing searched and found
  Result := nil;
end;

procedure TPasUnit.BuildLinks(AllUnits: TPasUnits;
  TheGenerator: TLinkGenerator);
var
  i: integer;
  item: TPasItem;
  u: TDescriptionItem;
begin
(* primary call, distribute to all scopes
*)
  FFullLink := TheGenerator(self);
  FOutputFileName := FFullLink;
//used units
  if not IsEmpty(UsesUnits) then begin
    for i := 0 to UsesUnits.Count - 1 do begin
      u := UsesUnits.ItemAt(i);
      if u.PasItem = nil then
        u.PasItem := AllUnits.FindName(u.Name);
    end;
  //add description item
    AddListDelegate(trUses, UsesUnits.FList);
  end;
//if CIOs are merged with other types!?
  for i := 0 to Members.Count - 1 do begin
    item := Members.PasItemAt[i];
    item.FFullLink := TheGenerator(item);
    if item is TPasScope then //has its own member list
      item.BuildLinks(AllUnits, TheGenerator);
  end;
//build sections
(* hints
     trDeprecated, trPlatformSpecific, trLibrarySpecific
   AbstractDescription
   DetailedDescription
Uses
*)
//build overview?
  inherited BuildLinks(AllUnits, TheGenerator);
end;

procedure TPasUnit.BuildSections;
begin
(* Entry point, called from PasUnits.
  Build all sections, recursive for all scopes.
*)
  if Self.ToBeExcluded then
    exit; //should be skipped earlier in the call chain!

//add Unit special sections - already done!?
  //if not IsEmpty(UsesUnits) then AddListDelegate(UsesUnits);
//build general sections and recurse.
  inherited BuildSections;
{$IFDEF old}
//sort all lists
  SortByID([trDeclaration, trDescription, trSeeAlso, trUses,
    trOverview, trDescriptions,
    trAuthors, trCreated, trLastModified]);
{$ELSE}
{$ENDIF}
//special sort of Unit member list.
  FMemberLists.SortByID([trClasses, trFunctionsAndProcedures, trTypes,
    trConstants, trVariables]);
end;

procedure TPasUnit.AddMember(item: TPasItem);
begin
  inherited;  //add to Members
  case item.Kind of
  KEY_CONST:  FConstants.Add(item);
  KEY_TYPE:   FTypes.Add(item);
  KEY_VAR:    FVariables.Add(item);
  KEY_UNIT:   UsesUnits.AddNew(trUnit, dkDelegate, item.Name).PasItem := item;
  else
    if item.Kind in CioTypes then
      FCIOs.Add(item)
    else
      FFuncsProcs.Add(item);
  end;
end;

function TPasUnit.DoGroup(how: eGroupAs; AItem: TPasItem;
  const AParam: string; lst: TDescriptionItem): boolean;
var
  i: integer;
begin
(* Group item(s) from the appropriate item group.
*)
//safe way: find containing list
  for i := 0 to MemberLists.Count - 1 do begin
    lst := MemberLists.ItemAt(i);
    if (lst.FList <> nil) and (lst.FList.IndexOf(AItem) >= 0) then begin
      Result := inherited DoGroup(how, AItem, AParam, lst);
      exit;
    end;
  end;
//item not found in any memberlist
  Result := False;
end;

function TPasUnit.FindFieldMethodProperty(const S1, S2: string): TPasItem;
begin
  Result := nil;
  if CIOs = nil then Exit;
  Result := CIOs.FindName(S1).PasItem;
  if assigned(Result) then
    Result := Result.FindItem(S2).PasItem;
end;

function TPasUnit.FileNewerThanCache(const FileName: string): boolean;
begin
  Result := WasDeserialized and FileExists(FileName) and
    (CacheDateTime < FileDateToDateTime(FileAge(FileName)));
end;

function TPasUnit.BasePath: string;
begin
  Result := ExtractFilePath(ExpandFileName(SourceFileName));
end;

{ TPasUnits ------------------------------------------------------------------ }

procedure TPasUnits.BuildSections;
var
  i: integer;
begin
  for i := 0 to Count - 1 do begin
    UnitAt[i].BuildSections;
  end;
end;

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
  SetItem(AIndex, Value);
end;

{ TPasMethod ----------------------------------------------------------------- }

{$IFDEF old}
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
{$ELSE}
{$ENDIF}

{$IFDEF old}
procedure TPasMethod.BuildMemberLists;
begin
  FMemberLists.FTID := trSubroutine;
  inherited BuildMemberLists; //filter, create overview
  if FOverview = nil then
    exit;
  //FOverview.FTID := trSubroutine; //unless done before
//todo: should sort Overview!?
  FMemberLists.SortByID([trParameters, trReturns, trExceptionsRaised]);
end;

procedure TPasMethod.BuildSections;
begin
  inherited BuildSections;
{$IFDEF old}
//sort
  SortByID([trDeclaration, trDescription, trOverview, trSeeAlso]);
{$ELSE}
  //SeeAlso should be sorted last!?
{$ENDIF}
end;
{$ELSE}
{$ENDIF}

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
{$IFDEF old}
  o: TDescriptionItem;
{$ELSE}
{$ENDIF}
begin
  if TagParameter = '' then exit;
  if FRaises = nil then begin
{$IFDEF old}
    o := NeedOverview;
    FRaises := o.AddNew(trExceptionsRaised, dkItemList);
{$ELSE}
    FRaises := AddNew(trExceptionsRaised, dkItemList);
{$ENDIF}
  end;
  item := FRaises.AddExtractFirstWord(trException, TagParameter);
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
{$IFDEF old}
  o: TDescriptionItem;
{$ELSE}
{$ENDIF}
begin
  if TagParameter = '' then exit;
  if FParams = nil then begin
  {$IFDEF old}
    o := NeedOverview;
    FParams := o.AddNew(trParameters, dkItemList);
  {$ELSE}
    FParams := AddNew(trParameters, dkItemList);
  {$ENDIF}
  end;
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
{$IFDEF old}
var
  o: TDescriptionItem;
{$ELSE}
{$ENDIF}
begin
  if TagParameter = '' then exit;
//Name could hold the function type!
  if FReturns = nil then begin
  {$IFDEF old}
    o := NeedOverview;
    FReturns := o.AddString(trReturns, TagParameter);
  {$ELSE}
    FReturns := AddString(trReturns, TagParameter);
  {$ENDIF}
  end else begin
    ThisTag.TagManager.DoMessage(1, pmtWarning, 'override Returns %s by %s',
      [FReturns.Name, TagParameter]);
    FReturns.Name := TagParameter;
  end;
  ReplaceStr := '';
end;

function TPasMethod.HasMethodOptionalInfo: boolean;
begin
  Result := not IsEmpty(FList);
end;

{$IFDEF old}
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
{$ELSE}
{$ENDIF}

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
{$IFDEF DetailedProps}
  FIndexDecl := LoadStringFromStream(ASource);
  FStoredID := LoadStringFromStream(ASource);
  FDefaultID := LoadStringFromStream(ASource);
  FPropType := LoadStringFromStream(ASource);
{$ELSE}
{$ENDIF}
  FReader := LoadStringFromStream(ASource);
  FWriter := LoadStringFromStream(ASource);
end;

procedure TPasProperty.Serialize(const ADestination: TStream);
begin
  inherited;
{$IFDEF DetailedProps}
  SaveStringToStream(FIndexDecl, ADestination);
  SaveStringToStream(FStoredID, ADestination);
  SaveStringToStream(FDefaultID, ADestination);
  SaveStringToStream(FPropType, ADestination);
{$ELSE}
{$ENDIF}
  SaveStringToStream(FReader, ADestination);
  SaveStringToStream(FWriter, ADestination);
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

{$IFDEF old}
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
{$ELSE}
{$ENDIF}

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

procedure TExternalItem.SetOutputFileName(const AValue: string);
begin
  FOutputFileName := AValue;
end;

function TExternalItem.BasePath: string;
begin
  Result := ExtractFilePath(ExpandFileName(SourceFileName));
end;

{ global things ------------------------------------------------------------ }

function VisToStr(const Vis: TVisibility): string;
begin
  result := StringReplace(VisibilityStr[Vis], ' ', '', [rfReplaceAll]);
end;

function VisibilitiesToStr(const Visibilities: TVisibilities): string;
var Vis: TVisibility;
begin
  Result := '';
  for Vis := Low(Vis) to High(Vis) do begin
    if Vis in Visibilities then
      Result := Result + VisToStr(Vis) + ',';
  end;
  if Result <> '' then
    SetLength(Result, Length(Result) - 1); //strip trailing ","
end;

procedure TPasScope.Sort(const SortSettings: TSortSettings);
var
  i: integer;
  lists: TDescriptionItem;
  lst: TPasItems;
begin
(* Can be: Unit, Cio, but also Type, Method...
  We have a list of member lists, which may deserve sorting.
  MemberLists: TDescriptionItem .List: TDescriptionList - special sort
    MemberList: TDescriptionItem .List: TPasItems - conditional sort
      Member: TPasCio - deep sort!
*)
  lists := FMemberLists;
  if IsEmpty(lists) then
    exit; //should never happen
  lists.SortByID(MemberListSortOrder);
  for i := 0 to lists.Count - 1 do begin
    lst := lists.Items[i].PasItems; //can be TPasCios - recurse
  //even if a list is not sorted, it's members may deserve sorting!
    if (lst <> nil) then
      lst.SortDeep(SortSettings);
  end;
end;

procedure TPasScope.BuildSections;
begin
  BuildMemberLists;
  inherited BuildSections;
end;

procedure TPasScope.BuildMemberLists;
var
  i, j: integer;
  o, ml: TDescriptionItem;
  ps: TPasItem;
begin
(* Build overview from member lists.
  If not already done, the MemberLists can be built in the overwritten methods,
    prior to calling inherited.
  MemberList must contain PasItem lists or list delegates (two levels).
  This procedure
    deletes excluded items,
    calls BuildSections for every other member,
    finally filters empty lists.
  Sorting must occur elsewhere, depending on the item kind (unit...).

  Also remove excluded items (FExclude).
*)
  o := NeedOverview;
//for now, we add all non-empty member lists
  for i := 0 to FMemberLists.Count - 1 do begin
    ml := FMemberLists.ItemAt(i);
    if not IsEmpty(ml) then begin
    //top-down, because items can be excluded!
      for j := ml.Count - 1 downto 0 do begin
        ps := ml.PasItemAt(j);
        //assert(ps <> nil, 'invalid member');
        if ps.FExclude then
          ml.FList.Delete(j)
        else
          ps.BuildSections;
      end;
      if not IsEmpty(ml) then
        o.AddListDelegate(ml);
    end;
  end;
  if o.Count <= 0 then begin
  //remove overview item
    i := FList.IndexOf(o);
    if i >= 0 then begin
      FList.Delete(i); //deletes owned overview
      FOverview := nil;
    end;
  end;
end;

function TPasScope.NeedOverview: TDescriptionItem;
begin
//create Overview, if not already done
  if FOverview = nil then begin
    //FOverview := AddNew(trOverview, dkItemList);
    FOverview := AddNew(FMemberLists.ID, dkItemList);
  end;
  Result := FOverview;
end;

function TPasScope.DoGroup(how: eGroupAs; AItem: TPasItem;
  const AParam: string; lst: TDescriptionItem): boolean;
var
  m: integer;
  //lst, item,
  grp: TDescriptionItem;
  n, d: string;
  iFirst, iLast: integer;
begin
(* Since the tags can be expanded in any (fwd/back) order, we'll have to track
  the groupstart/groupend item. When both are assigned, the range is moved.

  We have already built the default lists in MemberLists, while Overview still is nil.

  lst has to be determined by the overridden methods.
*)
  //lst := MemberLists.Items[0];
  if lst = nil then begin
    Result := False;
    exit;
  end;

  m := lst.FList.IndexOf(AItem);
  Result := m >= 0;
  if not Result then begin
  {$IFDEF new}
  //error...
    ThisTag.TagManager.DoMessage(1, pmtWarning,
      '@group tag for unknown or already grouped member "%s"', [n]);
  {$ELSE}
  {$ENDIF}
  end else begin //make general method?
    if how = gaEnd then
      grp := nil  //must be specified with gaStart
    else begin
    //determine the group - optional (ignored) for gaEnd
    //split group name from description
      ExtractFirstWord(AParam, n, d);
      grp := MemberLists.Find(n);
      (* Finding the group by name allows for mixed-item lists,
        e.g. for a property with getters and setters.
        Consequently group names must be unique throughout a scope!
        The group ID of a new group should be taken from where???
        Best: do NOT use the group ID in the section header!
      *)
    //find group in Overview
      if grp = nil then
        grp := MemberLists.AddNew(lst.FTID, dkPasItems, n, d)
      else if grp.Value = '' then
        grp.Value := d //put description
      else if d <> '' then //append description - deprecated!
        grp.Value := grp.Value + LineEnding + LineEnding + d;
    end;
  //move immediately?
    case how of
    gaSingle:
      begin //move value
        lst.FList.Delete(m);
        grp.Add(AItem);
        exit;
      end;
    gaStart:
      begin
        FGroup := grp;
        gFirst := AItem;
        if gLast = nil then begin
          gScope := self;
          exit; //move at gaEnd
        end;
        iFirst := m;
        iLast := lst.FList.IndexOf(gLast);
      end;
    gaEnd:
      begin
        gLast := AItem;
        if gFirst = nil then begin
          gScope := self;
          exit; //move at gaStart
        end;
        grp := FGroup;
        iFirst := lst.FList.IndexOf(gFirst);
        iLast := m;
      end;
    else //keep compiler happy
      iFirst := -1;
      iLast := -1;
      Result := False;
    end; //case
  //move range of items
  //prevent iFirst=-1 (not found)
    if (gScope <> self) or (grp = nil) or (cardinal(iFirst) > cardinal(iLast)) then
      Result := False //no group assigned, or invalid range
    else begin //move range of items
      while iLast >= iFirst do begin
        AItem := lst.PasItemAt(iFirst);
        if AItem <> nil then begin
        //prevent moving non-PasItems
          lst.FList.Delete(iFirst);
          grp.Add(AItem);
          dec(iLast);
        end else
          inc(iFirst); //skip this item
      end;
    end;
  //always clear range, even in case of an error
    FGroup := nil;
    gFirst := nil;
    gLast := nil;
    gScope := nil;
  end;
end;

function TPasScope.PasScope: TPasScope;
begin
  Result := Self;
end;

function TPasScope.ShowVisibility: boolean;
begin
  Result := FKind in CIOClassTypes;
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

function IsEmpty(item: TDescriptionItem): boolean;
begin
  Result := (item = nil) or (item.FList = nil) or (item.FList.Count <= 0);
end;

function IsEmpty(item: TDescriptionList): boolean;
begin
  Result := (item = nil) or (item.Count <= 0);
end;

constructor TDescriptionItem.Create(const AName, AValue: string;
  tid: TTranslationID; AListKind: eDescriptionKind);
begin
  inherited Create; //(AName, AValue);
  Name := AName;
  Value := AValue;
  FTID := tid;
  case AListKind of
  dkPasItems: FList := TPasItems.Create(False);
  //dkDelegates: FList := TDescriptionList.Create(True);
  dkListDelegate: fExternalList := True;
  dkItemList: FList := TDescriptionList.Create(True);
  end;
//set list sort kind
  if FList <> nil then begin
    case tid of
    trCio:
      FList.SortKind := ssCIOs;
    trConstants: 
      FList.SortKind := ssConstants;
    trFunctionsAndProcedures: 
      FList.SortKind := ssFuncsProcs;
    trTypes:
      FList.SortKind := ssTypes;
    trVariables:
      FList.SortKind := ssVariables;
    trUses:
      FList.SortKind := ssUsesClauses;
    trFields: //distinguish non/record fields in TPasCio constructor
      FList.SortKind := ssNonRecordFields; // ssRecordFields;
    trMethods:
      FList.SortKind := ssMethods;
    trProperties:
      FList.SortKind := ssProperties;
    //trEvents: SortKind := ssEvents; //trEvents not yet declared!
    end;
  end;
end;

destructor TDescriptionItem.Destroy;
begin
  if fExternalList then
    FList := nil  //do NOT destroy
  else
    FreeAndNil(FList);
  inherited;
end;

{$IFDEF new}

function TDescriptionItem.AddStrings(tid: TTranslationID;
  lst: TStrings): TDescriptionItem;
begin
...
end;

procedure TDescriptionItem.LoadFromBinaryStream(Stream: TStream);
var
  i: Integer;
  //aid: TTranslationID absolute i;
  akind: eDescriptionKind absolute i;
begin
  FTID := TTranslationID(TSerializable.LoadIntegerFromStream(Stream));
  i := TSerializable.LoadIntegerFromStream(Stream);
  assert(kind = akind, 'clobbered stream');
  Name := TSerializable.LoadStringFromStream(Stream);
  Value := TSerializable.LoadStringFromStream(Stream);
//more in derived (lists)
end;

function TDescriptionItem.AddFromStream(Stream: TStream): TDescriptionItem;
var
  i, k: integer;
  aid: TTranslationID absolute i;
  akind: eDescriptionKind absolute k;
  aname, avalue: string;
begin
  assert(kind = dkItemList, 'can only deserialize item lists');
//see: SaveToBinaryStream
  i := TSerializable.LoadIntegerFromStream(Stream);
  k := TSerializable.LoadIntegerFromStream(Stream);
  aname := TSerializable.LoadStringFromStream(Stream);
  avalue := TSerializable.LoadStringFromStream(Stream);
  Result := AddNew(aid, akind, aname, avalue);
{$IFDEF recursive}
  if akind = dkItemList then begin
    //list part
    k := TSerializable.LoadIntegerFromStream(Stream);
    //set capacity - how?
    for i := 0 to k-1 do begin
      Result.AddFromStream(Stream);
    end;
  end;
{$ELSE}
  //we assume flat lists???
{$ENDIF}
end;

procedure TDescriptionItem.SaveToBinaryStream(Stream: TStream);
var
  i: Integer;
  sl: TStrings;
  //il: TObjectVector;
  item: TDescriptionItem;
begin
(* Needed for ancestors, uses.
  Do not save objects, i.e. no TPasItems in dkPasItems.
*)
  TSerializable.SaveIntegerToStream(ord(ID), Stream);
  TSerializable.SaveIntegerToStream(ord(kind), Stream);
  TSerializable.SaveStringToStream(Name, Stream);
  TSerializable.SaveStringToStream(Value, Stream);
{$IFDEF old}
  case self.kind of
  dkText:
    TSerializable.SaveStringToStream(Description, Stream);
  dkNameDesc:
    begin
      TSerializable.SaveStringToStream(Name, Stream);
      TSerializable.SaveStringToStream(Value, Stream);
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
{$ELSE}
{$ENDIF}
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
  if n <> '' then
    Result := AddNew(tid, dkNoList, n, desc)
  else
    Result := nil;  //flag error
end;

function TDescriptionItem.GetCount: integer;
begin
  if FList <> nil then
    Result := FList.Count
  else
    Result := 0;  //or -1, to signal NoList?
end;

function TDescriptionItem.GetItemFPC(intID: integer): TDescriptionItem;
var
  tid: TTranslationID absolute intID;
begin
  Result := FindID(tid);
end;

function TDescriptionItem.GetString(index: integer): string;
begin
//include range check
  if (FList <> nil) and (cardinal(index) < cardinal(FList.Count)) then
    Result := FList.Items[index].Name
  else
    Result := '';
end;

function TDescriptionItem.ItemAt(index: integer): TDescriptionItem;
begin
//override in lists
  if FList <> nil then
    Result := FList.GetItem(index) as TDescriptionItem
  else
    Result := nil;
end;

function TDescriptionItem.FindID(tid: TTranslationID): TDescriptionItem;
var
  i: integer;
begin
  //FList.IndexOfID(tid)?
  i := IndexOfID(tid);
  if i >= 0 then
    Result := ItemAt(i)
  else
    Result := nil;
end;

function TDescriptionItem.AsPasItem: TPasItem;
begin
//protect against Nil!
  Result := nil;
  if Self = nil then exit;
  if Self is TPasItem then
    Result := TPasItem(Self)
  else if Self is TPasDelegate then
    Result := TPasDelegate(Self).MyItem;
end;

function TDescriptionItem.PasItemAt(index: integer): TPasItem;
begin
  Result := FList.ItemAt(index).PasItem;
end;

procedure TDescriptionItem.SetPasItem(AItem: TPasItem);
begin
//overwrite in TPasDelegate!
  assert(AItem = self, 'illegal SetPasItem');
end;

function TDescriptionItem.Add(AItem: TDescriptionItem): integer;
begin
  if AItem = nil then
    Result := -1
  else begin
    if FList = nil then
      FList := TDescriptionList.Create(True);
    Result := FList.Add(AItem);
  end;
end;

function TDescriptionItem.AddNew(tid: TTranslationID; AKind: eDescriptionKind;
  const AName, AValue: string): TDescriptionItem;
begin
(* Prevent dupes of lists. Simple items typically need no checks.
  !Authors list must not have duplicate entries!
  Find either by id or by name?
*)
  assert(Self <> nil, 'bad call');
//we (the container) need a list! Specialized lists of TPasItems...?
  if FList = nil then
    FList := TDescriptionList.Create(True)
  else begin
  //find entry in list, in detail when a contained list is requested!
    case AKind of
    //dkNoList, //no checks required
    dkUniqueString,
    dkDelegate: //check for unique string=AName
    { Unique strings must reside in Name, for dupe search! }
      Result := Find(AName);
    //dkDelegates,
    dkListDelegate,
    dkPasItems:
      if AName = '' then
        Result := FindID(tid) //inappropriate for groups
      else begin //find group list - groups have unique names
        Result := Find(AName);
      end;
    dkItemList: //check for unique ID
      if tid = trNoTrans then
        Result := Find(AName)
      else
        Result := FindID(tid);
    else //always create new entry
      Result := nil;
    end;
  //check
    if Result <> nil then begin
    //update item(???)
      if (Result.Name = '') and (AName <> '') then
        Result.Name := AName;
      if (Result.Value = '') and (AValue <> '') then
        Result.Value := AValue;
      exit;
    end;  //else really add new
  end;
//create item, polymorphic!
  case AKind of
  dkNoList, dkUniqueString:
    Result := TDescriptionItem.Create(AName, AValue, tid, AKind);
  dkDelegate: //create special item, used in @param, @member
    Result := TPasDelegate.Create(AName, AValue, tid, AKind);
  dkListDelegate,
  dkPasItems,
  dkItemList:
    Result := TDescriptionItem.Create(AName, AValue, tid, AKind);
  else
    Result := nil;  //unhandled!
  end;
//if created, add new item
  if Result <> nil then begin
    FList.Add(Result);
  end;
end;

function TDescriptionItem.AddListDelegate(
  lst: TDescriptionItem): TDescriptionItem;
begin
(* We exclude NIL items, but not empty lists.
*)
  if lst = nil then
    Result := nil
  else begin
    //Result := AddListDelegate(lst.ID, lst.FList);
    Result := AddNew(lst.ID, dkListDelegate, lst.Name, lst.Value);
    if Result <> nil then begin
      assert(Result.FList = nil, 'bad Add mode');
      Result.FList := lst.FList;
    end;
  end;
end;

function TDescriptionItem.AddListDelegate(tid: TTranslationID;
  lst: TDescriptionList): TDescriptionItem;
begin
  if IsEmpty(lst) then
    Result := nil
  else begin
    Result := AddNew(tid, dkListDelegate);
    //Result.fExternalList := True;
    Result.FList := lst;
  end;
end;

function TDescriptionItem.AddString(tid: TTranslationID;
  const s: string): TDescriptionItem;
begin
  Result := AddNew(tid, dkNoList, s);
end;

function TDescriptionItem.AddUniqueString(tid: TTranslationID;
  const s: string): TDescriptionItem;
begin
//for unique check, the string must be stored in Name!
  Result := AddNew(tid, dkUniqueString, s);
end;

function TDescriptionItem.AddToStrings(tblTid, itemTid: TTranslationID;
  const s: string): integer;
var
  lst: TDescriptionItem;
  item: TDescriptionItem;
begin
//Add unique string to one of our lists.
//usage: Authors (only?) SeeAlso?
  lst := AddNew(tblTid, dkItemList); //create if required
  Result := lst.IndexOf(s);
  if Result >= 0 then
    exit; //string already exists
//add new, always!
  item := TDescriptionItem.Create(s, '', itemTid);
  Result := lst.Add(item);
end;

function TDescriptionItem.Find(const AName: string): TDescriptionItem;
begin
  if FList <> nil then
    Result := FList.Find(AName) //may use hash...
  else
    Result := nil
end;

function TDescriptionItem.Text(const NameValueSeparator,
  ItemSeparator: string): string;
var
  i: Integer;
begin
(* Build string from Name and Value.
  Append from list only if ItemSeparator is not empty
*)
  if (ItemSeparator <> '') and (Count > 0) then begin
  //list version, ignore Self.Name and .Value
    Result := Items[0].Text(NameValueSeparator, ItemSeparator);
    for i := 1 to Count - 1 do
      Result := Result + ItemSeparator + Items[i].Text(NameValueSeparator, ItemSeparator);
  end else begin //simple item
    if Name = '' then
      Result := Value
    else if Value = '' then
      Result := Name
    else
      Result := Name + NameValueSeparator + Value;
  end;
end;

function TDescriptionItem.IndexOf(const AName: string): integer;
begin
  if FList <> nil then
    Result := FList.IndexOfName(AName)
  else
    Result := -1;
end;

function TDescriptionItem.IndexOfID(tid: TTranslationID): integer;
begin
  if FList <> nil then
    Result := FList.IndexOfID(tid)
  else
    Result := -1;
end;

function TDescriptionItem.PasItems: TPasItems;
begin
  if FList is TPasItems then
    Result := TPasItems(FList)
  else
    Result := nil;
end;

procedure TDescriptionItem.Sort(const SortSettings: TSortSettings);
begin
(* process MemberLists. sort: Fields-Methods-Properties?
[All...: class(TPasItems): always sort.]
[Units: TPasUnits -> Unit: always sort]
  Unit: TPasUnit -> MemberLists: never/special sort, UsedUnits!
  Cio: TPasCio -> MemberLists, Ancestors(never sort)
    MemberLists: TDescriptionItem -> SpcItems:class(TPasItems) -> Member
      Member can be Cio:TPasCio=class(TPasScope)
*)
  //here: nop, override in PasItem classes?
  if (FList <> nil) and (FList.SortKind in SortSettings) then
    FList.Sort({$IFDEF fpc}@{$ENDIF} CompareDescriptionItemsByName);
end;

{$IFDEF old}
function TDescriptionItem.GetRawDescription: string;
begin
  Result := Value;
end;
{$ELSE}
  //replaced by Caption=Name
function TDescriptionItem.GetRawDescription: string;
begin
  Result := '';
end;
{$ENDIF}

procedure TDescriptionItem.SortByID(
  const weights: array of TTranslationID);
var
  it: TTranslationID;
  iw, w: integer;
begin
  if GetCount < 2 then
    exit; //no sort required
//build weight table
  w := Length(weights);
  //FillChar(ItemWeights[0], w*sizeof(ItemWeights[0]), 0);
  FillChar(ItemWeights, sizeof(ItemWeights), 0); //-1?
  for iw := 0 to w-1 do
    ItemWeights[weights[iw]] := iw + 1;
  for it := low(it) to high(it) do begin
    if ItemWeights[it] <= 0 then begin
      inc(w);
      ItemWeights[it] := w;
    end;
  end;
  FList.Sort({$IFDEF fpc}@{$ENDIF} CompareWeight);
end;

{ TDescriptionList }

function TDescriptionList.Add(const AObject: TDescriptionItem): integer;
begin
//here: simply add to the list
  Result := inherited Add(AObject);
end;

procedure TDescriptionList.AddItems(lst: TDescriptionItem);
begin
  InsertItems(lst.FList);
end;

procedure TDescriptionList.InsertItems(lst: TDescriptionList);
var
  i: integer;
begin
  if lst = nil then
    exit; //shit happens, really :-(
  for i := 0 to lst.Count - 1 do begin
    Add(lst.ItemAt(i));
  end;
end;

function TDescriptionList.Find(const AName: string): TDescriptionItem;
var
  i: integer;
begin
  i := IndexOfName(AName);
  if i < 0 then
    Result := nil
  else
    Result := ItemAt(i);
end;

function TDescriptionList.ItemAt(index: integer): TDescriptionItem;
begin
//this typecast MUST work.
  Result := inherited GetItem(index) as TDescriptionItem;
end;

function TDescriptionList.IndexOfName(const AName: string): integer;
var
  i: integer;
  item: TDescriptionItem;
begin
  for i := 0 to Count - 1 do begin
    item := ItemAt(i); //optimize?
    //item := TObjectVector(FList).Items[i] as TDescriptionItem;
    if CompareText(AName, item.Name) = 0 then begin
      Result := i;
      exit;
    end;
  end;
//not found
  Result := -1;
end;

function TDescriptionList.IndexOfID(tid: TTranslationID): integer;
var
  i: integer;
  item: TDescriptionItem;
begin
  for i := 0 to Count - 1 do begin
    item := ItemAt(i); //optimize?
    //item := TObjectVector(FList).Items[i] as TDescriptionItem;
    if item.ID = tid then begin
      Result := i;
      exit;
    end;
  end;
//not found
  Result := -1;
end;

{$IFDEF new}
procedure TDescriptionList.LoadFromBinaryStream(Stream: TStream);
var
  i, n: Integer;
  item: TDescriptionItem;
begin
//in lists only!
  inherited;  //assume list created!?
(*
  ID := TTranslationID(TSerializable.LoadIntegerFromStream(Stream));
  i := TSerializable.LoadIntegerFromStream(Stream);
  assert(ord(kind) = i, 'clobbered stream');
*)
  n := TSerializable.LoadIntegerFromStream(Stream);
  FList.Capacity := n;
  for i := 0 to n - 1 do begin
    //Append(TSerializable.LoadStringFromStream(Stream));
    //prevent recursion???
    item := AddFromStream(Stream);
  end;
end;

procedure TDescriptionList.SaveToBinaryStream(Stream: TStream);
var
  i: integer;
  item: TDescriptionItem;
begin
  inherited;
{
  TSerializable.SaveIntegerToStream(ord(ID), Stream);
  TSerializable.SaveIntegerToStream(ord(kind), Stream);
}
  TSerializable.SaveIntegerToStream(Count, Stream);
  for i := 0 to Count - 1 do begin
    item := ItemAt(i);
    item.SaveToBinaryStream(Stream);
  end;
end;
{$ELSE}
{$ENDIF}

{ TPasDelegate }

procedure TPasDelegate.SetPasItem(AItem: TPasItem);
begin
  MyItem := AItem;
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
