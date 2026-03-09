{ Testcase of classes nested in classes. This is a reworked version of code
  from Castle Game Engine. }
unit ok_nested_1;

interface

type
  TRepoSoundEngine = class(TSoundEngine)
  private
    type
      TSoundInfoBuffer = class;
      TSoundInfoList = class;
      TSoundGroup = class;

      { Sound that can be played -- actual sound with buffer, or an alias to it. }
      TSoundInfo = class
      strict private
        { Although we only support one instance of TSoundEngine,
          in @code(SoundEngine), but it seems more future-proof
          to store in TSoundInfo own reference to TSoundEngine.
          Automatically assigned in ReadElement. }
        FOwningSoundEngine: TSoundEngine;
      strict protected
        property OwningSoundEngine: TSoundEngine read FOwningSoundEngine;
      public
        { Unique sound name (including parent group names). Empty for the special sound stNone = nil. }
        Name: String;

        { Like Name, but without parent group names.
          Unique within the ParentGroup (not necessarily unique among all sounds). }
        ShortName: String;

        { A group (one among FSoundGroups, or @nil if not in any group). }
        ParentGroup: TSoundGroup;

        { Read a sound from XML element <sound> or <alias>. }
        procedure ReadElement(const Element: TDOMElement;
          const AParentGroup: TSoundGroup;
          const BaseUrl: String; const ASoundEngine: TRepoSoundEngine); virtual;

        { Do some finalization once all sounds are known, and their names are known. }
        procedure ResolveNames(const AllSounds: TSoundInfoList); virtual;

        { Get the final TSoundInfoBuffer, resolving aliases. }
        function FinalSound(const RecursionDepth: Cardinal): TSoundInfoBuffer; virtual; abstract;
      end;

      { List of TSoundInfo. }
      TSoundInfoList = class({$ifdef FPC}specialize{$endif} TObjectList<TSoundInfo>)
      public
        { Index of sound with given TSoundInfo.Name, or -1 if not found. }
        function IndexOfName(const SoundName: String): Integer;
      end;

      { Sound that can be played, as an alias to a number of other TSoundInfo instances
        (an alias may lead to another alias, that's OK, as long as eventually it
        resolves into actual sound name). }
      TSoundInfoAlias = class(TSoundInfo)
      strict private
        Target: TCastleStringList;
      public
        constructor Create;
        destructor Destroy; override;
        procedure ReadElement(const Element: TDOMElement;
          const AParentGroup: TSoundGroup;
          const BaseUrl: String; const ASoundEngine: TRepoSoundEngine); override;
        procedure ResolveNames(const AllSounds: TSoundInfoList); override;
        function FinalSound(const RecursionDepth: Cardinal): TSoundInfoBuffer; override;
      end;

      { Sound that can be played, with a buffer. }
      TSoundInfoBuffer = class(TSoundInfo)
      public
        Sound: TCastleSound;
        procedure ReadElement(const Element: TDOMElement;
          const AParentGroup: TSoundGroup;
          const BaseUrl: String; const ASoundEngine: TRepoSoundEngine); override;
        function FinalSound(const RecursionDepth: Cardinal): TSoundInfoBuffer; override;
        destructor Destroy; override;
      end;

      TSoundGroup = class(TSoundInfoList)
      public
        { Group name (including parent group names). }
        Name: string;
        { Group URL.
          Absolute (including parent group URL parts).
          Always ends with slash. }
        Url: String;
        { A parent group (one among FSoundGroups, or @nil if not in any group). }
        ParentGroup: TSoundGroup;
      end;

      TSoundGroupList = class({$ifdef FPC}specialize{$endif} TObjectList<TSoundGroup>)
      public
        { Index of group with given TSoundGroup.Name, or -1 if not found. }
        function IndexOfName(const GroupName: String): Integer;
      end;

      TLoopingChannelList = {$ifdef FPC}specialize{$endif} TObjectList<TLoopingChannel>;

    var
      FSoundImportanceNames: TStringList;
      { A list of sounds used by your program.
        Each sound has a unique name, used to identify sound in
        the XML file and for SoundFromName function.

        At the beginning, this list always contains exactly one sound, empty.
        This is a special TSoundInfoBuffer that has Sound=nil and Name=''.
        It means "no sound" in many cases. }
      FSounds: TSoundInfoList;
      FSoundGroups: TSoundGroupList;
      FRepositoryUrl: String;
      FLoopingChannels: TLoopingChannelList;

    procedure SetRepositoryUrl(const Value: String);
    procedure RestartLoopingChannels;
    function GetLoopingChannel(const Index: Cardinal): TLoopingChannel;
    procedure ContextOpenCore; override;
    property SoundImportanceNames: TStringList read FSoundImportanceNames;
    procedure AddSoundImportanceName(const Name: string; Importance: Integer);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation
end.