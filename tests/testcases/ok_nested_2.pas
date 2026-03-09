{ Testcase of classes nested in classes. This is a reworked version of code
  from Castle Game Engine. }
unit ok_nested_2;

interface

type
  TCastleProfiler = class
  public
    type
      TTimes = class; // forward declaration, just to make testcase harder (this was causing bugs)

      TTime = class
        Description: String;
        Time, ProcessTime: TFloatTime;
        Finished: Boolean;
        Children: TObject; // nil if empty; always TTimes, but declaring it as such confuses C++ Builder
        destructor Destroy; override;
        function Summary(const Indent: String; const AFloatPrecision: Cardinal): String;
      end;

      TTimeList = {$ifdef FPC}specialize{$endif} TObjectList<TTime>;

      TTimes = class(TTimeList)
        procedure Sort;
        function Summary(const Indent: String; const AFloatPrecision: Cardinal): String;
      end;

    var
      FEnabled: boolean;
      FFloatPrecision: Cardinal;

      { Collected times tree. }
      Times: TTimes;

      { The currently started (but not stopped) TTime structures. }
      CurrentStack: TTimeList;
  public
    const
      DefaultFloatPrecision = 2;

    constructor Create;
    destructor Destroy; override;

    { Whether to gather speed measurements.
      When not enabled, the @link(Start) and @link(Stop) methods do as little
      as possible to not waste time. }
    property Enabled: boolean read FEnabled write FEnabled;

    { Summary of the gathered speed measurements. }
    function Summary: string;

    { Clear currently gathered times.

      Do not call this when some time measure is started but not yet stopped
      Currently, this will cause such time measure to be rejected,
      and stopping it will cause a one-time warning,
      but don't depend on this exact behavior in the future.
      We will always gracefully accept this case (not crash). }
    procedure Clear; deprecated 'This method is not reliable (may cause crashes when used between Start/Stop) and also not comfortable. To display partial profile information, better use Stop with 2nd argument true.';

    { Start a task which execution time will be measured.
      You @italic(must) later call @link(Stop) with
      the returned TCastleProfilerTime value.

      Typical usage looks like this:

      @longCode(#
      procedure TMyClass.LoadSomething;
      var
        TimeStart: TCastleProfilerTime;
      begin
        TimeStart := Profiler.Start('Loading something (TMyClass)');
        try
          // do the time-consuming loading now...
        finally
          Profiler.Stop(TimeStart);
        end;
      end;
      #)

      If you don't use the "finally" clause to @italic(always) call
      the matching @link(Stop), and an exception may occur in LoadSomething,
      then you will have an unmatched Start / Stop calls.
      The profiler output is undefined in this case.
      (However, we guarantee that the profiler will not crash or otherwise cause
      problems in the application.)
      So, you do not really have to wrap this in "try ... finally ... end" clause,
      if it's acceptable that the profiler output is useless
      in exceptional situations.

      @seealso Stop
    }
    function Start(const Description: String): TCastleProfilerTime;

    { Stop a task. This call must match earlier @link(Start) call.

      If LogSummary, and profiling is @link(Enabled), we will output (to CastleLog)
      a summary of things that happened within this particular TCastleProfilerTime
      instance (between it's start and stop).
      This is useful when you are interested not only in adding this
      TCastleProfilerTime to the profiler tree, but also in immediately viewing the
      times in this TCastleProfilerTime subtree.

      If LogSummary and LogSummaryOnlyIfNonTrivial, then the summary will be output
      only if it's larger than some ignorable amount of time (right now: 1 milisecond).
      This is useful to output only things that take non-trivial amount of time.

      @seealso Start }
    procedure Stop(const TimeStart: TCastleProfilerTime;
      const LogSummary: Boolean = false;
      const LogSummaryOnlyIfNonTrivial: Boolean = false);
  end;

implementation
end.
