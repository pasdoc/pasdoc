{ Test of handling help insight comments, in the form "/// <tag> ... </tag>".
  See http://delphi.wikia.com/wiki/Help_insight,
  example snippet with @link(Parse) function is straight from there.
  See https://sourceforge.net/tracker/?func=detail&atid=304213&aid=3485263&group_id=4213. }
unit ok_helpinsight_comments;
interface

/// <summary>parses the commandline</summary>
///   <param name="CmdLine"> is a string giving the commandline.
///                          NOTE: Do not pass System.CmdLine since it contains the
///                          program's name as the first "parameter".
///                          If you want to parse the commandline as passed by
///                          windows, call the overloaded Parse method without
///                          parameters. It handles this.</param>
procedure Parse(const _CmdLine: string);

/// <summary>Pascal code sample.</summary>
/// <code lang="Delphi">var
///   A: Integer;
// begin
///   A := 1 + 2 + 3;
/// end;</code>
procedure PascalCodeSample;

/// <summary>Multiline code sample.</summary>
/// <code># Sample bash script
/// echo "Hello, World!"
/// echo "This is a multiline code sample."
/// </code>
procedure MultilineCodeSample;

/// <summary>
/// Multiline summary example.
/// This is the second line of the summary.
/// </summary>
procedure MultilineSummaryExample;

implementation
end.