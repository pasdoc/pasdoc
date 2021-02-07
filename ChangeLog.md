# List of changes

## Version 0.16.0 (2021-02-07)

* [New website](https://pasdoc.github.io/), using Jekyll, generated from our wiki, see https://github.com/pasdoc/pasdoc.github.io (Michalis)
* [Moved everything to GitHub](https://github.com/pasdoc/pasdoc)
* [Ancestors list is now affected by external class hierarchy](https://pasdoc.github.io/ExternalClassHierarchy) (Michalis)
* [Markdown support](https://pasdoc.github.io/MarkdownOption) (Fr0sT-Brutal)
  Supporting bold, italic, inline code, multi-line code, URLs, lists.
* [@note and @warning tags](https://pasdoc.github.io/NoteAndWarningTags) (Bi0T1N)
* [@url tag](https://pasdoc.github.io/UrlTag) (Bi0T1N)
* [Allow to lowercase output of @nil, @false, @true by \--lowercase-keywords](https://pasdoc.github.io/TrueFalseNilTag) (Bi0T1N)
* Automatically detect flag like [xxx] at @param description.
  [See here for example](https://github.com/pasdoc/pasdoc/blob/master/tests/testcases/ok_flag_parameter.pas). (PifPof)
* [Scan implementation section of a unit in addition to the interface section](https://pasdoc.github.io/ImplementationCommentsOption) (Fr0sT-Brutal)
* Parser improvements to correctly handle some special cases: reading chars > $FF, "*.inc" includes, files with Mac-style line endings (Fr0sT-Brutal)
* Mem leaks fixed (Fr0sT-Brutal)
* Tag parameters now could be multiline without enclosing parens by means of "line feed" character "\" (Fr0sT-Brutal)
* [Read additional command-line options from file](https://pasdoc.github.io/ConfigFileOption) (Fr0sT-Brutal)
* [\--auto-back-comments command-line option](https://pasdoc.github.io/AutoBackComments) (Fr0sT-Brutal)
* pasdoc_gui opens a file given at command-line, opening WWW browser is optional
  (Fr0sT-Brutal)
* @longCode without markers fixed (Michalis)
* Support for namespaces in units in @links (Fr0sT-Brutal)
* [\--ignore-marker option](https://pasdoc.github.io/IgnoreMarkerOption) (Fr0sT-Brutal)
* Test suite fixes and better documentation, in particular for Windows users
* Catalan translation updated (Xavier Martínez)
* Delphi Tokyo files, to compile all projects and packages (Carlos Feitoza Filho)
* Brazilian Portuguese translation updated (Carlos Feitoza Filho)
* Parse identifiers declared as &xxx, where "xxx" may be a reserved word.
* Possibility to specify additional files using -A or \--additional
  (just like introduction or conclusion, but you can provide any number
  of items) (Alex Merkel)
* Added de.utf8 (German with UTF-8 encoding)
* Automatically remove %region and %endregion from comments
* Show visibility (public, protected, private...) inside records too
  (it is meanigful for advanced records)
* Implement in pasdoc_gui options to (Michalis)
  - customize HTML head and body
  - set external descriptions file
  - configure identifiers excluded from auto-linking
* Rename "internal" to "nested" to describe this language feature.
  Because that is how it's called in official docs
  ( http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Nested_Type_Declarations,
    https://www.freepascal.org/docs-html/ref/refse41.html )
  and the "internal" word has traditionally different meaning
  ("something not supposed to be visible/used from the outside"). (Michalis)
* The automatic tests are now easier to run and check (see tests/README.md) (Michalis)
* `@raises` and `@param` is now supported at properties as well as methods.
  Actually, it's supported everywhere now, but it makes sense only at properties and methods now.
  (Michalis)
* [Tipue (client-side search)](https://pasdoc.github.io/UseTipueSearchOption) improvements:
  - we have upgraded to use Tipue 6.1, which highlights the found terms
  - we strip HTML from Tipue index, which makes the "search results" page correct
* Fix handling SVN fixed-length $Date::
* Parsing of `$if` and `$elseif` expressions, like `defined(MSWINDOWS) or defined(UNIX)` (Michalis)

## Version 0.15.0 (2018-02-08)

* @links to enumerated type members work now.
* [New @includeCode tag](https://pasdoc.github.io/IncludeCodeTag) (by Silvio Clécio)
* @longCode and @preformatted improved to better honor indentation.
* parsing "experimental" directive.
* Allow to customize HTML output more, with your own CSS and HTML, by
  \--html-head, \--html-body-begin, \--html-body-end command-line params.
* HTML output is now HTML5.
* Add our tools to the binary release: pascal_pre_proc (Pascal preprocessor),
  file_to_pascal_data, file_to_pascal_string.
* Improve CSS and HTML, in particular for accessibility and mobile browsers.
  Among many improvements, we removed fixed font size in pixels,
  changed some tables into divs, and fixed HTML validity around Tipue search box.
* pasdoc_gui uses now default font size on your system.
* Updated Spanish (Spain) translation (by Guillermo Martínez Jiménez)
* Tipue works now more efficiently \-- the (potentially large) "index data"
  is only loaded on the "Search Results" page.
* Automatic tests rearrangements and simplifications (see tests/run_all_tests.sh).

Authors: Michalis Kamburelis and contributors mentioned above (thank you!).

## Version 0.14.0 (2015-08-09)

* Many fixes to parsing "deprecated", "platform", "library" directives.
* simplexml output fixes (by Denis Grinyuk)
* The document creation time is not printed in the docs by default.
  Use `--include-creation-time` to show it.
  \--include-creation-time is orthogonal to \--exclude-generator.
* The build duration time is not printed in the output by default.
  Use `--verbosity 3` to show it.
* Brazilian utf8 translation (by Alexsander da Rosa)
* Upgrade tipue to 3.0.1, update jquery to 2.0.0.
* Fix Delphi compilation (long generated tipue code)
  (thanks to Marcos Rocha for investigating)
* Add pasdoc_gui icon (by Karl-Michael Schindler)
* Copyrights and docs fixes (thanks to Paul Gevers)
* Various other small fixes and code cleanups.

## Version 0.13.0 (2013-07-14)

* Handling of declarations nested inside classes (of other types, classes,
  constans and such) (by Arno Garrels)
* Handling HelpInsight comments (by VCejka)
* Parsing Delphi attributes (by VCejka)
* Parsing "final" standard directive (by VCejka)
* Tipue search upgraded to latest Tipue version, fixes problems in
  Google Chrome in some cases (by Michalis Kamburelis)
* Parsing of "deprecated", "platform", "library" directives for properties
  (by Michalis)
* HTML ouput changes (no more <frameset>, so external links/bookmarks
  work naturally; tipue search input+button layout corrected) (by Michalis)
* Mac OS X version includes the GUI.
* Many other small fixes and improvements.

## Version 0.12.1 (2010-11-03)

* Parsing Delphi operator overloads (D2006+),
  Delphi anonymous methods (D2009+),
  Delphi class and record helpers (by Arno Garrels)
* Fix handling source files that start with UTF-8 BOM (closes: #3101708)
  (and clear error messages when encountering UTF-16 or UTF-32 BOMs)
* Cache files get version markers (closes: #3101524)

## Version 0.12.0 (2010-10-31)

* Class Hierarchy diagrams are more complete, because PasDoc knows
  about hierarchy of the standard ObjectPascal classes.
  [You can also extend this by `--external-class-hierarchy` option](https://pasdoc.github.io/ExternalClassHierarchy) (Michalis)
* Support for Delphi Unicode compilers,
  improve processing speed by using TBufferedStream,
  parsing some new Delphi features (like "deprecated 'string'")
  (by Arno Garrels)
* Many improvements to pasdoc_gui:
  * Better adjusts to various themes and font sizes, on all platforms
  * More intuitive UI: "Generate" button on the left,
    "Output directory" on the "Options" tab and filled by default
    with temp directory, and more.
  * xdg-open is used on Unix now.
  (by Michalis Kamburelis)
+ [--ignore-leading= option](https://pasdoc.github.io/IgnoreLeadingOption) (by `<tobigun at users.sourceforge.net>`)
* [Translations](https://pasdoc.github.io/OutputLanguage):
  * Russian localization updated by <werewolf_ at users.sourceforge.net>
  * Simplified Chinese Translation updated by Liu Da
  * Czech translation by Rene Mihula
  * Polish translation updated (by anonymous)
  * Bulgarian translation by Andrew Andreev
  * French translation updated (and utf-8 version added) by Yann Merignac

## Version 0.11.0 (2008-06-22)

* Various fixes to parsing by Richard B. Winston,
  including fixes to Delphi 2006 syntax parsing and library files.
  PasDoc now parses Delphi 2006 RTL and VCL sources.
* Various pasdoc_gui improvements, including
  * "Display Comments" tab (by Richard B. Winston)
  * Many options already available in command-line pasdoc are now in pasdoc_gui too
  * "Store relative paths" option
  * Proper "Save" command on Ctrl+S (doesn't always display SaveDialog)
+ [@image tag](https://pasdoc.github.io/ImageTag) (by Grzegorz Skoczylas and Michalis)
+ [@include tag](https://pasdoc.github.io/IncludeTag)
* program files are now parsed (their "uses" clauses are shown in output)
  (by Mark de Wever)
* [@( and @) construct](https://pasdoc.github.io/TagsParametersMatching)
+ pascal_pre_proc tool (using PasDoc scanner)
+ simplexml output format (by MfG TAK2004 and Michalis)
+ [--auto-link-exclude option](https://pasdoc.github.io/AutoLinkOption)
* Translations:
  + Chinese gb2312 translation by Liu Chuanjun
  * Polish translation updated by Grzegorz Skoczylas
  * Hungarian translation updated by Gergo Jonas
  * Updated Spanish translation from JBarbero Quiter
+ Mac OS X (Darwin) port
* Many bugfixes.

Authors: features above not explicitly marked by author were
done by Michalis Kamburelis.

## Version 0.10.0 (2005-11-26)

+ New command-line options:
    * [\--auto-link](https://pasdoc.github.io/AutoLinkOption)
    * [\--implicit-visibility](https://pasdoc.github.io/ImplicitVisibilityOption)
    * [\--no-macro](https://pasdoc.github.io/NoMacroOption)

+ New @-tags:
    * [@bold and @italic](https://pasdoc.github.io/BoldAndItalicTags)
    * [@seealso](https://pasdoc.github.io/SeeAlsoTag)
    * [@inheritedClass](https://pasdoc.github.io/InheritedClassnameNameTag)
    * [@preformatted (by Ascanio Pressato)](https://pasdoc.github.io/PreformattedTag)
    * [@orderedList, @unorderedList, @definitionList, @item, @itemLabel, @itemSpacing, @itemSetNumber](https://pasdoc.github.io/ListTags)
    * [@table, @row, @rowHead, @cell](https://pasdoc.github.io/TableTags)
    * [@noAutoLinkHere, @noAutoLink](https://pasdoc.github.io/AutoLinkOption)
    * [@tableOfContents](https://pasdoc.github.io/TableOfContentsTag)

* [Dashes rules: em-dash, en-dash, short dash, "@-"](https://pasdoc.github.io/WritingDocumentation)
* FPC macros are now correctly parsed by pasdoc.
* Each detailed description in HTML output is enclosed within gray frame
* Various fixes. E.g.
  * --spell-check-ignore-words works now.
  * @links to @anchors and @sections work always now.
* Many improvements to pasdoc_gui (by Richard B. Winston and Michalis)
* [Back comments](https://pasdoc.github.io/WhereToPlaceComments)

Some compatibility had to be broken:
* New dashes rules break compatibility. But actually previous
  behavior with regards to dashes was broken, because "-"
  was just always directly copied to output. So previously
  in HTML "-" always meant just a short dash (there was no
  way to write en-dash or em-dash). And in LaTeX
  "---"/"--"/"-" meant em-dash/en-dash/short dash,
  but there was no way to escape it (i.e. there was no "@-"
  construct).
* Back-comments feature breaks compatibility if you have comments
  that have as their exact 1st character "<". Now they will be
  interpreted as back-comments (assigned to previous item
  with "<" stripped). To fix your docs, just add
  a space inside such problematic comment right before "<".

Authors: features above not explicitly marked by author were
done by Michalis Kamburelis.

## Version 0.9.0 (2005-07-09)

End-user visible changes:

+ New command-line options:
    * [\--auto-abstract](https://pasdoc.github.io/AutoAbstractOption)
    * [\--introduction and \--conclusion](https://pasdoc.github.io/IntroductionAndConclusion)
    * [\--latex-head](https://pasdoc.github.io/CommandLine)
    * [\--link-gv-uses and \--link-gv-classes](https://pasdoc.github.io/GraphVizSupport)
    * [\--link-look](https://pasdoc.github.io/LinkLookOption)
    * [\--sort](https://pasdoc.github.io/SortOption)
    * [\--use-tipue-search](https://pasdoc.github.io/UseTipueSearchOption)
    * [\--version](https://pasdoc.github.io/CommandLine)

+ New @-tags:
    * [@br](https://pasdoc.github.io/BrTag)
    * [@latex](https://pasdoc.github.io/LatexTag)
    * [@section, @anchor, @title, @shorttitle tags in introduction/conclusion](https://pasdoc.github.io/IntroductionAndConclusion)
    * [@deprecated](https://pasdoc.github.io/DeprecatedTag)
    * [@value and @member](https://pasdoc.github.io/MemberValueTag)

+ pasdoc_gui, a GUI alternative to console pasdoc version.
+ [Spell checking](https://pasdoc.github.io/SpellChecking)
* [Cache is now independent from output format](https://pasdoc.github.io/CacheOption)
* Many fixes and improvements to HTML output,
  it's now structured a little more consistently,
  it's more configurable by CSS,
  it's 100% conforming HTML 4.01 Transitional.
* Many fixes and improvements to LaTeX output,
  it's also structured more consistently,
  and it doesn't omit undocumented items
  (see the bottom of [https://pasdoc.github.io/WritingDocumentation]).
* @longcode improvements:
  it's now formatted in LaTeX output,
  the look of float and hex values inside @longcode in HTML output
  is configurable by CSS.
* Parsing improvements:
  * Better full declaration of items is now displayed in documentation
  * FPC overloaded operators are now parsed
  * Delphi hint directives (deprecated, platform, library) are now parsed
* [You don't have to enclose tag parameters in parenthesis](https://pasdoc.github.io/TagsParametersWithoutParenthesis)
* Many many other small fixes and improvements.

Many internal improvements, like:
+ We maintain a large set of tests (regression tests, conformance tests etc.), see [tests/README.md](https://github.com/pasdoc/pasdoc/blob/master/tests/README.md)

Documentation:
  You can find the most complete and up-to-date documentation
  of pasdoc features in [our wiki](https://github.com/pasdoc/pasdoc/wiki).
  Unfortunately, with pasdoc 0.9.0, offline documentation
  (previously in docs/ directory of released archives) is no longer
  provided, this is intended to be fixed in future releases
  (see [https://pasdoc.github.io/ToDoOfflineDocs]).

Some compatibility had to be broken:
* pasdoc.css will be always overwritten when you generate HTML documentation.
  [You must use --css command-line option](https://pasdoc.github.io/CssOption)
  if you want to use your custom css.
* By default no items are sorted.
  [You must use --sort command-line option](https://pasdoc.github.io/SortOption)
  if you want to change this.
* @links look now a little different, see
  [https://pasdoc.github.io/LinkLookOption] and
  [https://pasdoc.github.io/LinkTag],
  use --link-look=stripped if you really need old behavior.

Authors: many. See @author tags at the beginning of pasdoc 0.9.0 units.

## Versions 0.8.8.1 to 0.8.8.3 (2004-07-12)

+ applied a bunch of patches that were floating around on the mailing list
+ some code cleanup

Authors: Johannes Berg, others.

## Version 0.8.8 (2004-05-06)

+ fix a lot of tiny bugs, range check errors, etc
+ implement consolidation for // style comments so that you can now
  use // style comments in multiple lines like this:
  // @abstract(something)
  // and a real comment
  and both lines will be added to the documentation.
+ @longcode tag implemented (fixes bug #802469)
+ \LaTeX output

Authors: Johannes Berg, Richard B Winston, Carl Eric Codere

## Version 0.8.8-pre6 (2003-11-20)

+ fix 3 logged bugs
  [ pasdoc-Bugs-842325 ] bug in function IsMacro
  [ pasdoc-Bugs-844324 ] tag "returns" very together does not insert a jump of line
  [ pasdoc-Bugs-844325 ] The tag "return" does not work, "returns" with "s" if
+ corrected output of @raises tag (last character could be cut off)
+ javi fixed and updated the Spanish translation
+ added proper warning for FPC operator overloading as it is not supported
  right now. TODO item: write a ParseOperator function
+ added support for FreePascal inline "calling convention"

Authors: Johannes Berg

## Version 0.8.8-pre5 (2003-08-04)

* more CSS, completely new style

Authors: Johannes Berg

## Version 0.8.8-pre4 (2003-05-14)

+ more CSS
+ code cleanup
+ if class has no description, write ancestor's description and
  a warning
+ reordering by visibility
+ @param, @returns, @raises except argument in parentheses now

Authors: Thomas Mueller

## Version 0.8.8-pre3 (2003-05-09)

+ write complete known hierarchy in class descriptions
- remove calling hhc.exe, you should do this from a script
+ fixed HTML-Help output
+ fixed numeric name creation wrt. cross-links
  (there was a rather BAD bug, now all numbers are
   sequential too!)
+ automatically turn on numeric filenames for HTML help because
  hhc chokes on extra dots in filenames
+ bugfix: "~" is a valid character in a URL

Authors: Thomas Mueller/Johannes Berg

## Version 0.8.8-pre2 (2003-05-02)

+ better HTML output with CSS
+ HTML output has new section links
+ fix hierarchy: objects descend from TObject,
  interfaces with GUID are now shown properly

Authors: Thomas Mueller/Johannes Berg

## Version 0.8.8-pre1 (2003-05-01)

+ hierarchy is shown properly with everything
+ name directive for imported functions parsed properly
+ proper HTML entity encoding

Authors: Thomas Mueller/Johannes Berg

## Version 0.8.7 (2003-04-20)

Authors: Johannes Berg/Thomas Mueller

Based on Ralf Junker's changes, I did the following:

+ added enumerated type parsing
+ made work with FPC
+ records (handled like classes),
  + case statements in records
  + nested records
+ spell-checker for linux (currently disabled, use an older CVS file
  of RunHelp to use under Kylix, will not work in FPC, need advice)
+ new option-parser
+ uses delphi streams instead of files. Supposedly does not work
  on all platforms FPC runs on - need advice
+ commentmarker (for example only {: comments )
+ declarations like
  "var a: function(x,y,z:Integer):Integer cdecl = nil"
  are parsed correctly
+ dependency plotting with GraphViz (AT&T )
  (not very useful)
+ uses clause will be included in doc (optional)
+ links "http://", "ftp://" etc. are recognized automatically
+ varargs directive
+ abbreviations "@author(johannes)" can be expanded
  to @author(Johannes Berg <...>) via abbreviatons file
+ @cvs($Date ...$), @cvs($Author ...$) is recognized and
  used for lastmod / author
  + duplicate authors skipped
- name search doesn't look into classes any more,
  if a method reference is needed outside the current class
  then the class has to be specified:
  @link(class.method)
  (faster and less error-prone)
+ HTML output uses unitname.classname.html instead of numbers,
  but has option to make number-only-filenames (for short-name
  filesystems)
+ class member visibility can be specified in output
+ long option names (see PasDoc --help)
+ images are no longer carried in a .RES file but in include
  files instead, as constants in code, for FPC

## Some time inbetween... (0.6.21 to 0.8.6)

 - LaTeX documentation removed
 - see http://zeitungsjunge.de/delphi/PasDoc/History.htm for
   more information.

Authors: Ralf Junker

## Version 0.6.20 (2000-04-20)

- added Rodrigo Jardim's translations of pasdoc's output to Brasilian
  Portuguese; new switch -b (and --brasilian)
- increased STEP from 32 to 128 when reallocating description pointers,
  this makes pasdoc crash less with RTE 216; bug must still be fixed by
  changing GetMem's default behaviour

Authors: Marco Schmidt

## Version 0.6.19 (2000-04-17)

- fixed bug that prevented pasdoc from skipping code until an $else
  conditional directive (thanks to Michael v. Canneyt for reporting
  this bug)

Authors: Marco Schmidt

## Version 0.6.17 (2000-04-12)

- added Marc Weustinks Dutch translation to pasdoc; switch -m (or --dutch) now
  creates Dutch output
- successfully created docs on Delphi 5 rtl\win directory (222 seconds, 16 MB html
  files on a P-II 350, 256 MB RAM)

Authors: Marco Schmidt

## Version 0.6.15 (2000-04-09)

- added Michael v.C.'s update of the tex unit (changed chapter/section/
  subsection nesting of LaTeX output)
- moved LoadDescriptionFile and LoadDescriptionFiles from main.TPasdoc to
  gendoc.TDocGenerator
- worked on external description file loading
- email addresses in author tags are now displayed as mailto links in HTML
  output
- hard-coded HTML colors in body element (already added colors for tables
  etc., as long as the whole thing is not CSS-based, it's better to have
  all colors defined so that unusual default colors of a browser won't ruin
  the overall impression)
- released 0.6.15

Authors: Marco Schmidt

## Version 0.6.13 (2000-04-08)

- released 0.6.13

Authors: Marco Schmidt

## Version 0.6.13 (2000-04-06)

- fixed bug that made pasdoc ignore a unit that has ). in a type declaration;
  in scanning.pas, ). was supposed to be a replacement for ] while it must be
  .)
- empty author tags are now ignored
- fixed LaTeX bug that kept pasdoc from converting identifiers that were not
  found in the list of items to output format style

Authors: Marco Schmidt

## Version 0.6.13 (2000-04-05)

- added -u DIR switch to make pasdoc search for include files

Authors: Marco Schmidt

## Version 0.6.11 (2000-04-04)

- moved objects.pas to directory 'other'; will now use default objects unit from FPC
- for the time being, I stop trying to compile pasdoc with Delphi, there is some bug
  I am unable to trace; Delphi 5 compiles pasdoc without problems but crashes when
  running it on appinfo.pas between the second and third constant for no apparent
  reason; I will try to fix bugs that are more obvious in the near future...
- fixed bug that made default keyword after a property appear as a field of that class
  (thanks to Andre Jager for pointing this out)
- fixed bug that made pasdoc crash on an empty author tag: @author()
  (thanks to Michael Hess for finding and isolating (!) this!)
- made pasdoc skip property declaration; property parsing needs some work,
  I will do this soon

Authors: Marco Schmidt

## Version 0.6.11 (2000-04-03)

- created sourceforge.net account; pasdoc is now at http://pasdoc.sourceforge.net
- modified homepage to have frames; single page has become too huge

Authors: Marco Schmidt

## Version 0.6.11 (2000-03-31)

- on a totally unrelated note - I just passed my exam in theoretical computer
  science - woohooo! again some time to work on pasdoc...

Authors: Marco Schmidt

## Version 0.6.10 ms (2000-02-11)

- changed directory structure of the pasdoc project (added src, bin, man etc.)
- added -v (or --verbosity) switch to be able to change the amount of output

Authors: Marco Schmidt

## Version 0.6.10 ms (2000-02-08)

- added PDF version of manual to homepage, thanks to Martin Krumpolec

Authors: Marco Schmidt

## Version 0.6.10 (2000-02-07)

- arguments in functions and procedures no longer become links if there is a
  type of the same name (THTMLDocGenerator.WriteCodeWithLinks)
- HTML now writes Class, Interface or Object (translated to the chosen
  language) in front of each CIO item in a unit's summary of these items
- removed some unnecessary methods from THTMLDocGenerator in html.pas

Authors: Marco Schmidt

## Version 0.6.10 (2000-02-06)

- added Catalan and Spanish translations sent in by Ivan Montes Velencoso to
  gendoc.pas and corresponding command line switches in main.pas
- removed tabularx environments from TTexDocGenerator (for LaTeX output)
- added equivalent long command line switches for most of the short ones
- added check that will detect unknown (=> invalid) command line switches and
  react on them with an error message
- received pasdoc DOS binaries sent in by Jean-Pierre Vial and created a 0.6.9
  DOS binaries release, then updated the homepage
- added IOResult check in TFileInputStream.Open in filestre.pas; no more crashing
  on non-existent files
- added explanation for non-unique identifiers to manual; may find a workaround
  for this later (bug?)
- fixed bug that made the time in HTML documents always be 00:00:00 with
  the Delphi pasdoc version
- started new unit chars that will be the basis for the handling of special
  characters in other languages

Authors: Marco Schmidt

## Version 0.6.9 (2000-02-05)

- integrated fixes sent in by Erwin Scheuch-Hellig (no more destination
directory in HTML overview file links, path names of source code files
are no longer dropped, destination directory gets a terminating slash)
- renamed this file from History to ChangeLog
- updated homepage and freshmeat appindex

Authors: Marco Schmidt

## Version 0.6.9 (1999-11-26)
- pasdoc now seems to work with Delphi 5

Authors: Marco Schmidt

## Version 0.6.9 (1999-11-17)

- updated French output TDocGenerator.GetFrenchString in gendoc.pas

Authors: Marco Schmidt

## Version 0.6.9 (1999-11-16)

- added support for user-defined line feeds

Authors: Marco Schmidt

## Version 0.6.8 (1999-09-28)

- improved TeX output

Authors: Marco Schmidt

## Version 0.6.8 (1999-09-27)

- continued work on Tex unit; TeX output working again partially
- added -j switch to suppress headers and footers in Tex output (Johann Glaser's suggestion)

Authors: Marco Schmidt

## Version 0.6.8 (1999-09-24)

- replaced numerous calls to WriteLn with PrintLn from new unit Msg
- moved TPasdoc to new unit Main

Authors: Marco Schmidt

## Version 0.6.7 (1999-08-16)

- fixed bug in main, wrong parameter (units instead of file list)
- fixed bug in TTokenizer.SkipUntilElseOrEndif that would choke on unidentified directives
- fixed some bugs in TParser.ParseProperty

Authors: Marco Schmidt

## Version 0.6.7 (1999-08-15)

- continued implementing handling of @descrfile tags

Authors: Marco Schmidt

## Version 0.6.6 (1999-08-10)

- fixed property index parsing bug

Authors: Marco Schmidt

## Version 0.6.5 (1999-08-08)

- HTML declarations of functions, procedures, methods and properties now contain hyperlinks
- started adding support for descriptions in external files

Authors: Marco Schmidt

## Version 0.6.4 (1999-08-05)

- added support for $ifopt directive
- added support for resourcestring key word (now handled like constants)

Authors: Marco Schmidt

## Version 0.6.3 (1999-08-04)

- added -s switch to read file names from text file
- manual updated, large part added
- made HTML doc generator suppress Properties section for objects (which never have properties)
- fixed bug that made HTML generator write no empty method section for C/I/O
- added @exclude() tag to disallow inclusion into output

Authors: Marco Schmidt

## Version 0.6.2 (1999-08-03)

- various fixes
- initial property parsing and output

Authors: Marco Schmidt

## Version 0.6.1 (1999-08-02)

- made 'include private fields / methods / properties' switch work
- added dates, author to classes / interfaces / objects
- cleaned up TDocGenerator
- moved STATE_xxx constants from unit Parsing to Items

Authors: Marco Schmidt

## Version 0.6.1 (1999-07-29)

- rewrote part to support different output languages
  (TDocGenerator.GetString now calls TDocGenerator.GetStringEnglish
   etc., each language gets its own GetStringXXXX function)
- integrated Jean Dit Bailleul's French translation

Authors: Marco Schmidt

## Version 0.6.0 (1999-04-27)

- added support for directive file switch -f

Authors: Marco Schmidt

## Version 0.6.0 (1999-04-18)

- added support for getting ancestor list in objects / classes
  and skipping interface ID in Delphi-4-type interfaces;
  now Delphi 4 d4system.pas and d4sysuti.pas are parsed flawlessly!

Authors: Marco Schmidt

## Version 0.6.0 (1999-03-11)

- replaced platform.inc with version from latest FV snapshot to detect
  Delphi 4
- added CompVer unit to store compiler name / OS / bits
- moved TItem and descendants to new unit Items
- started adding support for interfaces (Delphi 3.0+ feature)
- added a stripped-down version of Objects to work with Delphi
  (Delphi command line compiler dcc32 now compiles pasdoc, but
   execution results in runtime error 103 at the beginning);
  don't use this Objects for anything else, only TCollection, TObject
  and TSortedCollection (IIRC) will be used!

Authors: Marco Schmidt

## Version v0.5.? (????/??/??)

- added test.pas to test pasdoc functionality
- changed switches to one letter
- added --version and --help switches
- added -d switch to specify conditional directive on command line
- added new units Scanning and Tokenizi for decent support of
  conditional compiler directives
- created a basic pasdoc.tex file that will be the manual
- added applications pastype, pasdep and pasconv (no functionality yet)
  which will use the Scanning unit

Authors: Marco Schmidt

## Version 0.5.2 (1999-02-25)

- moved TPasDoc from parsing.pas to pasdoc.pas
- added fields Language, DestDir and Units to TDocGenerator

Authors: Marco Schmidt

## Version 0.5.2 (1999-02-24)

- created scanning.pas

Authors: Marco Schmidt

## Version 0.5.2 (1999-02-22)

- unit Parsing exceeded 3000 lines of code, so I
  moved TText from unit Parsing to new unit Texts

Authors: Marco Schmidt

## Version 0.5.1 (1999-02-11)

- fixed several bugs
  - HTML links
  - property parsing
  - class parsing
  - some changes in tex generator
- added support for German docs

Authors: Marco Schmidt

## Version 0.5.0 (1998-12-10)

- added "nogeneratorinfo" switch

Authors: Marco Schmidt

## Version 0.5.0 (1998-12-09)

- continued work on ExpandDescription

Authors: Marco Schmidt

## Version 0.5.0 (1998-12-08)

- first entry in this history file
- restarted work on pasdoc after 2-3 months...
- now pasdoc can only create one type of output each time it gets called,
  not multiple
- TGenDoc.WriteDescription copies all data byte-wise to output stream;
  replacing of links and special characters is done in additional
  procedure TPasDoc.ExpandDescriptions to be called after
  TDocGenerator.BuildLinks and before TDocGenerator.WriteDocumentation

Authors: Marco Schmidt
