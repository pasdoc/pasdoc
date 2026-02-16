# ok\_hint\_directives


# Unit ok\_hint\_directives

- [Description](#PasDoc-Description)
- Uses
- [Classes, Interfaces, Objects and Records](#PasDoc-Classes)
- [Functions and Procedures](#PasDoc-FuncsProcs)
- Types
- [Constants](#PasDoc-Constants)
- [Variables](#PasDoc-Variables)

<span id="PasDoc-Description"/>

## Description
<p class="hint_directive">Warning: this symbol is deprecated.</p><p class="hint_directive">Warning: this symbol is specific to some platform.</p><p class="hint_directive">Warning: this symbol is specific to some library.</p>Test parsing &quot;platform&quot;, &quot;library&quot; and &quot;deprecated&quot; directives (called collectively &quot;hint directives&quot;) by pasdoc.</p>
<p>


Related tracker bug: \[ 1196073 \] &quot;some modifiers are not parsed&quot;.

We want to support all situations where these directives are allowed in modern FPC (>= 2.5.1) and Delphi. Their placement in unfortunately not consistent, thanks go to Borland. Quoting Delphi help (from Kylix 3): &quot;Hint directives can be applied to type declarations, variable declarations, class and structure declarations, field declarations within classes or records, procedure, function and method declarations, and unit declarations.&quot;

Summary:



1.  Between &quot;unit UnitName&quot; and hints you *mustn't* put any semicolon, and you *mustn't* put any semicolons between hints. 
 Same thing for CIOs (Classes / Interfaces / Objects / Records). 
 Same thing for CIOs fields. 
 Same thing for variables. 
 Same thing for constants.

1.  Between &quot;procedure/function Name (...)&quot; and hints you *must* put a semicolon, and semicolons between hints are allowed but not required. It seems that you can't specify &quot;library&quot; directive for procedures/functions -- why? Probably because &quot;library&quot; is a keyword and Borland was unable to correctly modify it's compiler to parse such thing. But pasdoc parses library directive correctly.

1.  Between method and hints you *must* put a semicolon, and semicolon between hints is *required*. You can specify &quot;library&quot; directive for methods.





I'm unable to figure out how to specify these hints for normal (non-structural) types. If anyone can 

- tell me how to specify hint directives for non-structural types or

- explain why parsing these directives is so weird and inconsistent in Delphi or

- point me to some precise documentation by Borland specifying grammar rules with these directives



 ... then please send email about this to pasdoc-main mailing list (or directly to me, Michalis Kamburelis, <kambi@users.sourceforge.net>, if your comments about this do not really concern pasdoc). I will be grateful.

Contrary to most units in tests/, this unit *is* kept at compileable by Delphi/Kylix and FPC. That's because this unit is also a test whether we really specify here hint directives in the way parseable by Delphi/Kylix.<span id="PasDoc-Uses"/>

## Overview

### Classes, Interfaces, Objects and Records
<span id="PasDoc-Classes"/>


<table>
<tr class="listheader">
<th class="itemname">Name</th>
<th class="itemdesc">Description</th>
</tr>
<tr>

<td>

Class&nbsp;[`TTestClassDeprecated`](ok_hint_directives.TTestClassDeprecated.md)
</td>

<td>

&nbsp;
</td>
</tr>
<tr>

<td>

Record&nbsp;[`TTestRecordDeprecated`](ok_hint_directives.TTestRecordDeprecated.md)
</td>

<td>

&nbsp;
</td>
</tr>
</table>

### Functions and Procedures
<span id="PasDoc-FuncsProcs"/>


<table>
<tr>

<td>

<code>function <strong><a href="ok_hint_directives.md#TestFuncCombined">TestFuncCombined</a></strong>(SomeParams: Integer): Integer; deprecated; platform;</code>
</td>
</tr>
<tr>

<td>

<code>function <strong><a href="ok_hint_directives.md#TestFuncDeprecated">TestFuncDeprecated</a></strong>: Integer; deprecated;</code>
</td>
</tr>
<tr>

<td>

<code>function <strong><a href="ok_hint_directives.md#TestFuncPlatform">TestFuncPlatform</a></strong>: Integer; platform;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_hint_directives.md#TestProcCombined">TestProcCombined</a></strong>(SomeParams: Integer); deprecated  platform;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_hint_directives.md#TestProcDeprecated">TestProcDeprecated</a></strong>; deprecated;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_hint_directives.md#TestProcPlatform">TestProcPlatform</a></strong>; platform;</code>
</td>
</tr>
</table>

### Constants
<span id="PasDoc-Constants"/>


<table>
<tr>

<td>

<code><strong><a href="ok_hint_directives.md#TestConstPlatform">TestConstPlatform</a></strong> = 1 platform;</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_hint_directives.md#TestConstLibrary">TestConstLibrary</a></strong> = 2 library;</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_hint_directives.md#TestConstDeprecated">TestConstDeprecated</a></strong> = 3 deprecated;</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_hint_directives.md#TestConstCombined">TestConstCombined</a></strong> = 4 deprecated library platform;</code>
</td>
</tr>
</table>

### Variables
<span id="PasDoc-Variables"/>


<table>
<tr>

<td>

<code><strong><a href="ok_hint_directives.md#TestVarPlatform">TestVarPlatform</a></strong>: Integer platform;</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_hint_directives.md#TestVarLibrary">TestVarLibrary</a></strong>: Integer library;</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_hint_directives.md#TestVarDeprecated">TestVarDeprecated</a></strong>: Integer deprecated;</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_hint_directives.md#TestVarCombined">TestVarCombined</a></strong>: Integer library deprecated platform;</code>
</td>
</tr>
</table>

## Description

### Functions and Procedures

<table>
<tr>

<td>

<span id="TestFuncCombined"/><code>function <strong>TestFuncCombined</strong>(SomeParams: Integer): Integer; deprecated; platform;</code>
</td>
</tr>
<tr><td colspan="1">

<p class="hint_directive">Warning: this symbol is deprecated.</p><p class="hint_directive">Warning: this symbol is specific to some platform.</p>This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="TestFuncDeprecated"/><code>function <strong>TestFuncDeprecated</strong>: Integer; deprecated;</code>
</td>
</tr>
<tr><td colspan="1">

<p class="hint_directive">Warning: this symbol is deprecated.</p>This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="TestFuncPlatform"/><code>function <strong>TestFuncPlatform</strong>: Integer; platform;</code>
</td>
</tr>
<tr><td colspan="1">

<p class="hint_directive">Warning: this symbol is specific to some platform.</p>This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="TestProcCombined"/><code>procedure <strong>TestProcCombined</strong>(SomeParams: Integer); deprecated  platform;</code>
</td>
</tr>
<tr><td colspan="1">

<p class="hint_directive">Warning: this symbol is deprecated.</p><p class="hint_directive">Warning: this symbol is specific to some platform.</p>This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="TestProcDeprecated"/><code>procedure <strong>TestProcDeprecated</strong>; deprecated;</code>
</td>
</tr>
<tr><td colspan="1">

<p class="hint_directive">Warning: this symbol is deprecated.</p>This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="TestProcPlatform"/><code>procedure <strong>TestProcPlatform</strong>; platform;</code>
</td>
</tr>
<tr><td colspan="1">

<p class="hint_directive">Warning: this symbol is specific to some platform.</p>This item has no description.



</td></tr>
</table>

### Constants

<table>
<tr>

<td>

<span id="TestConstPlatform"/><code><strong>TestConstPlatform</strong> = 1 platform;</code>
</td>
</tr>
<tr><td colspan="1">

<p class="hint_directive">Warning: this symbol is specific to some platform.</p>This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="TestConstLibrary"/><code><strong>TestConstLibrary</strong> = 2 library;</code>
</td>
</tr>
<tr><td colspan="1">

<p class="hint_directive">Warning: this symbol is specific to some library.</p>This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="TestConstDeprecated"/><code><strong>TestConstDeprecated</strong> = 3 deprecated;</code>
</td>
</tr>
<tr><td colspan="1">

<p class="hint_directive">Warning: this symbol is deprecated.</p>This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="TestConstCombined"/><code><strong>TestConstCombined</strong> = 4 deprecated library platform;</code>
</td>
</tr>
<tr><td colspan="1">

<p class="hint_directive">Warning: this symbol is deprecated.</p><p class="hint_directive">Warning: this symbol is specific to some platform.</p><p class="hint_directive">Warning: this symbol is specific to some library.</p>This item has no description.



</td></tr>
</table>

### Variables

<table>
<tr>

<td>

<span id="TestVarPlatform"/><code><strong>TestVarPlatform</strong>: Integer platform;</code>
</td>
</tr>
<tr><td colspan="1">

<p class="hint_directive">Warning: this symbol is specific to some platform.</p>This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="TestVarLibrary"/><code><strong>TestVarLibrary</strong>: Integer library;</code>
</td>
</tr>
<tr><td colspan="1">

<p class="hint_directive">Warning: this symbol is specific to some library.</p>This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="TestVarDeprecated"/><code><strong>TestVarDeprecated</strong>: Integer deprecated;</code>
</td>
</tr>
<tr><td colspan="1">

<p class="hint_directive">Warning: this symbol is deprecated.</p>This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="TestVarCombined"/><code><strong>TestVarCombined</strong>: Integer library deprecated platform;</code>
</td>
</tr>
<tr><td colspan="1">

<p class="hint_directive">Warning: this symbol is deprecated.</p><p class="hint_directive">Warning: this symbol is specific to some platform.</p><p class="hint_directive">Warning: this symbol is specific to some library.</p>This item has no description.



</td></tr>
</table>
