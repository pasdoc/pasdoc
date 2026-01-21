# ok\_deprecated\_directive\_note


# Unit ok\_deprecated\_directive\_note

- [Description](#PasDoc-Description)
- Uses
- [Classes, Interfaces, Objects and Records](#PasDoc-Classes)
- [Functions and Procedures](#PasDoc-FuncsProcs)
- Types
- [Constants](#PasDoc-Constants)
- Variables

<span id="PasDoc-Description"/>

## Description
<p class="hint_directive">Warning: this symbol is deprecated: Deprecation note for unit</p>Test deprecated directive with and without note.<span id="PasDoc-Uses"/>

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

Class&nbsp;[`TTestClass`](ok_deprecated_directive_note.TTestClass.md)
</td>

<td>

&nbsp;
</td>
</tr>
<tr>

<td>

Class&nbsp;[`TTestClassDeprecated1`](ok_deprecated_directive_note.TTestClassDeprecated1.md)
</td>

<td>

&nbsp;
</td>
</tr>
<tr>

<td>

Class&nbsp;[`TTestClassDeprecated2`](ok_deprecated_directive_note.TTestClassDeprecated2.md)
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

<code>procedure <strong><a href="ok_deprecated_directive_note.md#MyProc1">MyProc1</a></strong>; deprecated 'Deprecation note for procedure with some apostrophe: '' here you go:)';</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_deprecated_directive_note.md#MyProc2">MyProc2</a></strong>; deprecated;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_deprecated_directive_note.md#MyProc3">MyProc3</a></strong>; deprecated #72 #$65 'llo';</code>
</td>
</tr>
</table>

### Constants
<span id="PasDoc-Constants"/>


<table>
<tr>

<td>

<code><strong><a href="ok_deprecated_directive_note.md#TestConstDeprecated1">TestConstDeprecated1</a></strong> = 1 deprecated 'Deprecation note for constant';</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_deprecated_directive_note.md#TestConstDeprecated2">TestConstDeprecated2</a></strong> = 1 deprecated;</code>
</td>
</tr>
</table>

## Description

### Functions and Procedures

<table>
<tr>

<td>

<span id="MyProc1"/><code>procedure <strong>MyProc1</strong>; deprecated 'Deprecation note for procedure with some apostrophe: '' here you go:)';</code>
</td>
</tr>
<tr><td colspan="1">

<p class="hint_directive">Warning: this symbol is deprecated: Deprecation note for procedure with some apostrophe: ' here you go:)</p>This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="MyProc2"/><code>procedure <strong>MyProc2</strong>; deprecated;</code>
</td>
</tr>
<tr><td colspan="1">

<p class="hint_directive">Warning: this symbol is deprecated.</p>This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="MyProc3"/><code>procedure <strong>MyProc3</strong>; deprecated #72 #$65 'llo';</code>
</td>
</tr>
<tr><td colspan="1">

<p class="hint_directive">Warning: this symbol is deprecated: Hello</p>Deprecated note should say 'Hello'. Handled Ok, we convert and sum string tokens correctly.

</td></tr>
</table>

### Constants

<table>
<tr>

<td>

<span id="TestConstDeprecated1"/><code><strong>TestConstDeprecated1</strong> = 1 deprecated 'Deprecation note for constant';</code>
</td>
</tr>
<tr><td colspan="1">

<p class="hint_directive">Warning: this symbol is deprecated: Deprecation note for constant</p>This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="TestConstDeprecated2"/><code><strong>TestConstDeprecated2</strong> = 1 deprecated;</code>
</td>
</tr>
<tr><td colspan="1">

<p class="hint_directive">Warning: this symbol is deprecated.</p>This item has no description.



</td></tr>
</table>
