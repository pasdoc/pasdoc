# ok\_macros


# Unit ok\_macros

- [Description](#PasDoc-Description)
- Uses
- [Classes, Interfaces, Objects and Records](#PasDoc-Classes)
- [Functions and Procedures](#PasDoc-FuncsProcs)
- Types
- [Constants](#PasDoc-Constants)
- Variables

<span id="PasDoc-Description"/>

## Description
Test of FPC macros handling.</p>
<p>


Parts based on \[[http://sourceforge.net/tracker/index.php?func=detail&amp;aid=861356&amp;group\_id=4213&amp;atid=354213](http://sourceforge.net/tracker/index.php?func=detail&amp;aid=861356&amp;group_id=4213&amp;atid=354213)\]<span id="PasDoc-Uses"/>

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

Class&nbsp;[`TAncestor`](ok_macros.TAncestor.md)
</td>

<td>

&nbsp;
</td>
</tr>
<tr>

<td>

Class&nbsp;[`TMyClass`](ok_macros.TMyClass.md)
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

<code>procedure <strong><a href="ok_macros.md#MyProc1">MyProc1</a></strong>( a:Integer);</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_macros.md#MyProc2">MyProc2</a></strong>( b: Integer);</code>
</td>
</tr>
<tr>

<td>

<code>function <strong><a href="ok_macros.md#Foo">Foo</a></strong>(c: string): Integer;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_macros.md#MyProc3">MyProc3</a></strong>( X: Integer = 1; Y: Integer = 2);</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_macros.md#ThisShouldBeIncluded">ThisShouldBeIncluded</a></strong>;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_macros.md#ThisShouldBeIncluded2">ThisShouldBeIncluded2</a></strong>;</code>
</td>
</tr>
</table>

### Constants
<span id="PasDoc-Constants"/>


<table>
<tr>

<td>

<code><strong><a href="ok_macros.md#ThisShouldBeTrue">ThisShouldBeTrue</a></strong> = true;</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_macros.md#FourConst">FourConst</a></strong> =  (1 + 1) \* (1 + 1);</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_macros.md#OneAndNotNothing">OneAndNotNothing</a></strong> = 1  + 1;</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_macros.md#OnlyOne">OnlyOne</a></strong> = 1 ;</code>
</td>
</tr>
</table>

## Description

### Functions and Procedures

<table>
<tr>

<td>

<span id="MyProc1"/><code>procedure <strong>MyProc1</strong>( a:Integer);</code>
</td>
</tr>
<tr><td colspan="1">

Below is an example of a very bad and confusing (but valid) macro usage. Just to test pasdoc.

</td></tr>
</table>

<table>
<tr>

<td>

<span id="MyProc2"/><code>procedure <strong>MyProc2</strong>( b: Integer);</code>
</td>
</tr>
<tr><td colspan="1">

This is very stupid way to declare a procedure

</td></tr>
</table>

<table>
<tr>

<td>

<span id="Foo"/><code>function <strong>Foo</strong>(c: string): Integer;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="MyProc3"/><code>procedure <strong>MyProc3</strong>( X: Integer = 1; Y: Integer = 2);</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="ThisShouldBeIncluded"/><code>procedure <strong>ThisShouldBeIncluded</strong>;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="ThisShouldBeIncluded2"/><code>procedure <strong>ThisShouldBeIncluded2</strong>;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

### Constants

<table>
<tr>

<td>

<span id="ThisShouldBeTrue"/><code><strong>ThisShouldBeTrue</strong> = true;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="FourConst"/><code><strong>FourConst</strong> =  (1 + 1) \* (1 + 1);</code>
</td>
</tr>
<tr><td colspan="1">

Test of recursive macro expansion.

</td></tr>
</table>

<table>
<tr>

<td>

<span id="OneAndNotNothing"/><code><strong>OneAndNotNothing</strong> = 1  + 1;</code>
</td>
</tr>
<tr><td colspan="1">

Test that symbol that is not a macro is something different than a macro that expands to nothing.

</td></tr>
</table>

<table>
<tr>

<td>

<span id="OnlyOne"/><code><strong>OnlyOne</strong> = 1 ;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>
