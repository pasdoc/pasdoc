# ok\_directive\_as\_identifier


# Unit ok\_directive\_as\_identifier

- [Description](#PasDoc-Description)
- Uses
- [Classes, Interfaces, Objects and Records](#PasDoc-Classes)
- [Functions and Procedures](#PasDoc-FuncsProcs)
- Types
- Constants
- Variables

<span id="PasDoc-Description"/>

## Description
All calling-convention specifiers must \*not\* be made links in docs. But &quot;Register&quot; procedure name must be made a link. Yes, the difficulty is here that &quot;register&quot; is once a calling-convention specifier and once a procedure name.

This is related to bug submitted to pasdoc-main list \[[http://sourceforge.net/mailarchive/message.php?msg\_id=11397611](http://sourceforge.net/mailarchive/message.php?msg_id=11397611)\].<span id="PasDoc-Uses"/>

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

Class&nbsp;[`TMyClass`](ok_directive_as_identifier.TMyClass.md)
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

<code>procedure <strong><a href="ok_directive_as_identifier.md#Bar">Bar</a></strong>; cdecl;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_directive_as_identifier.md#Cdecl">Cdecl</a></strong>; register;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_directive_as_identifier.md#Foo">Foo</a></strong>; register;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_directive_as_identifier.md#Foo1">Foo1</a></strong>(const S: string = 'register'; MyClass: <a href="ok_directive_as_identifier.TMyClass.md">TMyClass</a>);</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_directive_as_identifier.md#Register">Register</a></strong>; register;</code>
</td>
</tr>
</table>

## Description

### Functions and Procedures

<table>
<tr>

<td>

<span id="Bar"/><code>procedure <strong>Bar</strong>; cdecl;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="Cdecl"/><code>procedure <strong>Cdecl</strong>; register;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="Foo"/><code>procedure <strong>Foo</strong>; register;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="Foo1"/><code>procedure <strong>Foo1</strong>(const S: string = 'register'; MyClass: <a href="ok_directive_as_identifier.TMyClass.md">TMyClass</a>);</code>
</td>
</tr>
<tr><td colspan="1">

Some other test for THTMLDocGenerator.WriteCodeWithLinks, while I'm at it:

Note that link to TMyClass should be correctly made. 'register' should be displayed as a string, of course, and not linked.

</td></tr>
</table>

<table>
<tr>

<td>

<span id="Register"/><code>procedure <strong>Register</strong>; register;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>
