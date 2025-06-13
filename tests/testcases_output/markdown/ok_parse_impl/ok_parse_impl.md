# ok\_parse\_impl


# Unit ok\_parse\_impl

- [Description](#PasDoc-Description)
- Uses
- [Classes, Interfaces, Objects and Records](#PasDoc-Classes)
- [Functions and Procedures](#PasDoc-FuncsProcs)
- Types
- Constants
- Variables

<span id="PasDoc-Description"/>

## Description
Parse implementation section. Must be tested with --implementation-comments=join --define PASDOC<span id="PasDoc-Uses"/>

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

Class&nbsp;[`TClass`](ok_parse_impl.TClass.md)
</td>

<td>

&nbsp;
</td>
</tr>
<tr>

<td>

Record&nbsp;[`TMyType`](ok_parse_impl.TMyType.md)
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

<code>procedure <strong><a href="ok_parse_impl.md#Foo">Foo</a></strong>;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_parse_impl.md#Bar">Bar</a></strong>;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_parse_impl.md#Laz">Laz</a></strong>;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_parse_impl.md#Dex">Dex</a></strong>;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_parse_impl.md#NoDescr">NoDescr</a></strong>;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_parse_impl.md#Overloaded">Overloaded</a></strong>; overload;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_parse_impl.md#Overloaded">Overloaded</a></strong>(a: Byte); overload;</code>
</td>
</tr>
<tr>

<td>

<code>Operator := (C : <a href="ok_parse_impl.TMyType.md">TMyType</a>) z : <a href="ok_parse_impl.TMyType.md">TMyType</a>;</code>
</td>
</tr>
</table>

## Description

### Functions and Procedures

<table>
<tr>

<td>

<span id="Foo"/><code>procedure <strong>Foo</strong>;</code>
</td>
</tr>
<tr><td colspan="1">

This is Foo (intf) This is Foo (impl)

</td></tr>
</table>

<table>
<tr>

<td>

<span id="Bar"/><code>procedure <strong>Bar</strong>;</code>
</td>
</tr>
<tr><td colspan="1">

This is Bar (impl)

</td></tr>
</table>

<table>
<tr>

<td>

<span id="Laz"/><code>procedure <strong>Laz</strong>;</code>
</td>
</tr>
<tr><td colspan="1">

This is Laz And it must not be doubled

</td></tr>
</table>

<table>
<tr>

<td>

<span id="Dex"/><code>procedure <strong>Dex</strong>;</code>
</td>
</tr>
<tr><td colspan="1">

This is Dex described inside method body

</td></tr>
</table>

<table>
<tr>

<td>

<span id="NoDescr"/><code>procedure <strong>NoDescr</strong>;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="Overloaded"/><code>procedure <strong>Overloaded</strong>; overload;</code>
</td>
</tr>
<tr><td colspan="1">

This is overloaded proc #1

</td></tr>
</table>

<table>
<tr>

<td>

<span id="Overloaded"/><code>procedure <strong>Overloaded</strong>(a: Byte); overload;</code>
</td>
</tr>
<tr><td colspan="1">

This is overloaded proc #2

</td></tr>
</table>

<table>
<tr>

<td>

<span id=":="/><code>Operator := (C : <a href="ok_parse_impl.TMyType.md">TMyType</a>) z : <a href="ok_parse_impl.TMyType.md">TMyType</a>;</code>
</td>
</tr>
<tr><td colspan="1">

This is assignment operator

</td></tr>
</table>
