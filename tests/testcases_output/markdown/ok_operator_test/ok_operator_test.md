# ok\_operator\_test


# Unit ok\_operator\_test

- [Description](#PasDoc-Description)
- Uses
- [Classes, Interfaces, Objects and Records](#PasDoc-Classes)
- [Functions and Procedures](#PasDoc-FuncsProcs)
- Types
- Constants
- Variables

<span id="PasDoc-Description"/>

## Description
Operator overloads Delphi and FPC<span id="PasDoc-Uses"/>

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

Record&nbsp;[`TDelphiRec`](ok_operator_test.TDelphiRec.md)
</td>

<td>

Operator overloads declared within a record (Delphi 2006+)
</td>
</tr>
<tr>

<td>

Class&nbsp;[`TMyClass`](ok_operator_test.TMyClass.md)
</td>

<td>

In this case, &quot;Operator&quot; is used as a normal Delphi identifier
</td>
</tr>
<tr>

<td>

Record&nbsp;[`TMyType`](ok_operator_test.TMyType.md)
</td>

<td>

&nbsp;
</td>
</tr>
<tr>

<td>

Record&nbsp;[`TMyType2`](ok_operator_test.TMyType2.md)
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

<code>Operator := (C : <a href="ok_operator_test.TMyType2.md">TMyType2</a>) z : <a href="ok_operator_test.TMyType.md">TMyType</a>;</code>
</td>
</tr>
<tr>

<td>

<code>Operator + (c: <a href="ok_operator_test.TMyType.md">TMyType</a>; c1: <a href="ok_operator_test.TMyType.md">TMyType</a>) c2: <a href="ok_operator_test.TMyType.md">TMyType</a>;</code>
</td>
</tr>
<tr>

<td>

<code>Operator - (c: <a href="ok_operator_test.TMyType.md">TMyType</a>; c1: <a href="ok_operator_test.TMyType.md">TMyType</a>) c2: <a href="ok_operator_test.TMyType.md">TMyType</a>;</code>
</td>
</tr>
<tr>

<td>

<code>Operator \* (c: <a href="ok_operator_test.TMyType.md">TMyType</a>; i: integer) c2: <a href="ok_operator_test.TMyType.md">TMyType</a>;</code>
</td>
</tr>
<tr>

<td>

<code>Operator / (A, B: <a href="ok_operator_test.TMyType.md">TMyType</a>): <a href="ok_operator_test.TMyType.md">TMyType</a>;</code>
</td>
</tr>
<tr>

<td>

<code>Operator \*\* (A, B: <a href="ok_operator_test.TMyType.md">TMyType</a>): <a href="ok_operator_test.TMyType.md">TMyType</a>;</code>
</td>
</tr>
<tr>

<td>

<code>operator = (const c, d: <a href="ok_operator_test.TMyType.md">TMyType</a>) : boolean;</code>
</td>
</tr>
<tr>

<td>

<code>operator < (const c, d: <a href="ok_operator_test.TMyType.md">TMyType</a>) : boolean;</code>
</td>
</tr>
<tr>

<td>

<code>operator > (const c, d: <a href="ok_operator_test.TMyType.md">TMyType</a>) : boolean;</code>
</td>
</tr>
<tr>

<td>

<code>operator <= (const c, d: <a href="ok_operator_test.TMyType.md">TMyType</a>) : boolean;</code>
</td>
</tr>
<tr>

<td>

<code>operator >= (const c, d: <a href="ok_operator_test.TMyType.md">TMyType</a>) : boolean;</code>
</td>
</tr>
<tr>

<td>

<code>operator <strong><a href="ok_operator_test.md#or">or</a></strong> (const c,d:<a href="ok_operator_test.TMyType.md">TMyType</a>) : <a href="ok_operator_test.TMyType.md">TMyType</a>;</code>
</td>
</tr>
<tr>

<td>

<code>operator <strong><a href="ok_operator_test.md#and">and</a></strong> (const c,d:<a href="ok_operator_test.TMyType.md">TMyType</a>) : <a href="ok_operator_test.TMyType.md">TMyType</a>;</code>
</td>
</tr>
<tr>

<td>

<code>operator <strong><a href="ok_operator_test.md#xor">xor</a></strong> (const c,d:<a href="ok_operator_test.TMyType.md">TMyType</a>) : <a href="ok_operator_test.TMyType.md">TMyType</a>;</code>
</td>
</tr>
</table>

## Description

### Functions and Procedures

<table>
<tr>

<td>

<span id=":="/><code>Operator := (C : <a href="ok_operator_test.TMyType2.md">TMyType2</a>) z : <a href="ok_operator_test.TMyType.md">TMyType</a>;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="+"/><code>Operator + (c: <a href="ok_operator_test.TMyType.md">TMyType</a>; c1: <a href="ok_operator_test.TMyType.md">TMyType</a>) c2: <a href="ok_operator_test.TMyType.md">TMyType</a>;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="-"/><code>Operator - (c: <a href="ok_operator_test.TMyType.md">TMyType</a>; c1: <a href="ok_operator_test.TMyType.md">TMyType</a>) c2: <a href="ok_operator_test.TMyType.md">TMyType</a>;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="*"/><code>Operator \* (c: <a href="ok_operator_test.TMyType.md">TMyType</a>; i: integer) c2: <a href="ok_operator_test.TMyType.md">TMyType</a>;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="/"/><code>Operator / (A, B: <a href="ok_operator_test.TMyType.md">TMyType</a>): <a href="ok_operator_test.TMyType.md">TMyType</a>;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="**"/><code>Operator \*\* (A, B: <a href="ok_operator_test.TMyType.md">TMyType</a>): <a href="ok_operator_test.TMyType.md">TMyType</a>;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="="/><code>operator = (const c, d: <a href="ok_operator_test.TMyType.md">TMyType</a>) : boolean;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="<"/><code>operator < (const c, d: <a href="ok_operator_test.TMyType.md">TMyType</a>) : boolean;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id=">"/><code>operator > (const c, d: <a href="ok_operator_test.TMyType.md">TMyType</a>) : boolean;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="<="/><code>operator <= (const c, d: <a href="ok_operator_test.TMyType.md">TMyType</a>) : boolean;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id=">="/><code>operator >= (const c, d: <a href="ok_operator_test.TMyType.md">TMyType</a>) : boolean;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="or"/><code>operator <strong>or</strong> (const c,d:<a href="ok_operator_test.TMyType.md">TMyType</a>) : <a href="ok_operator_test.TMyType.md">TMyType</a>;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="and"/><code>operator <strong>and</strong> (const c,d:<a href="ok_operator_test.TMyType.md">TMyType</a>) : <a href="ok_operator_test.TMyType.md">TMyType</a>;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="xor"/><code>operator <strong>xor</strong> (const c,d:<a href="ok_operator_test.TMyType.md">TMyType</a>) : <a href="ok_operator_test.TMyType.md">TMyType</a>;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>
