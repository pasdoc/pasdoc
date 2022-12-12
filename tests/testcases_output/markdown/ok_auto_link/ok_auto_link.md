# ok\_auto\_link


# Unit ok\_auto\_link

- [Description](#PasDoc-Description)
- Uses
- [Classes, Interfaces, Objects and Records](#PasDoc-Classes)
- Functions and Procedures
- Types
- [Constants](#PasDoc-Constants)
- [Variables](#PasDoc-Variables)

<span id="PasDoc-Description"/>

## Description
Auto-link tests.</p>
<p>


Self name: `ok\_auto\_link`, simple identifiers [Var1](ok_auto_link.md#Var1), qualified ident: [ok\_auto\_link.Var1](ok_auto_link.md#Var1).

Ident that can't be auto-linked: no\_auto\_link\_to\_me. Explicit link to ident that can't be auto-linked: [no\_auto\_link\_to\_me](ok_auto_link.md#no_auto_link_to_me).<span id="PasDoc-Uses"/>

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

Class&nbsp;[`TMyClass`](ok_auto_link.TMyClass.md)
</td>

<td>

&nbsp;
</td>
</tr>
</table>

### Constants
<span id="PasDoc-Constants"/>


<table>
<tr>

<td>

<code><strong><a href="ok_auto_link.md#no_auto_link_to_me">no\_auto\_link\_to\_me</a></strong> = 1;</code>
</td>
</tr>
</table>

### Variables
<span id="PasDoc-Variables"/>


<table>
<tr>

<td>

<code><strong><a href="ok_auto_link.md#Var1">Var1</a></strong>: Integer;</code>
</td>
</tr>
</table>

## Description

### Constants

<table>
<tr>

<td>

<span id="no_auto_link_to_me"/><code><strong>no\_auto\_link\_to\_me</strong> = 1;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

### Variables

<table>
<tr>

<td>

<span id="Var1"/><code><strong>Var1</strong>: Integer;</code>
</td>
</tr>
<tr><td colspan="1">



-   **Test of auto-linking:**: Self name is `Var1`, simple ident is [TMyClass](ok_auto_link.TMyClass.md), qualified ident is [TMyClass.Field](ok_auto_link.TMyClass.md#Field).

Ident that can't be auto-linked: no\_auto\_link\_to\_me. Explicit link to ident that can't be auto-linked: [no\_auto\_link\_to\_me](ok_auto_link.md#no_auto_link_to_me).

Note that auto-linking works also inside @code:

` Self name is `Var1`, simple ident is [TMyClass](ok_auto_link.TMyClass.md), qualified ident is [TMyClass.Field](ok_auto_link.TMyClass.md#Field). ` 

-   **Test of @noAutoLink:**: Things below should *not* be converted to links:

 Self name is Var1, simple ident is TMyClass, qualified ident is TMyClass.Field.

Ident that can't be auto-linked: no\_auto\_link\_to\_me. Explicit link to ident that can't be auto-linked: [no\_auto\_link\_to\_me](ok_auto_link.md#no_auto_link_to_me).

Simple ident once again TMyClass.

Inside @code:

` Self name is Var1, simple ident is TMyClass, qualified ident is TMyClass.Field.`  





</td></tr>
</table>
