# ok\_param\_raises\_returns\_proctype


# Unit ok\_param\_raises\_returns\_proctype

- [Description](#PasDoc-Description)
- Uses
- [Classes, Interfaces, Objects and Records](#PasDoc-Classes)
- Functions and Procedures
- [Types](#PasDoc-Types)
- Constants
- Variables

<span id="PasDoc-Description"/>

## Description
This item has no description.

<span id="PasDoc-Uses"/>

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

Class&nbsp;[`EFoo`](ok_param_raises_returns_proctype.EFoo.md)
</td>

<td>

&nbsp;
</td>
</tr>
</table>

### Types
<span id="PasDoc-Types"/>


<table>
<tr>

<td>

<code><strong><a href="ok_param_raises_returns_proctype.md#TMyProcedure">TMyProcedure</a></strong> = procedure(A: integer);</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_param_raises_returns_proctype.md#TMyMethod">TMyMethod</a></strong> = function(A: integer): boolean of object;</code>
</td>
</tr>
</table>

## Description

### Types

<table>
<tr>

<td>

<span id="TMyProcedure"/><code><strong>TMyProcedure</strong> = procedure(A: integer);</code>
</td>
</tr>
<tr><td colspan="1">

 
###### Parameters
<dl>
<dt>A</dt>
<dd>

Description of param A</dd>
</dl>

###### Exceptions raised
<dl>
<dt>[EFoo](ok_param_raises_returns_proctype.EFoo.md)</dt>
<dd>

Description when EFoo is raised</dd>
</dl>


</td></tr>
</table>

<table>
<tr>

<td>

<span id="TMyMethod"/><code><strong>TMyMethod</strong> = function(A: integer): boolean of object;</code>
</td>
</tr>
<tr><td colspan="1">

  
###### Parameters
<dl>
<dt>A</dt>
<dd>

Description of param A</dd>
</dl>

###### Returns
<p class="return">`True` or `False`</p>
###### Exceptions raised
<dl>
<dt>[EFoo](ok_param_raises_returns_proctype.EFoo.md)</dt>
<dd>

Description when EFoo is raised</dd>
</dl>


</td></tr>
</table>
