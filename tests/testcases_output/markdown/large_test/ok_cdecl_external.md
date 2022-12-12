# ok\_cdecl\_external


# Unit ok\_cdecl\_external

- [Description](#PasDoc-Description)
- Uses
- Classes, Interfaces, Objects and Records
- [Functions and Procedures](#PasDoc-FuncsProcs)
- Types
- Constants
- Variables

<span id="PasDoc-Description"/>

## Description
Bug:

Parsing of this unit fails with Warning\[2\]: Error EPasDoc: todo/ok\_cdecl\_external.pas(5): Unexpected keyword external. parsing unit ok\_cdecl\_external.pas, continuing...

Bar and Xyz added as additional tests.

Update: it's fixed now, pasdoc parses it correctly.<span id="PasDoc-Uses"/>

## Overview

### Functions and Procedures
<span id="PasDoc-FuncsProcs"/>


<table>
<tr>

<td>

<code>procedure <strong><a href="ok_cdecl_external.md#Bar">Bar</a></strong>; cdecl; external 'bar\_library\_name' name 'bar\_name\_in\_library';</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_cdecl_external.md#Foo">Foo</a></strong>; cdecl; external 'whatever';</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_cdecl_external.md#Xyz">Xyz</a></strong>; external 'xyz\_library\_name' name 'xyz\_name\_in\_library'; cdecl;</code>
</td>
</tr>
</table>

## Description

### Functions and Procedures

<table>
<tr>

<td>

<span id="Bar"/><code>procedure <strong>Bar</strong>; cdecl; external 'bar\_library\_name' name 'bar\_name\_in\_library';</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="Foo"/><code>procedure <strong>Foo</strong>; cdecl; external 'whatever';</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="Xyz"/><code>procedure <strong>Xyz</strong>; external 'xyz\_library\_name' name 'xyz\_name\_in\_library'; cdecl;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>
