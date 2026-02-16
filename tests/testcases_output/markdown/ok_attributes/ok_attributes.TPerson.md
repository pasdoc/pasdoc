# ok\_attributes: Class TPerson


# Class TPerson
<span id="TPerson"/>

- [Description](#PasDoc-Description)
- [Hierarchy](#PasDoc-Hierarchy)
- Fields
- Methods
- [Properties](#PasDoc-Properties)

<span id="PasDoc-Description"/>

## Unit


[ok\_attributes](ok_attributes.md)


## Declaration


```type TPerson = class(TObject)```


## Description
This item has no description.



## Hierarchy


<span id="PasDoc-Hierarchy"/>

- TObject
- TPerson



## Overview

### Properties
<span id="PasDoc-Properties"/>


<table>
<tr>

<td>

<a href="legend.md"><img src="public.gif" alt="Public" title="Public"></img></a>
</td>

<td>

<code>property <strong><a href="ok_attributes.TPerson.md#Name">Name</a></strong> : String read FName write FName;</code>
</td>
</tr>
<tr>

<td>

<a href="legend.md"><img src="public.gif" alt="Public" title="Public"></img></a>
</td>

<td>

<code>property <strong><a href="ok_attributes.TPerson.md#Age">Age</a></strong> : Integer read FAge write FAge;</code>
</td>
</tr>
</table>


## Description

### Properties

<table>
<tr>

<td>

<a href="legend.md"><img src="public.gif" alt="Public" title="Public"></img></a>
</td>

<td>

<span id="Name"/><code>property <strong>Name</strong> : String read FName write FName;</code>
</td>
</tr>
<tr><td colspan="2">

This item has no description.


###### Attributes
<dl class="attributes">
  <dt>`NonEmptyString`('Must provide a Name')</dt>
  <dd>


</dd>
</dl>


</td></tr>
</table>

<table>
<tr>

<td>

<a href="legend.md"><img src="public.gif" alt="Public" title="Public"></img></a>
</td>

<td>

<span id="Age"/><code>property <strong>Age</strong> : Integer read FAge write FAge;</code>
</td>
</tr>
<tr><td colspan="2">

This item has no description.


###### Attributes
<dl class="attributes">
  <dt>`MinimumInteger`(18, 'Must be at least 18 years old')</dt>
  <dd>


</dd>
  <dt>`MaximumInteger`(65, 'Must be no older than 65 years')</dt>
  <dd>


</dd>
</dl>


</td></tr>
</table>

