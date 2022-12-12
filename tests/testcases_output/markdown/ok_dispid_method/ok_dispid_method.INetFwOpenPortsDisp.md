# ok\_dispid\_method: DispInterface INetFwOpenPortsDisp


# DispInterface INetFwOpenPortsDisp
<span id="INetFwOpenPortsDisp"/>

- [Description](#PasDoc-Description)
- [Hierarchy](#PasDoc-Hierarchy)
- Fields
- [Methods](#PasDoc-Methods)
- [Properties](#PasDoc-Properties)

<span id="PasDoc-Description"/>

## Unit


[ok\_dispid\_method](ok_dispid_method.md)


## Declaration


```type INetFwOpenPortsDisp = dispinterface(IDispInterface)```


## Description
This item has no description.


###### Attributes
<dl class="attributes">
  <dt>GUID\['{C0E9D7FA-E07E-430A-B19A-090CE82D92E2}'\]</dt>
  <dd>


</dd>
</dl>


## Hierarchy


<span id="PasDoc-Hierarchy"/>

- IDispInterface
- INetFwOpenPortsDisp



## Overview

### Methods
<span id="PasDoc-Methods"/>


<table>
<tr>

<td>

<a href="legend.md"><img src="public.gif" alt="Public" title="Public"></img></a>
</td>

<td>

<code>procedure <strong><a href="ok_dispid_method.INetFwOpenPortsDisp.md#Add">Add</a></strong>(const Port: INetFwOpenPort); dispid 2; </code>
</td>
</tr>
<tr>

<td>

<a href="legend.md"><img src="public.gif" alt="Public" title="Public"></img></a>
</td>

<td>

<code>procedure <strong><a href="ok_dispid_method.INetFwOpenPortsDisp.md#Remove">Remove</a></strong>(portNumber: Integer; ipProtocol: NET\_FW\_IP\_PROTOCOL\_); dispid 3; </code>
</td>
</tr>
<tr>

<td>

<a href="legend.md"><img src="public.gif" alt="Public" title="Public"></img></a>
</td>

<td>

<code>function <strong><a href="ok_dispid_method.INetFwOpenPortsDisp.md#Item">Item</a></strong>(portNumber: Integer; ipProtocol: NET\_FW\_IP\_PROTOCOL\_): INetFwOpenPort; dispid 4; </code>
</td>
</tr>
</table>

### Properties
<span id="PasDoc-Properties"/>


<table>
<tr>

<td>

<a href="legend.md"><img src="public.gif" alt="Public" title="Public"></img></a>
</td>

<td>

<code>property <strong><a href="ok_dispid_method.INetFwOpenPortsDisp.md#Count">Count</a></strong>: Integer readonly dispid 1;</code>
</td>
</tr>
<tr>

<td>

<a href="legend.md"><img src="public.gif" alt="Public" title="Public"></img></a>
</td>

<td>

<code>property <strong><a href="ok_dispid_method.INetFwOpenPortsDisp.md#_NewEnum">\_NewEnum</a></strong>: IUnknown readonly dispid -4;</code>
</td>
</tr>
</table>


## Description

### Methods

<table>
<tr>

<td>

<a href="legend.md"><img src="public.gif" alt="Public" title="Public"></img></a>
</td>

<td>

<span id="Add"/><code>procedure <strong>Add</strong>(const Port: INetFwOpenPort); dispid 2; </code>
</td>
</tr>
<tr><td colspan="2">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<a href="legend.md"><img src="public.gif" alt="Public" title="Public"></img></a>
</td>

<td>

<span id="Remove"/><code>procedure <strong>Remove</strong>(portNumber: Integer; ipProtocol: NET\_FW\_IP\_PROTOCOL\_); dispid 3; </code>
</td>
</tr>
<tr><td colspan="2">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<a href="legend.md"><img src="public.gif" alt="Public" title="Public"></img></a>
</td>

<td>

<span id="Item"/><code>function <strong>Item</strong>(portNumber: Integer; ipProtocol: NET\_FW\_IP\_PROTOCOL\_): INetFwOpenPort; dispid 4; </code>
</td>
</tr>
<tr><td colspan="2">

This item has no description.



</td></tr>
</table>

### Properties

<table>
<tr>

<td>

<a href="legend.md"><img src="public.gif" alt="Public" title="Public"></img></a>
</td>

<td>

<span id="Count"/><code>property <strong>Count</strong>: Integer readonly dispid 1;</code>
</td>
</tr>
<tr><td colspan="2">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<a href="legend.md"><img src="public.gif" alt="Public" title="Public"></img></a>
</td>

<td>

<span id="_NewEnum"/><code>property <strong>\_NewEnum</strong>: IUnknown readonly dispid -4;</code>
</td>
</tr>
<tr><td colspan="2">

This item has no description.



</td></tr>
</table>

