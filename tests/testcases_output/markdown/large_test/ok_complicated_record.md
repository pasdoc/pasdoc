# ok\_complicated\_record


# Unit ok\_complicated\_record

- [Description](#PasDoc-Description)
- Uses
- [Classes, Interfaces, Objects and Records](#PasDoc-Classes)
- [Functions and Procedures](#PasDoc-FuncsProcs)
- [Types](#PasDoc-Types)
- Constants
- Variables

<span id="PasDoc-Description"/>

## Description
 This unit is used for converting to and from the BigEndian format. See [http://community.borland.com/article/0,1410,28964,00.html](http://community.borland.com/article/0,1410,28964,00.html).</p>
<p>
Submitted in thread &quot;Pasdoc tests&quot; on pasdoc-main on 2005-04-11 by Richard B Winston. pasdoc passes it, but the test checks many important things (line glueing single-line comments by pasdoc, record with case etc.) so it's worth adding it to test cases.

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

Packed Record&nbsp;[`TDoubleEndianCnvRec`](ok_complicated_record.TDoubleEndianCnvRec.md)
</td>

<td>

`TDoubleEndianCnvRec` is used in [ConvertDouble](ok_complicated_record.md#ConvertDouble) to convert a double to or from the BigEndian format.
</td>
</tr>
</table>

### Functions and Procedures
<span id="PasDoc-FuncsProcs"/>


<table>
<tr>

<td>

<code>function <strong><a href="ok_complicated_record.md#ConvertDouble">ConvertDouble</a></strong>(const Value: double): double;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_complicated_record.md#SwapDoubleBytes">SwapDoubleBytes</a></strong>(Dest, Source: <a href="ok_complicated_record.md#PDoubleEndianCnvRec">PDoubleEndianCnvRec</a>);</code>
</td>
</tr>
</table>

### Types
<span id="PasDoc-Types"/>


<table>
<tr>

<td>

<code><strong><a href="ok_complicated_record.md#BytePos">BytePos</a></strong> = (...);</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_complicated_record.md#PDoubleEndianCnvRec">PDoubleEndianCnvRec</a></strong> = &circ;<a href="ok_complicated_record.TDoubleEndianCnvRec.md">TDoubleEndianCnvRec</a>;</code>
</td>
</tr>
</table>

## Description

### Functions and Procedures

<table>
<tr>

<td>

<span id="ConvertDouble"/><code>function <strong>ConvertDouble</strong>(const Value: double): double;</code>
</td>
</tr>
<tr><td colspan="1">

`ConvertDouble` converts Value to or from the BigEndian format.</p>
<p>
  
###### Parameters
<dl>
<dt>Value</dt>
<dd>

is the value to be converted.</dd>
</dl>

###### Returns
<p class="return">Value after being converted to or from the BigEndian format.</p>

</td></tr>
</table>

<table>
<tr>

<td>

<span id="SwapDoubleBytes"/><code>procedure <strong>SwapDoubleBytes</strong>(Dest, Source: <a href="ok_complicated_record.md#PDoubleEndianCnvRec">PDoubleEndianCnvRec</a>);</code>
</td>
</tr>
<tr><td colspan="1">

`SwapDoubleBytes` copies [TDoubleEndianCnvRec.Bytes](ok_complicated_record.TDoubleEndianCnvRec.md#Bytes) in reverse order from Source&circ; to Dest&circ;.</p>
<p>
 `SwapDoubleBytes` is used in [ConvertDouble](ok_complicated_record.md#ConvertDouble).

</td></tr>
</table>

### Types

<table>
<tr>

<td>

<span id="BytePos"/><code><strong>BytePos</strong> = (...);</code>
</td>
</tr>
<tr><td colspan="1">

enumeration used in variant record
###### Values

- <span id="EndVal">EndVal</span>
- <span id="ByteVal">ByteVal</span>



</td></tr>
</table>

<table>
<tr>

<td>

<span id="PDoubleEndianCnvRec"/><code><strong>PDoubleEndianCnvRec</strong> = &circ;<a href="ok_complicated_record.TDoubleEndianCnvRec.md">TDoubleEndianCnvRec</a>;</code>
</td>
</tr>
<tr><td colspan="1">

`PDoubleEndianCnvRec` is a pointer to a [TDoubleEndianCnvRec](ok_complicated_record.TDoubleEndianCnvRec.md).

</td></tr>
</table>
