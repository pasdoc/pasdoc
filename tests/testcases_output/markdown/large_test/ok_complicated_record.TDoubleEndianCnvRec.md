# ok\_complicated\_record: Packed Record TDoubleEndianCnvRec


# Packed Record TDoubleEndianCnvRec
<span id="TDoubleEndianCnvRec"/>

- [Description](#PasDoc-Description)
- Hierarchy
- [Fields](#PasDoc-Fields)
- Methods
- Properties

<span id="PasDoc-Description"/>

## Unit


[ok\_complicated\_record](ok_complicated_record.md)


## Declaration


```type TDoubleEndianCnvRec = packed record```


## Description
`TDoubleEndianCnvRec` is used in [ConvertDouble](ok_complicated_record.md#ConvertDouble) to convert a double to or from the BigEndian format.</p>
<p>
 

```pascal
</p>

<pre class="longcode">
TDoubleEndianCnvRec = **packed** **record**
  **case** BytePos **of**
    EndVal: (EndianVal: double);
    ByteVal: (Bytes: **array**\[0..SizeOf(double) - 1\] **of** byte);
**end**;</pre>

<p>
```



## Overview

### Fields
<span id="PasDoc-Fields"/>


<table>
<tr>

<td>

<a href="legend.md"><img src="public.gif" alt="Public" title="Public"></img></a>
</td>

<td>

<code><strong><a href="ok_complicated_record.TDoubleEndianCnvRec.md#Bytes">Bytes</a></strong>: array\[0..SizeOf(double) - 1\] of byte</code>
</td>
</tr>
<tr>

<td>

<a href="legend.md"><img src="public.gif" alt="Public" title="Public"></img></a>
</td>

<td>

<code><strong><a href="ok_complicated_record.TDoubleEndianCnvRec.md#EndianVal">EndianVal</a></strong>: double</code>
</td>
</tr>
</table>


## Description

### Fields

<table>
<tr>

<td>

<a href="legend.md"><img src="public.gif" alt="Public" title="Public"></img></a>
</td>

<td>

<span id="Bytes"/><code><strong>Bytes</strong>: array\[0..SizeOf(double) - 1\] of byte</code>
</td>
</tr>
<tr><td colspan="2">

Overlapping bytes of the double

</td></tr>
</table>

<table>
<tr>

<td>

<a href="legend.md"><img src="public.gif" alt="Public" title="Public"></img></a>
</td>

<td>

<span id="EndianVal"/><code><strong>EndianVal</strong>: double</code>
</td>
</tr>
<tr><td colspan="2">

The value we are trying to convert

</td></tr>
</table>

