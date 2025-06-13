# ok\_vorbisfile


# Unit ok\_vorbisfile

- [Description](#PasDoc-Description)
- Uses
- [Classes, Interfaces, Objects and Records](#PasDoc-Classes)
- Functions and Procedures
- [Types](#PasDoc-Types)
- [Constants](#PasDoc-Constants)
- [Variables](#PasDoc-Variables)

<span id="PasDoc-Description"/>

## Description
API of vorbisfile library. Usually libvorbisfile.so under Unixes or vorbisfile.dll under Windows. This is just a quick translation of /usr/include/vorbis/vorbisfile.h header.<span id="PasDoc-Uses"/>

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

Record&nbsp;[`Tov_callbacks`](ok_vorbisfile.Tov_callbacks.md)
</td>

<td>

&nbsp;
</td>
</tr>
<tr>

<td>

Record&nbsp;[`TOggVorbis_File`](ok_vorbisfile.TOggVorbis_File.md)
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

<code><strong><a href="ok_vorbisfile.md#TSizeT">TSizeT</a></strong> = LongWord;</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_vorbisfile.md#TVorbisFileReadFunc">TVorbisFileReadFunc</a></strong> = function (ptr: Pointer; Size: <a href="ok_vorbisfile.md#TSizeT">TSizeT</a>; nmemb: <a href="ok_vorbisfile.md#TSizeT">TSizeT</a>; DataSource: Pointer): <a href="ok_vorbisfile.md#TSizeT">TSizeT</a>; cdecl;</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_vorbisfile.md#TVorbisFileSeekFunc">TVorbisFileSeekFunc</a></strong> = function (DataSource: Pointer; offset: Int64; whence: CInt): CInt; cdecl;</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_vorbisfile.md#TVorbisFileCloseFunc">TVorbisFileCloseFunc</a></strong> = function (DataSource: Pointer): CInt; cdecl;</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_vorbisfile.md#TVorbisFileTellFunc">TVorbisFileTellFunc</a></strong> = function (DataSource: Pointer): CLong; cdecl;</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_vorbisfile.md#Pov_callbacks">Pov\_callbacks</a></strong> = &circ;<a href="ok_vorbisfile.Tov_callbacks.md">Tov\_callbacks</a>;</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_vorbisfile.md#POggVorbis_File">POggVorbis\_File</a></strong> = &circ;<a href="ok_vorbisfile.TOggVorbis_File.md">TOggVorbis\_File</a>;</code>
</td>
</tr>
</table>

### Constants
<span id="PasDoc-Constants"/>


<table>
<tr>

<td>

<code><strong><a href="ok_vorbisfile.md#NOTOPEN">NOTOPEN</a></strong>   = 0;</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_vorbisfile.md#PARTOPEN">PARTOPEN</a></strong>  = 1;</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_vorbisfile.md#OPENED">OPENED</a></strong>    = 2;</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_vorbisfile.md#STREAMSET">STREAMSET</a></strong> = 3;</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_vorbisfile.md#INITSET">INITSET</a></strong>   = 4;</code>
</td>
</tr>
</table>

### Variables
<span id="PasDoc-Variables"/>


<table>
<tr>

<td>

<code><strong><a href="ok_vorbisfile.md#ov_clear">ov\_clear</a></strong>: function(Vf: <a href="ok_vorbisfile.md#POggVorbis_File">POggVorbis\_File</a>): CInt; cdecl;</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_vorbisfile.md#VorbisFileInited">VorbisFileInited</a></strong>: boolean;</code>
</td>
</tr>
</table>

## Description

### Types

<table>
<tr>

<td>

<span id="TSizeT"/><code><strong>TSizeT</strong> = LongWord;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="TVorbisFileReadFunc"/><code><strong>TVorbisFileReadFunc</strong> = function (ptr: Pointer; Size: <a href="ok_vorbisfile.md#TSizeT">TSizeT</a>; nmemb: <a href="ok_vorbisfile.md#TSizeT">TSizeT</a>; DataSource: Pointer): <a href="ok_vorbisfile.md#TSizeT">TSizeT</a>; cdecl;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="TVorbisFileSeekFunc"/><code><strong>TVorbisFileSeekFunc</strong> = function (DataSource: Pointer; offset: Int64; whence: CInt): CInt; cdecl;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="TVorbisFileCloseFunc"/><code><strong>TVorbisFileCloseFunc</strong> = function (DataSource: Pointer): CInt; cdecl;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="TVorbisFileTellFunc"/><code><strong>TVorbisFileTellFunc</strong> = function (DataSource: Pointer): CLong; cdecl;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="Pov_callbacks"/><code><strong>Pov\_callbacks</strong> = &circ;<a href="ok_vorbisfile.Tov_callbacks.md">Tov\_callbacks</a>;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="POggVorbis_File"/><code><strong>POggVorbis\_File</strong> = &circ;<a href="ok_vorbisfile.TOggVorbis_File.md">TOggVorbis\_File</a>;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

### Constants

<table>
<tr>

<td>

<span id="NOTOPEN"/><code><strong>NOTOPEN</strong>   = 0;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="PARTOPEN"/><code><strong>PARTOPEN</strong>  = 1;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="OPENED"/><code><strong>OPENED</strong>    = 2;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="STREAMSET"/><code><strong>STREAMSET</strong> = 3;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="INITSET"/><code><strong>INITSET</strong>   = 4;</code>
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

<span id="ov_clear"/><code><strong>ov\_clear</strong>: function(Vf: <a href="ok_vorbisfile.md#POggVorbis_File">POggVorbis\_File</a>): CInt; cdecl;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>

<table>
<tr>

<td>

<span id="VorbisFileInited"/><code><strong>VorbisFileInited</strong>: boolean;</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>
