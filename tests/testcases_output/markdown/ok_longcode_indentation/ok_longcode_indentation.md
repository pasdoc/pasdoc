# ok\_longcode\_indentation


# Unit ok\_longcode\_indentation

- [Description](#PasDoc-Description)
- Uses
- Classes, Interfaces, Objects and Records
- [Functions and Procedures](#PasDoc-FuncsProcs)
- Types
- Constants
- Variables

<span id="PasDoc-Description"/>

## Description
This item has no description.

<span id="PasDoc-Uses"/>

## Overview

### Functions and Procedures
<span id="PasDoc-FuncsProcs"/>


<table>
<tr>

<td>

<code>procedure <strong><a href="ok_longcode_indentation.md#Foo1">Foo1</a></strong>;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_longcode_indentation.md#Foo2">Foo2</a></strong>;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_longcode_indentation.md#Foo3">Foo3</a></strong>;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_longcode_indentation.md#Foo4">Foo4</a></strong>;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_longcode_indentation.md#Foo5">Foo5</a></strong>;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_longcode_indentation.md#Foo6">Foo6</a></strong>;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_longcode_indentation.md#Foo7">Foo7</a></strong>;</code>
</td>
</tr>
</table>

## Description

### Functions and Procedures

<table>
<tr>

<td>

<span id="Foo1"/><code>procedure <strong>Foo1</strong>;</code>
</td>
</tr>
<tr><td colspan="1">

Should cut nothing:



```pascal
</p>

<pre class="longcode">one
  two
    three</pre>

<p>
```



</td></tr>
</table>

<table>
<tr>

<td>

<span id="Foo2"/><code>procedure <strong>Foo2</strong>;</code>
</td>
</tr>
<tr><td colspan="1">

Should cut 4 spaces:



```pascal
</p>

<pre class="longcode">
one
two
three</pre>

<p>
```



</td></tr>
</table>

<table>
<tr>

<td>

<span id="Foo3"/><code>procedure <strong>Foo3</strong>;</code>
</td>
</tr>
<tr><td colspan="1">

Should cut 2 spaces:



```pascal
</p>

<pre class="longcode">
  one
two
  three</pre>

<p>
```



</td></tr>
</table>

<table>
<tr>

<td>

<span id="Foo4"/><code>procedure <strong>Foo4</strong>;</code>
</td>
</tr>
<tr><td colspan="1">

Should cut nothing (there's a tab here):



```pascal
</p>

<pre class="longcode">
    one
	two
    three</pre>

<p>
```



</td></tr>
</table>

<table>
<tr>

<td>

<span id="Foo5"/><code>procedure <strong>Foo5</strong>;</code>
</td>
</tr>
<tr><td colspan="1">

Should cut tab + 2 spaces:



```pascal
</p>

<pre class="longcode">
one
  two
three</pre>

<p>
```



</td></tr>
</table>

<table>
<tr>

<td>

<span id="Foo6"/><code>procedure <strong>Foo6</strong>;</code>
</td>
</tr>
<tr><td colspan="1">

Should cut 4 spaces (there's trailing whitespace in 1st lines here, that should be ignored):



```pascal
</p>

<pre class="longcode">
                 
                  
one</pre>

<p>
```



</td></tr>
</table>

<table>
<tr>

<td>

<span id="Foo7"/><code>procedure <strong>Foo7</strong>;</code>
</td>
</tr>
<tr><td colspan="1">

Should cut 4 spaces (empty line doesn't shorten IndentationPrefix):



```pascal
</p>

<pre class="longcode">
**if** something **then**
**begin**
  * **// empty line below** *

**end**;</pre>

<p>
```



</td></tr>
</table>
