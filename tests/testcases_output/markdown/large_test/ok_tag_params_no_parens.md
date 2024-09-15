# ok\_tag\_params\_no\_parens


# Unit ok\_tag\_params\_no\_parens

- [Description](#PasDoc-Description)
- Uses
- [Classes, Interfaces, Objects and Records](#PasDoc-Classes)
- [Functions and Procedures](#PasDoc-FuncsProcs)
- Types
- Constants
- Variables

<span id="PasDoc-Description"/>

## Description
This is a demo unit using tags without enclosing them in ()</p>
<p>


Parsing logic is simple: if a tag requires some parameters but you don't put open paren '(' char right after it, then tag parameters are understood to span to the end of line (or to the end of comment).

This doesn't break compatibility with documentation that enclosed parameters in (), because tags that have parameters were \*required\* to have '(' char after them. So they will still be correctly seen and parsed to the matching closing paren.

You can even have multiline params without parens with the help of &quot;line feed&quot; char \\ (just like shell scripts, C lang etc.)

See [SomeProc](ok_tag_params_no_parens.md#SomeProc) for more examples and comments.

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

Class&nbsp;[`EFoo`](ok_tag_params_no_parens.EFoo.md)
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

<code>function <strong><a href="ok_tag_params_no_parens.md#SomeProc">SomeProc</a></strong>(A: Integer): Integer;</code>
</td>
</tr>
</table>

## Description

### Functions and Procedures

<table>
<tr>

<td>

<span id="SomeProc"/><code>function <strong>SomeProc</strong>(A: Integer): Integer;</code>
</td>
</tr>
<tr><td colspan="1">

Note that this rule allows you to not specify () for \*any\* tag that has parameters. Even for @link tag: [ok\_tag\_params\_no\_parens](ok_tag_params_no_parens.md)

This rule doesn't create any problems for tags without parameters, like the @name tag: here it is: `SomeProc`. Such tags never have parameters, and on the above line you \*don't\* have @name tag with parameters &quot;tag. Such tags never have parameters,&quot;. Instead, you just specified `SomeProc` tag and &quot;tag. Such tags never have parameters,&quot; is just a normal text.

Check out this longcode: 

```pascal
</p>

<pre class="longcode">**begin** Writeln(*'Hello world'*); **end**; * **{ This works ! }** *</pre>

<p>
```



See also @html and @latex tags: <span style="color: #ff0000">I'm red</span> 

And here is some code: `begin X := Y + 1; end;`

  
###### Parameters
<dl>
<dt>A</dt>
<dd>

means something or maybe nothing</dd>
</dl>

###### Returns
<p class="return">Some integer and good wishes</p>
###### Exceptions raised
<dl>
<dt>[EBar](ok_expanding_descriptions.EBar.md)</dt>
<dd>

when it's in bad mood. But don't worry it won't happen too often. At least we hope so...</dd>
</dl>


</td></tr>
</table>

## Authors
- Michalis
- kambi


## Created


2005-05-04



## Last Modified


2005-05-04


