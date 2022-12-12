# ok\_expanding\_descriptions


# Unit ok\_expanding\_descriptions

- [Description](#PasDoc-Description)
- Uses
- [Classes, Interfaces, Objects and Records](#PasDoc-Classes)
- [Functions and Procedures](#PasDoc-FuncsProcs)
- Types
- Constants
- Variables

<span id="PasDoc-Description"/>

## Description
This is a test of tags expanded by TPasItem handlers. Of course with @abstract tag using some recursive tag: See also [TestPasMethodTags](ok_expanding_descriptions.md#TestPasMethodTags)</p>
<p>


This whole unit is actually a big test of many things related to pasdoc's @-tags.

   

See also [TMyClass](ok_expanding_descriptions.TMyClass.md) for other test of @cvs tag (with $Date, as an alternative specification of @lastmod)<span id="PasDoc-Uses"/>

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

Class&nbsp;[`EFoo`](ok_expanding_descriptions.EFoo.md)
</td>

<td>

&nbsp;
</td>
</tr>
<tr>

<td>

Class&nbsp;[`EBar`](ok_expanding_descriptions.EBar.md)
</td>

<td>

&nbsp;
</td>
</tr>
<tr>

<td>

Class&nbsp;[`EXyz`](ok_expanding_descriptions.EXyz.md)
</td>

<td>

&nbsp;
</td>
</tr>
<tr>

<td>

Class&nbsp;[`TMyClassAncestor`](ok_expanding_descriptions.TMyClassAncestor.md)
</td>

<td>

&nbsp;
</td>
</tr>
<tr>

<td>

Class&nbsp;[`TMyClass`](ok_expanding_descriptions.TMyClass.md)
</td>

<td>

These are some tags that are not allowed to have parameters: name `TMyClass`, inherited [TMyClassAncestor](ok_expanding_descriptions.TMyClassAncestor.md), nil `Nil`, true `True`, false `False`, classname `TMyClass`.
</td>
</tr>
</table>

### Functions and Procedures
<span id="PasDoc-FuncsProcs"/>


<table>
<tr>

<td>

<code>procedure <strong><a href="ok_expanding_descriptions.md#RecursiveTwoAt">RecursiveTwoAt</a></strong>;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_expanding_descriptions.md#TestHtmlAndLatexTags">TestHtmlAndLatexTags</a></strong>;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_expanding_descriptions.md#TestLongCode">TestLongCode</a></strong>;</code>
</td>
</tr>
<tr>

<td>

<code>function <strong><a href="ok_expanding_descriptions.md#TestPasMethodTags">TestPasMethodTags</a></strong>(A, B: Integer): string;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_expanding_descriptions.md#TestRecursiveTag">TestRecursiveTag</a></strong>;</code>
</td>
</tr>
<tr>

<td>

<code>procedure <strong><a href="ok_expanding_descriptions.md#TwoAt">TwoAt</a></strong>;</code>
</td>
</tr>
</table>

## Description

### Functions and Procedures

<table>
<tr>

<td>

<span id="RecursiveTwoAt"/><code>procedure <strong>RecursiveTwoAt</strong>;</code>
</td>
</tr>
<tr><td colspan="1">

aa aaaaa aa aaa `SHGetSpecialFolderPath(0, @Path, CSIDL\_APPDATA, true)` aaaa aaaaaa aaaaaa aaaaaaaaa aaaa

At some point, this test caused the bug: final </code> tag was inserted in converted form (processed with ConvertString) into html output. In effect, there was an opening <code> tag but there was no closing </code> tag.

</td></tr>
</table>

<table>
<tr>

<td>

<span id="TestHtmlAndLatexTags"/><code>procedure <strong>TestHtmlAndLatexTags</strong>;</code>
</td>
</tr>
<tr><td colspan="1">


    This is some <b>dummy</b> html code, just to show that inside
    @html tag of pasdoc (note that I used single @ char in this sentence)
    nothing is expanded by pasdoc.

    No paragraphs are created by pasdoc. (This text is still in the 1st,
    not in the 2nd, paragraph in html output)

    <p>You must explicitly write &lt;p&gt; to get paragraph.
    No tags work, e.g. @link(TestLongCode).
  



Note that text inside @html / @latex tags is absolutely not touched by pasdoc. Characters are not escaped (< is \*not\* changed to &amp;lt; in the html case), @tags are not expanded, @ needs not to be doubled, paragraphs (<p> in the html case) are not inserted.

</td></tr>
</table>

<table>
<tr>

<td>

<span id="TestLongCode"/><code>procedure <strong>TestLongCode</strong>;</code>
</td>
</tr>
<tr><td colspan="1">

Note that inside @longcode below I should be able to write singe @ char to get it in the output, no need to double it (like @@). No tags are expanded inside longcode.

Also note that paragraphs are not expanded inside longcode (no <p> inside <pre>...</pre> in html output).

Of course html characters are still correctly escaped (< changes to &amp;lt; etc.).



```pascal
</p>

<pre class="longcode">

**procedure** Foo;
**begin**
  **if** A < B **then** Bar; * **{ @link(No, this is not really pasdoc tag) }** *
**end**;

**procedure** Bar(X: Integer);
**begin**
  CompareMem(@X, @Y);
**end**;</pre>

<p>
```



</td></tr>
</table>

<table>
<tr>

<td>

<span id="TestPasMethodTags"/><code>function <strong>TestPasMethodTags</strong>(A, B: Integer): string;</code>
</td>
</tr>
<tr><td colspan="1">

This is a test of tags expanded by TPasRoutine handlers. Note that all three tags are expanded recursively.

 



 
###### Parameters
<dl>
<dt>A</dt>
<dd>

means sthg about [TestRecursiveTag](ok_expanding_descriptions.md#TestRecursiveTag)</dd>
<dt>B</dt>
<dd>

also means sthg. `Code inside.`</dd>
</dl>

###### Returns
<p class="return">You can make tags recursion any level deep : `This is a code with a link to [TestRecursiveTag](ok_expanding_descriptions.md#TestRecursiveTag)`</p>
###### Exceptions raised
<dl>
<dt>[EFoo](ok_expanding_descriptions.EFoo.md)</dt>
<dd>

when you do sthg nasty, like call [TestRecursiveTag](ok_expanding_descriptions.md#TestRecursiveTag) when you're not supposed to</dd>
<dt>[EBar](ok_expanding_descriptions.EBar.md)</dt>
<dd>

when code `if 1 = 0 then DoSomething;` will work as expected.</dd>
</dl>


</td></tr>
</table>

<table>
<tr>

<td>

<span id="TestRecursiveTag"/><code>procedure <strong>TestRecursiveTag</strong>;</code>
</td>
</tr>
<tr><td colspan="1">

@code and @returns (and some others) tags are recursive, you can freely put other tags inside.

`This is link to [TestHtmlAndLatexTags](ok_expanding_descriptions.md#TestHtmlAndLatexTags).`


###### Exceptions raised
<dl>
<dt>[EFoo](ok_expanding_descriptions.EFoo.md)</dt>
<dd>

in case [TestHtmlAndLatexTags](ok_expanding_descriptions.md#TestHtmlAndLatexTags) returns value >= 4 (actually, this is just a test text).</dd>
</dl>


</td></tr>
</table>

<table>
<tr>

<td>

<span id="TwoAt"/><code>procedure <strong>TwoAt</strong>;</code>
</td>
</tr>
<tr><td colspan="1">

Write two at chars, like this @@, to get one @ in output.

E.g. @ link(TSomeClass).

E.g. @link(TSomeClass).

E.g. @html foobar.

E.g. @link .

</td></tr>
</table>

## Authors
- Michalis <[my@email.address](mailto:my@email.address)>
- kambi


## Created


2005-03-30



## Last Modified


2005-03-30


