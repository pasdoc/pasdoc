# ok\_markdown


# Unit ok\_markdown

- [Description](#PasDoc-Description)
- Uses
- Classes, Interfaces, Objects and Records
- Functions and Procedures
- Types
- [Constants](#PasDoc-Constants)
- Variables

<span id="PasDoc-Description"/>

## Description
**This is a test of Markdown syntax**</p>
<p>
 Correct lists



- list item #1.1

- list item #1.2



end list



- list item #2.1

- list item #2.2



end list



- list item #3



end list



- list item #4





1. list item #5.1

1. list item #5.2

1. list item #5.10



Simple list with multiline text



- item 6.1 text text text

- item 6.2

item text



end list

List with nested list



- item 7.1 text 

- item 7.1.1 text



 text2



List with more nesting



- item 8.1 

- item 8.1.1 

- item 8.1.1.1



 text 8.1.1

- item 8.1.2 

- item 8.1.2.1







 text 8.1



end list

You can even mix markdown and PasDoc tags



- list item #8.1

- list item #8.2





Incorrect lists

*not a list*

1not a list

1 not a list

1.not a list

Emphasis, aka italics, with *asterisks* or *underscores*.

Strong emphasis, aka bold, with **asterisks** or **underscores**.

Some **bold text**.

Some ***bold and italic text***.

Some **some bold and *italic* and *italic* and *italic once again* text**.

Some *italic text*.

Some **bold text with some tags: My name is `ok\_markdown`, some `begin end` and a link to me: [ok\_markdown](ok_markdown.md)**.

\\\*markers could be escaped

*and also escaped at end\\\* of a word*

or placed inside\_word or placed\_inside\_word\_multiple\_times or at the end\_

Multiplications are OK: A\*B\*C\*D and with spaces too: A \* B \* C \* D

underscore \_ is used to name some deprecated thing: something\_

Some `inline code`, some `**formatting** *inside* code`

Some preformatted code:



```</p>

<pre class="preformatted">
program Foo;
  Some long code
  with
  syntax highlight</pre>

<p>
```



Some Pascal code:



```pascal
</p>

<pre class="longcode">
**program** Foo;
  Some long code
  **with**
  syntax highlight</pre>

<p>
```



Correct URLs:

[Some one-line descr](http://example)

[Some multi-line
   descr](http://example)

[Escaped \[\] descr](http://example)

[\\](http://example)

[a\\](http://example)

[Bracket URL](http://example/(foo))

Incorrect URLs:

([http://example](http://example))

\[Some descr\] ([http://example](http://example))

\[Some descr\]([http://example](http://example)

\[Bug fix: URL preceeded by block in square brackets\] [descr](http://example)<span id="PasDoc-Uses"/>

## Overview

### Constants
<span id="PasDoc-Constants"/>


<table>
<tr>

<td>

<code><strong><a href="ok_markdown.md#foo">foo</a></strong> = 1;</code>
</td>
</tr>
</table>

## Description

### Constants

<table>
<tr>

<td>

<span id="foo"/><code><strong>foo</strong> = 1;</code>
</td>
</tr>
<tr><td colspan="1">

[Bug fix: URL at the end of comment](http://example)

</td></tr>
</table>
