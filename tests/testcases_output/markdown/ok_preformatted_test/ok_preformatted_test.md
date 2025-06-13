# ok\_preformatted\_test


# Unit ok\_preformatted\_test

- [Description](#PasDoc-Description)
- Uses
- Classes, Interfaces, Objects and Records
- Functions and Procedures
- Types
- Constants
- Variables

<span id="PasDoc-Description"/>

## Description
Test of @preformatted tag.</p>
<p>




```</p>

<pre class="preformatted">
Line one ?
1111 222 ?
IIII WWW ?

Some    long    space
    Some    long     space

Three empty lines below (\*not\* converted into paragraph):



-------- end of three empty lines.

Some html and LaTeX special chars, to make sure that ConvertString is
called when it should be: < > &amp; \\ { }

Note that @-tags inside are not expanded: @name, and consequently
you don't have to double @ char. Just like within @html and @latex
tags.</pre>

<p>
```

<span id="PasDoc-Uses"/>
