# ok\_lists


# Unit ok\_lists

- [Description](#PasDoc-Description)
- Uses
- Classes, Interfaces, Objects and Records
- Functions and Procedures
- Types
- Constants
- Variables

<span id="PasDoc-Description"/>

## Description
Test of lists tags.</p>
<p>


Normal lists: 

1. One

1. Two

1. Three



 

- One

- Two

- Three





Empty list is accceptable: 



 





Whitespace inside lists is acceptable (and empty line that is not inside any @item is *not* a paragraph): 

1. One

1. Two

1.  Other tags inside the items are allowed, e.g. [link to self](ok_lists.md), `True`, 

```pascal
</p>

<pre class="longcode">**begin** X := Y; **end**;</pre>

<p>
```

, **something bold**, URLs: [http://pasdoc.sf.net/](http://pasdoc.sf.net/), paragraphs:

2nd paragraph, dashes: em dash ---, en dash --, short one -, two consecutive short dashes --. 





Nested lists are also freely allowed: 

1.  1st nested unordered list: 

- One

- Two

- Three



 

1.  2nd nested unordered list: 

- One

- Two

- Three



 

1.  And a couple of single-item ordered lists nested: 

1.  

1.  









(Source code of this example begins to look like LISP :) 





Definition lists tests:



-   ****:1st item

-   ****:2nd item







-   **1st item label**:1st item

-   **2nd item label**:2nd item







-   **1st item label**:

-   **2nd item label**:





Item spacing tests:



-   **1st item label**:1st item

-   **2nd item label**:2nd item







1. One

1. Two

1. Three



<span id="PasDoc-Uses"/>
