# ok\_at\_character\_in\_verbatim


# Unit ok\_at\_character\_in\_verbatim

- [Description](#PasDoc-Description)
- Uses
- Classes, Interfaces, Objects and Records
- Functions and Procedures
- Types
- Constants
- Variables

<span id="PasDoc-Description"/>

## Description
Test that @( and @) and @@ (see [https://github.com/pasdoc/pasdoc/wiki/SupportedTags](https://github.com/pasdoc/pasdoc/wiki/SupportedTags) and [https://github.com/pasdoc/pasdoc/wiki/TagsParametersMatching](https://github.com/pasdoc/pasdoc/wiki/TagsParametersMatching)) work Ok inside all @-tags. Including tags that don't interpret other @-tags inside, like @latex or @html.

*This is a closing paren ) and open ( and &quot;at&quot; char @ using italic font.*



<b>This is a closing paren ) and open ( and "at" char @ inside HTML.</b>



```pascal
</p>

<pre class="longcode">
* **// This is a closing paren ) and open ( and &quot;at&quot; char @ inside longcode.** *</pre>

<p>
```

<span id="PasDoc-Uses"/>
