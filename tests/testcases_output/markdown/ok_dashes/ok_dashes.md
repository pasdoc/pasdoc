# ok\_dashes


# Unit ok\_dashes

- [Description](#PasDoc-Description)
- Uses
- Classes, Interfaces, Objects and Records
- Functions and Procedures
- Types
- Constants
- Variables

<span id="PasDoc-Description"/>

## Description
Test of various dashes.</p>
<p>


Triple dash produces em-dash, for separating parts of sentence and such, like &quot;I know a secret --- but I won't tell&quot;.

Double dash produces en-dash, intended to use for numbers ranges, like &quot;10--20&quot;.

Normal single dash is a short dash, for compound words, like &quot;variable-width font&quot;.

You can write @- in cases where you really want to write just 2 or more consecutive short dashes. E.g. --long-option-name (here I escaped only the 1st &quot;-&quot;, this means that the rest of dashes is also treated as a short dash), or --long-option-name (here I escaped only the 2nd dash), or --long-option-name (here I escaped two first dashes, which wasn't really necessary, it's sufficient to escape either 1st or the 2nd dash), --long-option-name (here I escaped all dashes; this looks unnecessary ugly in source code, but it's correct).<span id="PasDoc-Uses"/>
