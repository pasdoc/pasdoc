# ok\_longcode\_underscores


# Unit ok\_longcode\_underscores

- [Description](#PasDoc-Description)
- Uses
- Classes, Interfaces, Objects and Records
- Functions and Procedures
- Types
- Constants
- Variables

<span id="PasDoc-Description"/>

## Description
2005-06-07: Latex version of docs for this unit are wrong, because ConvertString is not called to convert \_ char. So \_ is not escaped and latex fails. Also, HTML version is bad, because &quot;With&quot; is formatted in bold, because it's treated like keyword.

Fixed by adding \_ to AlphaNumeric in PasDoc\_Gen in FormatPascalCode.



```pascal
</p>

<pre class="longcode">
Identifier\_With\_Underscores;</pre>

<p>
```

<span id="PasDoc-Uses"/>
