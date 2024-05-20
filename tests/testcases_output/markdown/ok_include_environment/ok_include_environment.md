# ok\_include\_environment


# Unit ok\_include\_environment

- [Description](#PasDoc-Description)
- Uses
- Classes, Interfaces, Objects and Records
- Functions and Procedures
- Types
- [Constants](#PasDoc-Constants)
- Variables

<span id="PasDoc-Description"/>

## Description
Test of handling the &quot;*$I or $INCLUDE : Include compiler info*&quot; feature of FPC, see \[[http://www.freepascal.org/docs-html/prog/progsu38.html](http://www.freepascal.org/docs-html/prog/progsu38.html)\].</p>
<p>


PasDoc bug spotted by Michalis on 2005-12-04 when trying \`make htmldocs' on fpc compiler sources, in file version.pas.

Notes about how it should be implemented in PasDoc :

PasDoc will *not* expand these macros. Instead PasDoc will just explicitly show that e.g. value of MacDATE is %DATE%, value of MacFPCTARGET is %FPCTARGET% etc. Reasons: 

- For %DATE% and %TIME%, PasDoc could expand them, but it's not sensible. After all, at compilation they will be set to something different. So what PasDoc should do (and will) is to show user that the value of MacDATE is %DATE%.

This way user will know that MacDATE's value depends on time of compilation.

- For %FPC???% macros, PasDoc couldn't expand them, even if it should. After all, we don't know what FPC version will be used to compile the given unit.

- For %environment-variable%: argument like with %FPC???% macros: PasDoc is not able to predict what value $environment-variable will have at compilation time.

- Finally, for %FILE% and %LINE%: this is the only case when actually PasDoc could just expand them, just like FPC will.

For now, my decision is to not expand them, for consistency with handling all other %xxx%.



<span id="PasDoc-Uses"/>

## Overview

### Constants
<span id="PasDoc-Constants"/>


<table>
<tr>

<td>

<code><strong><a href="ok_include_environment.md#MacDATE">MacDATE</a></strong> = {$I %DATE%};</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_include_environment.md#MacFPCTARGET">MacFPCTARGET</a></strong> = {$I %FPCTARGET%};</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_include_environment.md#MacFPCTARGETCPU">MacFPCTARGETCPU</a></strong> = {$I %FPCTARGETCPU%};</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_include_environment.md#MacFPCTARGETOS">MacFPCTARGETOS</a></strong> = {$I %FPCTARGETOS%};</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_include_environment.md#MacFPCVERSION">MacFPCVERSION</a></strong> = {$I %FPCVERSION%};</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_include_environment.md#MacFILE">MacFILE</a></strong> = {$I %FILE%};</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_include_environment.md#MacLINE">MacLINE</a></strong> = {$I %LINE%};</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_include_environment.md#MacTIME">MacTIME</a></strong> = {$I %TIME%};</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_include_environment.md#MacUSEREnv">MacUSEREnv</a></strong> = {$I %USER%};</code>
</td>
</tr>
<tr>

<td>

<code><strong><a href="ok_include_environment.md#MacPathEnv">MacPathEnv</a></strong> = {$I %PATH%};</code>
</td>
</tr>
</table>

## Description

### Constants

<table>
<tr>

<td>

<span id="MacDATE"/><code><strong>MacDATE</strong> = {$I %DATE%};</code>
</td>
</tr>
<tr><td colspan="1">

Inserts the current date.

</td></tr>
</table>

<table>
<tr>

<td>

<span id="MacFPCTARGET"/><code><strong>MacFPCTARGET</strong> = {$I %FPCTARGET%};</code>
</td>
</tr>
<tr><td colspan="1">

Inserts the target CPU name. (deprecated, use FPCTARGETCPU)

</td></tr>
</table>

<table>
<tr>

<td>

<span id="MacFPCTARGETCPU"/><code><strong>MacFPCTARGETCPU</strong> = {$I %FPCTARGETCPU%};</code>
</td>
</tr>
<tr><td colspan="1">

Inserts the target CPU name.

</td></tr>
</table>

<table>
<tr>

<td>

<span id="MacFPCTARGETOS"/><code><strong>MacFPCTARGETOS</strong> = {$I %FPCTARGETOS%};</code>
</td>
</tr>
<tr><td colspan="1">

Inserts the target OS name.

</td></tr>
</table>

<table>
<tr>

<td>

<span id="MacFPCVERSION"/><code><strong>MacFPCVERSION</strong> = {$I %FPCVERSION%};</code>
</td>
</tr>
<tr><td colspan="1">

Current compiler version number.

</td></tr>
</table>

<table>
<tr>

<td>

<span id="MacFILE"/><code><strong>MacFILE</strong> = {$I %FILE%};</code>
</td>
</tr>
<tr><td colspan="1">

Filename in which the directive is found.

</td></tr>
</table>

<table>
<tr>

<td>

<span id="MacLINE"/><code><strong>MacLINE</strong> = {$I %LINE%};</code>
</td>
</tr>
<tr><td colspan="1">

Linenumer on which the directive is found.

</td></tr>
</table>

<table>
<tr>

<td>

<span id="MacTIME"/><code><strong>MacTIME</strong> = {$I %TIME%};</code>
</td>
</tr>
<tr><td colspan="1">

Current time.

</td></tr>
</table>

<table>
<tr>

<td>

<span id="MacUSEREnv"/><code><strong>MacUSEREnv</strong> = {$I %USER%};</code>
</td>
</tr>
<tr><td colspan="1">

If xxx inside %xxx% is none of the above, then it is assumed to be the name of an environment variable. Its value will be fetched.

</td></tr>
</table>

<table>
<tr>

<td>

<span id="MacPathEnv"/><code><strong>MacPathEnv</strong> = {$I %PATH%};</code>
</td>
</tr>
<tr><td colspan="1">

This item has no description.



</td></tr>
</table>
