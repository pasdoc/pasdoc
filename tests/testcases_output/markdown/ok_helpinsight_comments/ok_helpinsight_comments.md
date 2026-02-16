# ok\_helpinsight\_comments


# Unit ok\_helpinsight\_comments

- [Description](#PasDoc-Description)
- Uses
- Classes, Interfaces, Objects and Records
- [Functions and Procedures](#PasDoc-FuncsProcs)
- Types
- Constants
- Variables

<span id="PasDoc-Description"/>

## Description
Test of handling help insight comments, in the form &quot;/// <tag> ... </tag>&quot;. See [http://delphi.wikia.com/wiki/Help\_insight](http://delphi.wikia.com/wiki/Help_insight), example snippet with [Parse](ok_helpinsight_comments.md#Parse) function is straight from there. See [https://sourceforge.net/tracker/?func=detail&amp;atid=304213&amp;aid=3485263&amp;group\_id=4213](https://sourceforge.net/tracker/?func=detail&amp;atid=304213&amp;aid=3485263&amp;group_id=4213).<span id="PasDoc-Uses"/>

## Overview

### Functions and Procedures
<span id="PasDoc-FuncsProcs"/>


<table>
<tr>

<td>

<code>procedure <strong><a href="ok_helpinsight_comments.md#Parse">Parse</a></strong>(const \_CmdLine: string);</code>
</td>
</tr>
</table>

## Description

### Functions and Procedures

<table>
<tr>

<td>

<span id="Parse"/><code>procedure <strong>Parse</strong>(const \_CmdLine: string);</code>
</td>
</tr>
<tr><td colspan="1">

parses the commandline</p>
<p>
 
###### Parameters
<dl>
<dt>CmdLine</dt>
<dd>

is a string giving the commandline. NOTE: Do not pass System.CmdLine since it contains the program's name as the first &quot;parameter&quot;. If you want to parse the commandline as passed by windows, call the overloaded Parse method without parameters. It handles this.</dd>
</dl>


</td></tr>
</table>
