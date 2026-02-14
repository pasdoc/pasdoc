# First additional file


# Long descriptive name of the first additional file:
 

 

<h2 class=""><span id="SecFirst"></span>First section</h2>


This file is supposed to contain any additional information that you wish to include in your documentation. You can note that all rules that apply to normal pasdoc descriptions apply also here, e.g. empty line means new paragraph:

New paragraph.

3rd paragraph. URLs are automatically recognized, like this: [http://pasdoc.sourceforge.net/](http://pasdoc.sourceforge.net/). You have to write the @ twice (like @@) to get one @ in the output. Also normal @-tags work: `This is some code.`

<h2 class=""><span id="SecSecond"></span>Second section</h2>


Here you can see some hot snippet from implementation of this feature, just to test @longcode tag:



```pascal
</p>

<pre class="longcode">
**procedure** TPasDoc.HandleExtraFile(**const** FileName: **string**;
  **out** ExtraDescription: TExtraDescription);
**begin**
  ExtraDescription := TExtraDescription.Create;
  **try**
    DoMessage(2, mtInformation, *'Now parsing file %s...'*, \[FileName\]);

    ExtraDescription.**Name** := SCharsReplace(
      ChangeFileExt( ExtractFileName(FileName) , *''*), \[*' '*\], *'\_'*);

    ExtraDescription.RawDescription := FileToString(FileName);
  **except**
    FreeAndNil(ExtraDescription);
    **raise**;
  **end**;
**end**;</pre>

<p>
```



<h2 class=""><span id="ThirdSecond"></span>Third section</h2>


Normal links work : [MyConstant](ok_introduction_conclusion.md#MyConstant).

Blah.

Blah.

Blah.

Blah.

Blah.

Blah.

<span id="SomeAnchor"/> Here is a paragraph with an anchor. It looks like a normal paragraph, but you can link to it with @link(SomeAnchor).

Blah.

Blah.

Blah.

Blah.

Blah.

Blah.

Blah.

Blah.

Blah.

Sections with the same user-visible names are OK (example when this is useful is below):

<h2 class=""><span id="SecStrings"></span>Routines dealing with strings</h2>


<h3 class=""><span id="SecStringsOverview"></span>Overview</h3>


<h3 class=""><span id="SecStringsExamples"></span>Examples</h3>


<h2 class=""><span id="SecIntegers"></span>Routines dealing with integers</h2>


<h3 class=""><span id="SecIntegersOverview"></span>Overview</h3>


<h3 class=""><span id="SecIntegersExamples"></span>Examples</h3>

## Author
<ul class="authors">
<li>Kambi</li>
</ul>

## Created
<p>
On some rainy day
</p>
