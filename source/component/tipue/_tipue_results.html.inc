{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "_tipue_results.html" }
'<!DOCTYPE html>' + LineEnding +
'<html lang="###-PASDOC-LANGUAGE-###">' + LineEnding +
'<head>' + LineEnding +
'<title>Search Results</title>' + LineEnding +
'' + LineEnding +
'###-PASDOC-HEAD-###' + LineEnding +
'' + LineEnding +
'</head>' + LineEnding +
'<body>' + LineEnding +
'' + LineEnding +
'###-PASDOC-BODY-BEGIN-###' + LineEnding +
'' + LineEnding +
'<h1>Search results</h1>' + LineEnding +
'' + LineEnding +
'<div id="tipue_search_content"></div>' + LineEnding +
'' + LineEnding +
'###-PASDOC-BODY-END-###' + LineEnding +
'' + LineEnding +
'<!--' + LineEnding +
'  Note that we use a local jquery copy (not from Google Hosted Libraries),' + LineEnding +
'  to work offline.' + LineEnding +
'-->' + LineEnding +
'<script type="text/javascript" src="tipuesearch/jquery.min.js"></script>' + LineEnding +
'<script type="text/javascript" src="tipuesearch/tipuesearch_content.js"></script>' + LineEnding +
'<script type="text/javascript" src="tipuesearch/tipuesearch_set.js"></script>' + LineEnding +
'<script type="text/javascript" src="tipuesearch/tipuesearch.js"></script>' + LineEnding +
'' + LineEnding +
'<script type="text/javascript">' + LineEnding +
'jQuery(document).ready(function() {' + LineEnding +
'    jQuery(''#tipue_search_input'').tipuesearch({' + LineEnding +
'' + LineEnding +
'        /* 10 items to display seems standard */' + LineEnding +
'        ''show'': 10,' + LineEnding +
'' + LineEnding +
'        /* For PasDoc docs, showing urls is not very useful,' + LineEnding +
'           since the page title already shows the unit and identifier. */' + LineEnding +
'        ''showURL'': false,' + LineEnding +
'' + LineEnding +
'        /* Unfortunately we have to turn off this feature.' + LineEnding +
'           It looks nice when it works correctly, but...' + LineEnding +
'' + LineEnding +
'           But it also makes the results visibly broken sometimes.' + LineEnding +
'           When the search term is found also inside HTML attributes' + LineEnding +
'           inside the description, then highlighting means that Tipue' + LineEnding +
'           adds a <span class="tipue_search_content_bold"> which may break' + LineEnding +
'           the HTML.' + LineEnding +
'           Consider searching for CastleWindow inside a Castle Game Engine docs.' + LineEnding +
'           A result may have a description like this:' + LineEnding +
'' + LineEnding +
'           Some text <a class="normal" href="CastleWindow.TCastleWindowCustom.html#Width">Width</a> ...' + LineEnding +
'' + LineEnding +
'           When highlightTerms is on, every occurrence of CastleWindow' + LineEnding +
'           above will be replaced with <span class="tipue_search_content_bold">CastleWindow</span>' + LineEnding +
'           which will completely break the above HTML.' + LineEnding +
'' + LineEnding +
'           This bug can easily happen, in general, when searching for a name' + LineEnding +
'           of class or unit in a PasDoc documentation. So it seems quite common' + LineEnding +
'           in case of PasDoc-generated documentation.' + LineEnding +
'        */' + LineEnding +
'        ''highlightTerms'': false' + LineEnding +
'    });' + LineEnding +
'});' + LineEnding +
'</script>' + LineEnding +
'' + LineEnding +
'</body>' + LineEnding +
'</html>' + LineEnding +
''
