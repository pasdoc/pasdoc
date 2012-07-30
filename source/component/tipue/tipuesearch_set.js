
/*
Tipue Search 1.1.1
Tipue Search Copyright (c) 2012 Tri-State Consultants
Tipue Search is free for both both commercial and non-commercial use and released under the MIT License.
For the latest release, documentation and licence see http://www.tipue.com/search

These settings are documented in Tipue Search Settings at http://www.tipue.com/help/search/set
*/

/* PasDoc: 10 seems standard */
var tipuesearch_show = 10;
/* PasDoc: showing urls is not much useful for pasdoc-made docs,
   since our title will always show the unit and identifier already. */
var tipuesearch_show_url = 0;
var tipuesearch_minimum_length = 3;
var tipuesearch_new_window = 0;
var tipuesearch_descriptive_words = 25;

var tipuesearch_stop_words = ["and","be","by","do","for","he","how","if","is","it","my","not","of","or","the","to","up","what","when"];

var tipuesearch_replace = {"words": [
     {"word": "tipua", replace_with: "tipue"},
     {"word": "javscript", replace_with: "javascript"}
]};

var tipuesearch_stem = {"words": [
     {"word": "setup", stem: "install"},
     {"word": "email", stem: "contact"},
     {"word": "javascript", stem: "js"}
]};
