{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "pasdoc.css" }
'/*' + LineEnding +
'  Copyright 1998-2018 PasDoc developers.' + LineEnding +
'' + LineEnding +
'  This file is part of "PasDoc".' + LineEnding +
'' + LineEnding +
'  "PasDoc" is free software; you can redistribute it and/or modify' + LineEnding +
'  it under the terms of the GNU General Public License as published by' + LineEnding +
'  the Free Software Foundation; either version 2 of the License, or' + LineEnding +
'  (at your option) any later version.' + LineEnding +
'' + LineEnding +
'  "PasDoc" is distributed in the hope that it will be useful,' + LineEnding +
'  but WITHOUT ANY WARRANTY; without even the implied warranty of' + LineEnding +
'  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the' + LineEnding +
'  GNU General Public License for more details.' + LineEnding +
'' + LineEnding +
'  You should have received a copy of the GNU General Public License' + LineEnding +
'  along with "PasDoc"; if not, write to the Free Software' + LineEnding +
'  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA' + LineEnding +
'' + LineEnding +
'  ----------------------------------------------------------------------------' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'body, html {' + LineEnding +
'  margin: 0;' + LineEnding +
'  padding: 0;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'body {' + LineEnding +
'  font-family: Verdana,Arial;' + LineEnding +
'  color: black;' + LineEnding +
'  background-color: white;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'.container {' + LineEnding +
'  width: 100%;' + LineEnding +
'  height: 100%;' + LineEnding +
'  border-spacing: 0;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'.navigation {' + LineEnding +
'  float: left;' + LineEnding +
'  width: 20em; /* must match .content margin-left */' + LineEnding +
'  height: 100%;' + LineEnding +
'  color: white;' + LineEnding +
'  background-color: #787878;' + LineEnding +
'  position: fixed;' + LineEnding +
'  margin: 0;' + LineEnding +
'  box-sizing: border-box; /* without this, you could not have padding here, it would overlap with .content, causing errors on narrow screens */' + LineEnding +
'  padding: 1em;' + LineEnding +
'}' + LineEnding +
'.navigation ul {' + LineEnding +
'  margin: 0em;' + LineEnding +
'  padding: 0em;' + LineEnding +
'}' + LineEnding +
'.navigation li {' + LineEnding +
'  list-style-type: none;' + LineEnding +
'  margin: 0.2em 0em 0em 0em;' + LineEnding +
'  padding: 0.25em;' + LineEnding +
'}' + LineEnding +
'.navigation h2 {' + LineEnding +
'  text-align: center;' + LineEnding +
'  margin: 0em;' + LineEnding +
'  padding: 0.5em;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'.content {' + LineEnding +
'  margin-left: 20em; /* must match .navigation width */' + LineEnding +
'  box-sizing: border-box; /* without this, you could not have padding here, it would overlap with .navigation, causing errors on narrow screens */' + LineEnding +
'  padding: 1em;' + LineEnding +
'}' + LineEnding +
'.content h1 {' + LineEnding +
'  margin-top: 0;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'.appinfo {' + LineEnding +
'  float: right;' + LineEnding +
'  text-align: right;' + LineEnding +
'  margin-bottom: 1em;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'img { border:0px; }' + LineEnding +
'' + LineEnding +
'hr {' + LineEnding +
'  border-bottom: medium none;' + LineEnding +
'  border-top: thin solid #888;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'a:link {color:#C91E0C; text-decoration: none; }' + LineEnding +
'a:visited {color:#7E5C31; text-decoration: none; }' + LineEnding +
'a:hover {text-decoration: underline; }' + LineEnding +
'a:active {text-decoration: underline; }' + LineEnding +
'' + LineEnding +
'.navigation a:link { color: white; text-decoration: none; }' + LineEnding +
'.navigation a:visited { color: white; text-decoration: none; }' + LineEnding +
'.navigation a:hover { color: white; font-weight: bold; text-decoration: none; }' + LineEnding +
'.navigation a:active { color: white; text-decoration: none; }' + LineEnding +
'' + LineEnding +
'a.bold:link {color:#C91E0C; text-decoration: none; font-weight:bold; }' + LineEnding +
'a.bold:visited {color:#7E5C31; text-decoration: none; font-weight:bold; }' + LineEnding +
'a.bold:hover {text-decoration: underline; font-weight:bold; }' + LineEnding +
'a.bold:active {text-decoration: underline; font-weight:bold; }' + LineEnding +
'' + LineEnding +
'a.section {color: green; text-decoration: none; font-weight: bold; }' + LineEnding +
'a.section:hover {color: green; text-decoration: underline; font-weight: bold; }' + LineEnding +
'' + LineEnding +
'ul.useslist a:link {color:#C91E0C; text-decoration: none; font-weight:bold; }' + LineEnding +
'ul.useslist a:visited {color:#7E5C31; text-decoration: none; font-weight:bold; }' + LineEnding +
'ul.useslist a:hover {text-decoration: underline; font-weight:bold; }' + LineEnding +
'ul.useslist a:active {text-decoration: underline; font-weight:bold; }' + LineEnding +
'' + LineEnding +
'ul.hierarchy { list-style-type:none; }' + LineEnding +
'ul.hierarchylevel { list-style-type:none; }' + LineEnding +
'' + LineEnding +
'p.unitlink a:link {color:#C91E0C; text-decoration: none; font-weight:bold; }' + LineEnding +
'p.unitlink a:visited {color:#7E5C31; text-decoration: none; font-weight:bold; }' + LineEnding +
'p.unitlink a:hover {text-decoration: underline; font-weight:bold; }' + LineEnding +
'p.unitlink a:active {text-decoration: underline; font-weight:bold; }' + LineEnding +
'' + LineEnding +
'tr.list { background: #FFBF44; }' + LineEnding +
'tr.list2 { background: #FFC982; }' + LineEnding +
'tr.listheader { background: #C91E0C; color: white; }' + LineEnding +
'' + LineEnding +
'table.wide_list { border-spacing:2px; width:100%; }' + LineEnding +
'table.wide_list td { vertical-align:top; padding:4px; }' + LineEnding +
'' + LineEnding +
'table.markerlegend { width:auto; }' + LineEnding +
'table.markerlegend td.legendmarker { text-align:center; }' + LineEnding +
'' + LineEnding +
'.sections { background:white; }' + LineEnding +
'.sections .one_section {' + LineEnding +
'  background:lightgray;' + LineEnding +
'  display: inline-block;' + LineEnding +
'  margin: 0.2em;' + LineEnding +
'  padding: 0.5em 1em;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'table.summary td.itemcode { width:100%; }' + LineEnding +
'table.detail td.itemcode { width:100%; }' + LineEnding +
'' + LineEnding +
'td.itemname {white-space:nowrap; }' + LineEnding +
'td.itemunit {white-space:nowrap; }' + LineEnding +
'td.itemdesc { width:100%; }' + LineEnding +
'' + LineEnding +
'/* The paragraph "This item has no description." with this CSS class appears' + LineEnding +
'   on items that have no description, and no ancestor from which' + LineEnding +
'   to take a description.' + LineEnding +
'   By default we hide it, you can inhide it to explicitly see where descriptions are missing. */' + LineEnding +
'p.nodescription {' + LineEnding +
'  color:red;' + LineEnding +
'  display: none;' + LineEnding +
'}' + LineEnding +
'p.inheritdescription { color:red; }' + LineEnding +
'dl.description dt { color:red;}' + LineEnding +
'dl.parameters dt {' + LineEnding +
'  color:blue;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'code {' + LineEnding +
'  font-family: monospace;' + LineEnding +
'  font-size:1.2em;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/* style for warning and note tag */' + LineEnding +
'dl.tag.warning {' + LineEnding +
'  margin-left:-2px;' + LineEnding +
'  padding-left: 3px;' + LineEnding +
'  border-left:4px solid;' + LineEnding +
'  border-color: #FF0000;' + LineEnding +
'}' + LineEnding +
'dl.tag.note {' + LineEnding +
'  margin-left:-2px;' + LineEnding +
'  padding-left: 3px;' + LineEnding +
'  border-left:4px solid;' + LineEnding +
'  border-color: #D0C000;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/* Various browsers have various default styles for <h6>,' + LineEnding +
'   sometimes ugly for our purposes, so it''s best to set things' + LineEnding +
'   like font-size and font-weight in out pasdoc.css explicitly. */' + LineEnding +
'h6.description_section {' + LineEnding +
'  /* font-size 100% means that it has the same font size as the' + LineEnding +
'     parent element, i.e. normal description text */' + LineEnding +
'  font-size: 100%;' + LineEnding +
'  font-weight: bold;' + LineEnding +
'  /* By default browsers usually have some large margin-bottom and' + LineEnding +
'     margin-top for <h1-6> tags. In our case, margin-bottom is' + LineEnding +
'     unnecessary, we want to visually show that description_section' + LineEnding +
'     is closely related to content below. In this situation' + LineEnding +
'     (where the font size is just as a normal text), smaller bottom' + LineEnding +
'     margin seems to look good. */' + LineEnding +
'  margin-top: 1.4em;' + LineEnding +
'  margin-bottom: 0em;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/* Style applied to Pascal code in documentation' + LineEnding +
'   (e.g. produced by @longcode tag) } */' + LineEnding +
'.longcode {' + LineEnding +
'  font-family: monospace;' + LineEnding +
'  font-size: 1.2em;' + LineEnding +
'  background-color: #eee;' + LineEnding +
'  padding: 0.5em;' + LineEnding +
'  border: thin solid #ccc;' + LineEnding +
'}' + LineEnding +
'span.pascal_string { color: #000080; }' + LineEnding +
'span.pascal_keyword { font-weight: bolder; }' + LineEnding +
'span.pascal_comment { color: #000080; font-style: italic; }' + LineEnding +
'span.pascal_compiler_comment { color: #008000; }' + LineEnding +
'span.pascal_numeric { }' + LineEnding +
'span.pascal_hex { }' + LineEnding +
'' + LineEnding +
'p.hint_directive { color: red; }' + LineEnding +
'' + LineEnding +
'input#search_text { }' + LineEnding +
'input#search_submit_button { }' + LineEnding +
'' + LineEnding +
'acronym.mispelling { background-color: #f00; }' + LineEnding +
'' + LineEnding +
'/* Actually this reduces vertical space between *every* paragraph' + LineEnding +
'   inside list with @itemSpacing(compact).' + LineEnding +
'   While we would like to reduce this space only for the' + LineEnding +
'   top of 1st and bottom of last paragraph within each list item.' + LineEnding +
'   But, well, user probably will not do any paragraph breaks' + LineEnding +
'   within a list with @itemSpacing(compact) anyway, so it''s' + LineEnding +
'   acceptable solution. */' + LineEnding +
'ul.compact_spacing p { margin-top: 0em; margin-bottom: 0em; }' + LineEnding +
'ol.compact_spacing p { margin-top: 0em; margin-bottom: 0em; }' + LineEnding +
'dl.compact_spacing p { margin-top: 0em; margin-bottom: 0em; }' + LineEnding +
'' + LineEnding +
'/* Style for table created by @table tags:' + LineEnding +
'   just some thin border.' + LineEnding +
'' + LineEnding +
'   This way we have some borders around the cells' + LineEnding +
'   (so cells are visibly separated), but the border' + LineEnding +
'   "blends with the background" so it doesn''t look too ugly.' + LineEnding +
'   Hopefully it looks satisfactory in most cases and for most' + LineEnding +
'   people.' + LineEnding +
'' + LineEnding +
'   We add padding for cells, otherwise they look too close.' + LineEnding +
'   This is normal thing to do when border-collapse is set to' + LineEnding +
'   collapse (because this eliminates spacing between cells).' + LineEnding +
'*/' + LineEnding +
'table.table_tag { border-collapse: collapse; }' + LineEnding +
'table.table_tag td { border: 1pt solid gray; padding: 0.3em; }' + LineEnding +
'table.table_tag th { border: 1pt solid gray; padding: 0.3em; }' + LineEnding +
'' + LineEnding +
'table.detail {' + LineEnding +
'  border: 1pt solid gray;' + LineEnding +
'  margin-top: 0.3em;' + LineEnding +
'  margin-bottom: 0.3em;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'.search-form { white-space: nowrap; }' + LineEnding +
'.search-input input { max-width: 80%; } /* this provides some safe space to always fit even on very narrow screens */' + LineEnding +
'.search-input input, .search-button { display: inline-block; vertical-align: middle; }' + LineEnding +
'.search-input { display: inline-block; }' + LineEnding +
'' + LineEnding +
'/* Do not make extra vertical space at the beginning/end of table cells.' + LineEnding +
'   We need ">" selector, to not change paragraphs inside lists inside' + LineEnding +
'   table cells. */' + LineEnding +
'table.table_tag td > p:first-child,' + LineEnding +
'table.table_tag th > p:first-child,' + LineEnding +
'       td.itemdesc > p:first-child { margin-top: 0em; }' + LineEnding +
'' + LineEnding +
'table.table_tag td > p:last-child,' + LineEnding +
'table.table_tag th > p:last-child,' + LineEnding +
'       td.itemdesc > p:last-child { margin-bottom: 0em; }' + LineEnding +
''
