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
'/* === Custom Properties === */' + LineEnding +
':root {' + LineEnding +
'  --pasdoc-accent: #0d6efd;' + LineEnding +
'  --pasdoc-accent-hover: #0a58ca;' + LineEnding +
'  --pasdoc-accent-light: #e7f1ff;' + LineEnding +
'  --pasdoc-sidebar-width: 18rem;' + LineEnding +
'  --pasdoc-sidebar-bg: #f8f9fa;' + LineEnding +
'  --pasdoc-border-color: #dee2e6;' + LineEnding +
'  --pasdoc-code-bg: #f8f9fa;' + LineEnding +
'  --pasdoc-muted: #6c757d;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/* === Layout: sidebar + content === */' + LineEnding +
'' + LineEnding +
'@media (min-width: 768px) {' + LineEnding +
'  .offcanvas-md {' + LineEnding +
'    width: var(--pasdoc-sidebar-width) !important;' + LineEnding +
'    position: fixed !important;' + LineEnding +
'    top: 0;' + LineEnding +
'    bottom: 0;' + LineEnding +
'    left: 0;' + LineEnding +
'    border-right: 1px solid var(--pasdoc-border-color);' + LineEnding +
'    overflow-y: auto;' + LineEnding +
'    background-color: var(--pasdoc-sidebar-bg) !important;' + LineEnding +
'  }' + LineEnding +
'  .pasdoc-content {' + LineEnding +
'    margin-left: var(--pasdoc-sidebar-width);' + LineEnding +
'  }' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/* === Sidebar navigation styling (with Bootstrap) === */' + LineEnding +
'' + LineEnding +
'.navigation h2 {' + LineEnding +
'  font-size: 1.1rem;' + LineEnding +
'  padding: 0.5rem 0.75rem;' + LineEnding +
'  margin: 0.5rem 0;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'.offcanvas-body .nav-link {' + LineEnding +
'  color: #495057;' + LineEnding +
'  font-size: 0.9rem;' + LineEnding +
'  border-radius: 0.25rem;' + LineEnding +
'}' + LineEnding +
'.offcanvas-body .nav-link:hover {' + LineEnding +
'  color: var(--pasdoc-accent);' + LineEnding +
'  background-color: var(--pasdoc-accent-light);' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/* Backward-compat: .navigation and .content are used by old/custom CSS.' + LineEnding +
'   Our pasdoc.css neutralizes them so Bootstrap layout takes precedence.' + LineEnding +
'   When a custom CSS is used (--css), it can restyle these freely. */' + LineEnding +
'.navigation ul {' + LineEnding +
'  margin: 0;' + LineEnding +
'  padding: 0;' + LineEnding +
'}' + LineEnding +
'.navigation li {' + LineEnding +
'  list-style-type: none;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/* === Section tabs === */' + LineEnding +
'' + LineEnding +
'.sections {' + LineEnding +
'  display: flex;' + LineEnding +
'  flex-wrap: wrap;' + LineEnding +
'  gap: 0.25rem;' + LineEnding +
'  margin-bottom: 1rem;' + LineEnding +
'  padding: 0;' + LineEnding +
'  background: transparent;' + LineEnding +
'}' + LineEnding +
'.sections .one_section {' + LineEnding +
'  display: inline-block;' + LineEnding +
'  background: transparent;' + LineEnding +
'  margin: 0;' + LineEnding +
'  padding: 0;' + LineEnding +
'}' + LineEnding +
'.sections .one_section a.section {' + LineEnding +
'  display: inline-block;' + LineEnding +
'  padding: 0.3rem 0.7rem;' + LineEnding +
'  font-size: 0.85rem;' + LineEnding +
'  font-weight: 500;' + LineEnding +
'  color: var(--pasdoc-accent);' + LineEnding +
'  background-color: var(--pasdoc-accent-light);' + LineEnding +
'  border-radius: 2rem;' + LineEnding +
'  text-decoration: none;' + LineEnding +
'}' + LineEnding +
'.sections .one_section a.section:hover {' + LineEnding +
'  background-color: var(--pasdoc-accent);' + LineEnding +
'  color: white;' + LineEnding +
'  text-decoration: none;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/* === Tables === */' + LineEnding +
'' + LineEnding +
'table.wide_list {' + LineEnding +
'  width: 100%;' + LineEnding +
'  border-collapse: collapse;' + LineEnding +
'  margin-bottom: 1rem;' + LineEnding +
'}' + LineEnding +
'table.wide_list td,' + LineEnding +
'table.wide_list th {' + LineEnding +
'  vertical-align: top;' + LineEnding +
'  padding: 0.5rem 0.75rem;' + LineEnding +
'  border-bottom: 1px solid var(--pasdoc-border-color);' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'tr.listheader {' + LineEnding +
'  background-color: var(--pasdoc-sidebar-bg);' + LineEnding +
'  border-bottom: 2px solid var(--pasdoc-border-color);' + LineEnding +
'}' + LineEnding +
'tr.listheader th {' + LineEnding +
'  font-weight: 600;' + LineEnding +
'  font-size: 0.85rem;' + LineEnding +
'  text-transform: uppercase;' + LineEnding +
'  letter-spacing: 0.025em;' + LineEnding +
'  color: var(--pasdoc-muted);' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'tr.list { background-color: #ffffff; }' + LineEnding +
'tr.list2 { background-color: var(--pasdoc-sidebar-bg); }' + LineEnding +
'tr.list:hover,' + LineEnding +
'tr.list2:hover { background-color: var(--pasdoc-accent-light); }' + LineEnding +
'' + LineEnding +
'table.detail {' + LineEnding +
'  border: 1px solid var(--pasdoc-border-color);' + LineEnding +
'  border-radius: 0.375rem;' + LineEnding +
'  overflow: hidden;' + LineEnding +
'  margin-top: 0.75rem;' + LineEnding +
'  margin-bottom: 0.75rem;' + LineEnding +
'  width: 100%;' + LineEnding +
'}' + LineEnding +
'table.detail td {' + LineEnding +
'  padding: 0.5rem 0.75rem;' + LineEnding +
'  vertical-align: top;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'table.summary td.itemcode,' + LineEnding +
'table.detail td.itemcode { width: 100%; }' + LineEnding +
'td.itemname { white-space: nowrap; }' + LineEnding +
'td.itemunit { white-space: nowrap; }' + LineEnding +
'td.itemdesc { width: 100%; }' + LineEnding +
'' + LineEnding +
'/* === Visibility badges === */' + LineEnding +
'' + LineEnding +
'.badge.vis-public    { background-color: #198754; font-size: 0.7rem; }' + LineEnding +
'.badge.vis-published { background-color: #0d6efd; font-size: 0.7rem; }' + LineEnding +
'.badge.vis-protected { background-color: #fd7e14; font-size: 0.7rem; }' + LineEnding +
'.badge.vis-private   { background-color: #dc3545; font-size: 0.7rem; }' + LineEnding +
'.badge.vis-automated { background-color: #6c757d; font-size: 0.7rem; }' + LineEnding +
'' + LineEnding +
'td.visibility {' + LineEnding +
'  width: 2.5rem;' + LineEnding +
'  text-align: center;' + LineEnding +
'  vertical-align: middle;' + LineEnding +
'}' + LineEnding +
'td.visibility a { text-decoration: none; }' + LineEnding +
'' + LineEnding +
'table.markerlegend { width: auto; }' + LineEnding +
'table.markerlegend td.legendmarker { text-align: center; padding-right: 1rem; }' + LineEnding +
'' + LineEnding +
'/* === Code styling === */' + LineEnding +
'' + LineEnding +
'code {' + LineEnding +
'  font-family: ''SFMono-Regular'', Consolas, ''Liberation Mono'', Menlo, monospace;' + LineEnding +
'  font-size: 0.875em;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'.longcode {' + LineEnding +
'  font-family: ''SFMono-Regular'', Consolas, ''Liberation Mono'', Menlo, monospace;' + LineEnding +
'  font-size: 0.875em;' + LineEnding +
'  background-color: var(--pasdoc-code-bg);' + LineEnding +
'  border: 1px solid var(--pasdoc-border-color);' + LineEnding +
'  border-radius: 0.375rem;' + LineEnding +
'  padding: 1rem;' + LineEnding +
'  overflow-x: auto;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'pre.preformatted {' + LineEnding +
'  font-family: ''SFMono-Regular'', Consolas, ''Liberation Mono'', Menlo, monospace;' + LineEnding +
'  font-size: 0.875em;' + LineEnding +
'  background-color: var(--pasdoc-code-bg);' + LineEnding +
'  border: 1px solid var(--pasdoc-border-color);' + LineEnding +
'  border-radius: 0.375rem;' + LineEnding +
'  padding: 1rem;' + LineEnding +
'  overflow-x: auto;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/* Pascal syntax highlighting */' + LineEnding +
'span.pascal_string { color: #0a3069; }' + LineEnding +
'span.pascal_keyword { font-weight: 600; color: #cf222e; }' + LineEnding +
'span.pascal_comment { color: #6e7781; font-style: italic; }' + LineEnding +
'span.pascal_compiler_comment { color: #116329; }' + LineEnding +
'span.pascal_numeric { color: #0550ae; }' + LineEnding +
'span.pascal_hex { color: #0550ae; }' + LineEnding +
'' + LineEnding +
'/* === Links === */' + LineEnding +
'' + LineEnding +
'a:link, a:visited { color: var(--pasdoc-accent); text-decoration: none; }' + LineEnding +
'a:hover { color: var(--pasdoc-accent-hover); text-decoration: underline; }' + LineEnding +
'' + LineEnding +
'a.bold:link, a.bold:visited { color: var(--pasdoc-accent); font-weight: 600; text-decoration: none; }' + LineEnding +
'a.bold:hover { text-decoration: underline; }' + LineEnding +
'' + LineEnding +
'a.section { color: var(--pasdoc-accent); text-decoration: none; font-weight: 600; }' + LineEnding +
'a.section:hover { text-decoration: none; }' + LineEnding +
'' + LineEnding +
'ul.useslist a:link, ul.useslist a:visited { color: var(--pasdoc-accent); font-weight: 600; }' + LineEnding +
'ul.useslist a:hover { text-decoration: underline; }' + LineEnding +
'' + LineEnding +
'p.unitlink a:link, p.unitlink a:visited { color: var(--pasdoc-accent); font-weight: 600; }' + LineEnding +
'p.unitlink a:hover { text-decoration: underline; }' + LineEnding +
'' + LineEnding +
'/* === Lists === */' + LineEnding +
'' + LineEnding +
'ul.hierarchy { list-style-type: none; padding-left: 1.5rem; }' + LineEnding +
'ul.hierarchylevel { list-style-type: none; padding-left: 1.5rem; }' + LineEnding +
'' + LineEnding +
'ul.compact_spacing p { margin-top: 0; margin-bottom: 0; }' + LineEnding +
'ol.compact_spacing p { margin-top: 0; margin-bottom: 0; }' + LineEnding +
'dl.compact_spacing p { margin-top: 0; margin-bottom: 0; }' + LineEnding +
'' + LineEnding +
'/* === Description elements === */' + LineEnding +
'' + LineEnding +
'p.nodescription {' + LineEnding +
'  color: var(--pasdoc-muted);' + LineEnding +
'  display: none;' + LineEnding +
'}' + LineEnding +
'p.inheritdescription { color: var(--pasdoc-muted); font-style: italic; }' + LineEnding +
'dl.description dt { color: var(--pasdoc-muted); }' + LineEnding +
'dl.parameters dt { color: var(--pasdoc-accent); font-weight: 500; }' + LineEnding +
'' + LineEnding +
'h6.description_section {' + LineEnding +
'  font-size: 1rem;' + LineEnding +
'  font-weight: 600;' + LineEnding +
'  margin-top: 1.25rem;' + LineEnding +
'  margin-bottom: 0.25rem;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/* === Warning and Note tags === */' + LineEnding +
'' + LineEnding +
'dl.tag.warning {' + LineEnding +
'  background-color: #f8d7da;' + LineEnding +
'  border: 1px solid #f5c2c7;' + LineEnding +
'  border-left: 4px solid #dc3545;' + LineEnding +
'  border-radius: 0.375rem;' + LineEnding +
'  padding: 0.75rem 1rem;' + LineEnding +
'  margin: 0.5rem 0;' + LineEnding +
'}' + LineEnding +
'dl.tag.warning dt { color: #842029; }' + LineEnding +
'dl.tag.warning dd { color: #58151c; margin-left: 0; }' + LineEnding +
'' + LineEnding +
'dl.tag.note {' + LineEnding +
'  background-color: #fff3cd;' + LineEnding +
'  border: 1px solid #ffecb5;' + LineEnding +
'  border-left: 4px solid #ffc107;' + LineEnding +
'  border-radius: 0.375rem;' + LineEnding +
'  padding: 0.75rem 1rem;' + LineEnding +
'  margin: 0.5rem 0;' + LineEnding +
'}' + LineEnding +
'dl.tag.note dt { color: #664d03; }' + LineEnding +
'dl.tag.note dd { color: #664d03; margin-left: 0; }' + LineEnding +
'' + LineEnding +
'/* === Misc === */' + LineEnding +
'' + LineEnding +
'p.hint_directive { color: #dc3545; font-style: italic; }' + LineEnding +
'' + LineEnding +
'hr {' + LineEnding +
'  border: 0;' + LineEnding +
'  border-top: 1px solid var(--pasdoc-border-color);' + LineEnding +
'  margin: 1.5rem 0;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'.appinfo {' + LineEnding +
'  float: right;' + LineEnding +
'  text-align: right;' + LineEnding +
'  margin-bottom: 1rem;' + LineEnding +
'  font-size: 0.8rem;' + LineEnding +
'  color: var(--pasdoc-muted);' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'img { border: 0; }' + LineEnding +
'' + LineEnding +
'acronym.mispelling { background-color: #f00; }' + LineEnding +
'' + LineEnding +
'/*' + LineEnding +
'input#search_text { }' + LineEnding +
'input#search_submit_button { }' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'.source_position {' + LineEnding +
'  font-size: 80%;' + LineEnding +
'  color: gray;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/* === Search form === */' + LineEnding +
'' + LineEnding +
'.search-form { white-space: nowrap; }' + LineEnding +
'.search-input input {' + LineEnding +
'  max-width: 80%;' + LineEnding +
'  padding: 0.25rem 0.5rem;' + LineEnding +
'  border: 1px solid #ced4da;' + LineEnding +
'  border-radius: 0.25rem;' + LineEnding +
'  font-size: 0.875rem;' + LineEnding +
'}' + LineEnding +
'.search-input input:focus {' + LineEnding +
'  border-color: var(--pasdoc-accent);' + LineEnding +
'  outline: 0;' + LineEnding +
'  box-shadow: 0 0 0 0.2rem rgba(13, 110, 253, 0.25);' + LineEnding +
'}' + LineEnding +
'.search-input input, .search-button { display: inline-block; vertical-align: middle; }' + LineEnding +
'.search-input { display: inline-block; }' + LineEnding +
'' + LineEnding +
'/* === User-created tables (@table tag) === */' + LineEnding +
'' + LineEnding +
'table.table_tag { border-collapse: collapse; }' + LineEnding +
'table.table_tag td, table.table_tag th {' + LineEnding +
'  border: 1px solid var(--pasdoc-border-color);' + LineEnding +
'  padding: 0.5rem;' + LineEnding +
'}' + LineEnding +
'table.table_tag th {' + LineEnding +
'  background-color: var(--pasdoc-sidebar-bg);' + LineEnding +
'  font-weight: 600;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/* Avoid extra vertical space at beginning/end of table cells */' + LineEnding +
'table.table_tag td > p:first-child,' + LineEnding +
'table.table_tag th > p:first-child,' + LineEnding +
'       td.itemdesc > p:first-child { margin-top: 0; }' + LineEnding +
'' + LineEnding +
'table.table_tag td > p:last-child,' + LineEnding +
'table.table_tag th > p:last-child,' + LineEnding +
'       td.itemdesc > p:last-child { margin-bottom: 0; }' + LineEnding +
''
