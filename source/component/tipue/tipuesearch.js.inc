{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "tipuesearch.js" }
'' + LineEnding + 
'/*' + LineEnding + 
'Tipue Search 1.1.1' + LineEnding + 
'Tipue Search Copyright (c) 2012 Tri-State Consultants' + LineEnding + 
'Tipue Search is free for both both commercial and non-commercial use and released under the MIT License.' + LineEnding + 
'For the latest release, documentation and licence see http://www.tipue.com/search' + LineEnding + 
'*/' + LineEnding + 
'' + LineEnding + 
'' + LineEnding + 
'var tipue_search_w = '''';' + LineEnding + 
'if (tipuesearch_new_window == 1)' + LineEnding + 
'{' + LineEnding + 
'     tipue_search_w = '' target="_blank"'';      ' + LineEnding + 
'}' + LineEnding + 
'' + LineEnding + 
'var tipue_search_q = window.location.search;' + LineEnding + 
'if (tipue_search_q)' + LineEnding + 
'{' + LineEnding + 
'     var tipue_search_l_q = tipue_search_q.indexOf(''?q='');' + LineEnding + 
'     var tipue_search_q = tipue_search_q.substring(tipue_search_l_q + 3);' + LineEnding + 
'     tipue_search_q = tipue_search_q.replace(/\+/g, '' '');' + LineEnding + 
'     tipue_search_q = decodeURIComponent(tipue_search_q);' + LineEnding + 
'     $(''#tipue_search_input'').val(tipue_search_q);' + LineEnding + 
'     ' + LineEnding + 
'     getTipueSearch(0, 1);' + LineEnding + 
'}' + LineEnding + 
'' + LineEnding + 
'$(''#tipue_search_button'').click(function()' + LineEnding + 
'{' + LineEnding + 
'     getTipueSearch(0, 1);' + LineEnding + 
'});' + LineEnding + 
'' + LineEnding + 
'$(''#tipue_search_input'').keyup(function(event)' + LineEnding + 
'{' + LineEnding + 
'     if(event.keyCode == ''13'')' + LineEnding + 
'     {' + LineEnding + 
'          getTipueSearch(0, 1);' + LineEnding + 
'     }' + LineEnding + 
'});' + LineEnding + 
'' + LineEnding + 
'function getTipueSearch(start, replace)' + LineEnding + 
'{' + LineEnding + 
'     $(''#tipue_search_content'').hide();     ' + LineEnding + 
'     var out = '''';' + LineEnding + 
'     var results = '''';' + LineEnding + 
'     var show_replace = 0;' + LineEnding + 
'     var show_stop = 0;' + LineEnding + 
'     ' + LineEnding + 
'     var d = $(''#tipue_search_input'').val().toLowerCase();' + LineEnding + 
'     d = $.trim(d);' + LineEnding + 
'     var d_w = d.split('' '');' + LineEnding + 
'          ' + LineEnding + 
'     for (var i = 0; i < d_w.length; i++)' + LineEnding + 
'     {' + LineEnding + 
'          for (var f = 0; f < tipuesearch_stop_words.length; f++)' + LineEnding + 
'          {' + LineEnding + 
'               if (d_w[i] == tipuesearch_stop_words[f])' + LineEnding + 
'               {' + LineEnding + 
'                    d = d.replace(d_w[i], '''');' + LineEnding + 
'                    show_stop = 1;' + LineEnding + 
'               }' + LineEnding + 
'          }' + LineEnding + 
'     }' + LineEnding + 
'     d = $.trim(d);' + LineEnding + 
'     d = d.replace(/\s+/g, '' '');' + LineEnding + 
'     d_w = d.split('' '');' + LineEnding + 
'          ' + LineEnding + 
'     if (d.length >= tipuesearch_minimum_length)' + LineEnding + 
'     {' + LineEnding + 
'          if (replace == 1)' + LineEnding + 
'          {' + LineEnding + 
'               var d_r = d;' + LineEnding + 
'               for (var i = 0; i < d_w.length; i++)' + LineEnding + 
'               {' + LineEnding + 
'                    for (var f = 0; f < tipuesearch_replace.words.length; f++)' + LineEnding + 
'                    {' + LineEnding + 
'                         if (d_w[i] == tipuesearch_replace.words[f].word)' + LineEnding + 
'                         {' + LineEnding + 
'                              d = d.replace(d_w[i], tipuesearch_replace.words[f].replace_with);' + LineEnding + 
'                              show_replace = 1;' + LineEnding + 
'                         }' + LineEnding + 
'                    }' + LineEnding + 
'               }' + LineEnding + 
'               d_w = d.split('' '');' + LineEnding + 
'          }' + LineEnding + 
'          ' + LineEnding + 
'          var d_t = d;' + LineEnding + 
'          for (var i = 0; i < d_w.length; i++)' + LineEnding + 
'          {' + LineEnding + 
'               for (var f = 0; f < tipuesearch_stem.words.length; f++)' + LineEnding + 
'               {' + LineEnding + 
'                    if (d_w[i] == tipuesearch_stem.words[f].word)' + LineEnding + 
'                    {' + LineEnding + 
'                         d_t = d_t + '' '' + tipuesearch_stem.words[f].stem;' + LineEnding + 
'                    }' + LineEnding + 
'               }' + LineEnding + 
'          }' + LineEnding + 
'          d_w = d_t.split('' '');' + LineEnding + 
'         ' + LineEnding + 
'          var c = 0;' + LineEnding + 
'          found = new Array();' + LineEnding + 
'          for (var i = 0; i < tipuesearch.pages.length; i++)' + LineEnding + 
'          {' + LineEnding + 
'               var score = 10000000;' + LineEnding + 
'               for (var f = 0; f < d_w.length; f++)' + LineEnding + 
'               {' + LineEnding + 
'                    var pat = new RegExp(d_w[f], ''i'');' + LineEnding + 
'                    if (tipuesearch.pages[i].title.search(pat) != -1)' + LineEnding + 
'                    {' + LineEnding + 
'                         score -= (2000 - i);' + LineEnding + 
'                    }' + LineEnding + 
'                    if (tipuesearch.pages[i].text.search(pat) != -1)' + LineEnding + 
'                    {' + LineEnding + 
'                         score -= (1500 - i);' + LineEnding + 
'                    }' + LineEnding + 
'                    if (tipuesearch.pages[i].tags.search(pat) != -1)' + LineEnding + 
'                    {' + LineEnding + 
'                         score -= (1000 - i);' + LineEnding + 
'                    }                    ' + LineEnding + 
'               }' + LineEnding + 
'               if (score < 10000000)' + LineEnding + 
'               {' + LineEnding + 
'                    found[c++] = score + ''^'' + tipuesearch.pages[i].title + ''^'' + tipuesearch.pages[i].text + ''^'' + tipuesearch.pages[i].loc;' + LineEnding + 
'               }' + LineEnding + 
'          }' + LineEnding + 
'                   ' + LineEnding + 
'          if (c != 0)' + LineEnding + 
'          {' + LineEnding + 
'               if (show_replace == 1)' + LineEnding + 
'               {' + LineEnding + 
'                    out += ''<div id="tipue_search_warning_head">Showing results for '' + d + ''</div>'';' + LineEnding + 
'                    out += ''<div id="tipue_search_warning">Show results for <a href="#" onclick="getTipueSearch(0, 0)">'' + d_r + ''</a></div>''; ' + LineEnding + 
'               }' + LineEnding + 
'               if (c == 1)' + LineEnding + 
'               {' + LineEnding + 
'                    out += ''<div id="tipue_search_results_count">1 result</div>'';' + LineEnding + 
'               }' + LineEnding + 
'               else' + LineEnding + 
'               {' + LineEnding + 
'                    c_c = c.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");' + LineEnding + 
'                    out += ''<div id="tipue_search_results_count">'' + c_c + '' results</div>'';' + LineEnding + 
'               }' + LineEnding + 
'               ' + LineEnding + 
'               found.sort();' + LineEnding + 
'               var l_o = 0;' + LineEnding + 
'               for (var i = 0; i < found.length; i++)' + LineEnding + 
'               {' + LineEnding + 
'                    var fo = found[i].split(''^'');' + LineEnding + 
'                    if (l_o >= start && l_o < tipuesearch_show + start)' + LineEnding + 
'                    {' + LineEnding + 
'                         out += ''<div id="tipue_search_content_title"><a href="'' + fo[3] + ''"'' + tipue_search_w + ''>'' +  fo[1] + ''</a></div>'';' + LineEnding + 
'  ' + LineEnding + 
'                         var t = fo[2];' + LineEnding + 
'                         var t_d = '''';' + LineEnding + 
'                         var t_w = t.split('' '');' + LineEnding + 
'                         if (t_w.length < tipuesearch_descriptive_words)' + LineEnding + 
'                         {' + LineEnding + 
'                              t_d = t;' + LineEnding + 
'                         }' + LineEnding + 
'                         else' + LineEnding + 
'                         {' + LineEnding + 
'                              for (var f = 0; f < tipuesearch_descriptive_words; f++)' + LineEnding + 
'                              {' + LineEnding + 
'                                   t_d += t_w[f] + '' ''; 	' + LineEnding + 
'                              }' + LineEnding + 
'                         }' + LineEnding + 
'                         t_d = $.trim(t_d);' + LineEnding + 
'                         if (t_d.charAt(t_d.length - 1) != ''.'')' + LineEnding + 
'                         {' + LineEnding + 
'                              t_d += '' ...'';' + LineEnding + 
'                         }' + LineEnding + 
'                         out += ''<div id="tipue_search_content_text">'' + t_d + ''</div>'';' + LineEnding + 
'                         ' + LineEnding + 
'                         if (tipuesearch_show_url == 1)' + LineEnding + 
'                         {' + LineEnding + 
'                              out += ''<div id="tipue_search_content_loc"><a href="'' + fo[3] + ''"'' + tipue_search_w + ''>'' + fo[3] + ''</a></div>'';' + LineEnding + 
'                         }' + LineEnding + 
'                    }' + LineEnding + 
'                    l_o++;     ' + LineEnding + 
'               }' + LineEnding + 
'                              ' + LineEnding + 
'               if (c > tipuesearch_show)' + LineEnding + 
'               {' + LineEnding + 
'                    var pages = Math.ceil(c / tipuesearch_show);' + LineEnding + 
'                    var page = (start / tipuesearch_show);' + LineEnding + 
'                    out += ''<div id="tipue_search_foot"><ul id="tipue_search_foot_boxes">'';' + LineEnding + 
'                    ' + LineEnding + 
'                    if (start > 0)' + LineEnding + 
'                    {' + LineEnding + 
'                        out += ''<li><a href="#" onclick="getTipueSearch('' + (start - tipuesearch_show) + '', '' + replace + '')">&#171; Previous</a></li>''; ' + LineEnding + 
'                    }' + LineEnding + 
'                                        ' + LineEnding + 
'                    if (page <= 4)' + LineEnding + 
'                    {' + LineEnding + 
'                         var p_b = pages;' + LineEnding + 
'                         if (pages > 5)' + LineEnding + 
'                         {' + LineEnding + 
'                              p_b = 5;' + LineEnding + 
'                         }                    ' + LineEnding + 
'                         for (var f = 0; f < p_b; f++)' + LineEnding + 
'                         {' + LineEnding + 
'                              if (f == page)' + LineEnding + 
'                              {' + LineEnding + 
'                                   out += ''<li class="current">'' + (f + 1) + ''</li>'';' + LineEnding + 
'                              }' + LineEnding + 
'                              else' + LineEnding + 
'                              {' + LineEnding + 
'                                   out += ''<li><a href="#" onclick="getTipueSearch('' + (f * tipuesearch_show) + '', '' + replace + '')">'' + (f + 1) + ''</a></li>'';' + LineEnding + 
'                              }' + LineEnding + 
'                         }' + LineEnding + 
'                    }' + LineEnding + 
'                    else' + LineEnding + 
'                    {' + LineEnding + 
'                         var p_b = pages + 4;' + LineEnding + 
'                         if (p_b > pages)' + LineEnding + 
'                         {' + LineEnding + 
'                              p_b = pages; ' + LineEnding + 
'                         }' + LineEnding + 
'                         for (var f = page; f < p_b; f++)' + LineEnding + 
'                         {' + LineEnding + 
'                              if (f == page)' + LineEnding + 
'                              {' + LineEnding + 
'                                   out += ''<li class="current">'' + (f + 1) + ''</li>'';' + LineEnding + 
'                              }' + LineEnding + 
'                              else' + LineEnding + 
'                              {' + LineEnding + 
'                                   out += ''<li><a href="#" onclick="getTipueSearch('' + (f * tipuesearch_show) + '', '' + replace + '')">'' + (f + 1) + ''</a></li>'';' + LineEnding + 
'                              }' + LineEnding + 
'                         }                         ' + LineEnding + 
'                    }' + LineEnding + 
'                                       ' + LineEnding + 
'                    if (page + 1 != pages)' + LineEnding + 
'                    {' + LineEnding + 
'                        out += ''<li><a href="#" onclick="getTipueSearch('' + (start + tipuesearch_show) + '', '' + replace + '')">Next &#187;</a></li>''; ' + LineEnding + 
'                    }                    ' + LineEnding + 
'                    ' + LineEnding + 
'                    out += ''</ul></div>'';' + LineEnding + 
'               }' + LineEnding + 
'          }' + LineEnding + 
'          else' + LineEnding + 
'          {' + LineEnding + 
'               out += ''<div id="tipue_search_warning_head">Nothing found</div>''; ' + LineEnding + 
'          }          ' + LineEnding + 
'     }' + LineEnding + 
'     else' + LineEnding + 
'     {' + LineEnding + 
'          if (show_stop == 1)' + LineEnding + 
'          {' + LineEnding + 
'               out += ''<div id="tipue_search_warning_head">Nothing found</div><div id="tipue_search_warning">Common words are largely ignored</div>'';     ' + LineEnding + 
'          }' + LineEnding + 
'          else' + LineEnding + 
'          {' + LineEnding + 
'               out += ''<div id="tipue_search_warning_head">Search too short</div>'';' + LineEnding + 
'               if (tipuesearch_minimum_length == 1)' + LineEnding + 
'               {' + LineEnding + 
'                    out += ''<div id="tipue_search_warning">Should be one character or more</div>'';' + LineEnding + 
'               }' + LineEnding + 
'               else' + LineEnding + 
'               {' + LineEnding + 
'                    out += ''<div id="tipue_search_warning">Should be '' + tipuesearch_minimum_length + '' characters or more</div>'';' + LineEnding + 
'               }' + LineEnding + 
'          }' + LineEnding + 
'     }' + LineEnding + 
'     ' + LineEnding + 
'     $(''#tipue_search_content'').html(out);' + LineEnding + 
'     $(''#tipue_search_content'').slideDown(200);' + LineEnding + 
'}' + LineEnding + 
'' + LineEnding + 
'' + LineEnding + 
''
