{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "tipuesearch.js" }
'' + LineEnding +
'/*' + LineEnding +
'Tipue Search 3.0.1' + LineEnding +
'Copyright (c) 2013 Tipue' + LineEnding +
'Tipue Search is released under the MIT License' + LineEnding +
'http://www.tipue.com/search' + LineEnding +
'*/ ' + LineEnding +
'' + LineEnding +
'' + LineEnding +
'(function($) {' + LineEnding +
'' + LineEnding +
'     $.fn.tipuesearch = function(options) {' + LineEnding +
'' + LineEnding +
'          var set = $.extend( {' + LineEnding +
'          ' + LineEnding +
'               ''show''                   : 7,' + LineEnding +
'               ''newWindow''              : false,' + LineEnding +
'               ''showURL''                : true,' + LineEnding +
'               ''minimumLength''          : 3,' + LineEnding +
'               ''descriptiveWords''       : 25,' + LineEnding +
'               ''highlightTerms''         : true,' + LineEnding +
'               ''highlightEveryTerm''     : false,' + LineEnding +
'               ''mode''                   : ''static'',' + LineEnding +
'               ''liveDescription''        : ''*'',' + LineEnding +
'               ''liveContent''            : ''*'',' + LineEnding +
'               ''contentLocation''        : ''tipuesearch/tipuesearch_content.json''' + LineEnding +
'          ' + LineEnding +
'          }, options);' + LineEnding +
'          ' + LineEnding +
'          return this.each(function() {' + LineEnding +
'' + LineEnding +
'               var tipuesearch_in = {' + LineEnding +
'                    pages: []' + LineEnding +
'               };' + LineEnding +
'               $.ajaxSetup({' + LineEnding +
'                    async: false' + LineEnding +
'               });' + LineEnding +
'' + LineEnding +
'               if (set.mode == ''live'')' + LineEnding +
'               {' + LineEnding +
'                    for (var i = 0; i < tipuesearch_pages.length; i++)' + LineEnding +
'                    {' + LineEnding +
'                         $.get(tipuesearch_pages[i], '''',' + LineEnding +
'                              function (html)' + LineEnding +
'                              {' + LineEnding +
'                                   var cont = $(set.liveContent, html).text();' + LineEnding +
'                                   cont = cont.replace(/\s+/g, '' '');' + LineEnding +
'                                   var desc = $(set.liveDescription, html).text();' + LineEnding +
'                                   desc = desc.replace(/\s+/g, '' '');' + LineEnding +
'                                                                      ' + LineEnding +
'                                   var t_1 = html.toLowerCase().indexOf(''<title>'');' + LineEnding +
'                                   var t_2 = html.toLowerCase().indexOf(''</title>'', t_1 + 7);' + LineEnding +
'                                   if (t_1 != -1 && t_2 != -1)' + LineEnding +
'                                   {' + LineEnding +
'                                        var tit = html.slice(t_1 + 7, t_2);' + LineEnding +
'                                   }' + LineEnding +
'                                   else' + LineEnding +
'                                   {' + LineEnding +
'                                        var tit = ''No title'';' + LineEnding +
'                                   }' + LineEnding +
'' + LineEnding +
'                                   tipuesearch_in.pages.push({' + LineEnding +
'                                        "title": tit,' + LineEnding +
'                                        "text": desc,' + LineEnding +
'                                        "tags": cont,' + LineEnding +
'                                        "loc": tipuesearch_pages[i] ' + LineEnding +
'                                   });    ' + LineEnding +
'                              }' + LineEnding +
'                         );' + LineEnding +
'                    }' + LineEnding +
'               }' + LineEnding +
'               ' + LineEnding +
'               if (set.mode == ''json'')' + LineEnding +
'               {' + LineEnding +
'                    $.getJSON(set.contentLocation,' + LineEnding +
'                         function(json)' + LineEnding +
'                         {' + LineEnding +
'                              tipuesearch_in = $.extend({}, json);' + LineEnding +
'                         }' + LineEnding +
'                    );' + LineEnding +
'               }' + LineEnding +
'' + LineEnding +
'               if (set.mode == ''static'')' + LineEnding +
'               {' + LineEnding +
'                    tipuesearch_in = $.extend({}, tipuesearch);' + LineEnding +
'               }                              ' + LineEnding +
'               ' + LineEnding +
'               var tipue_search_w = '''';' + LineEnding +
'               if (set.newWindow)' + LineEnding +
'               {' + LineEnding +
'                    tipue_search_w = '' target="_blank"'';      ' + LineEnding +
'               }' + LineEnding +
'' + LineEnding +
'               function getURLP(name)' + LineEnding +
'               {' + LineEnding +
'                    return decodeURIComponent((new RegExp(''[?|&]'' + name + ''='' + ''([^&;]+?)(&|#|;|$)'').exec(location.search)||[,""])[1].replace(/\+/g, ''%20'')) || null;' + LineEnding +
'               }' + LineEnding +
'               if (getURLP(''q''))' + LineEnding +
'               {' + LineEnding +
'                    $(''#tipue_search_input'').val(getURLP(''q''));' + LineEnding +
'                    getTipueSearch(0, true);' + LineEnding +
'               }               ' + LineEnding +
'                             ' + LineEnding +
'               $(''#tipue_search_button'').click(function()' + LineEnding +
'               {' + LineEnding +
'                    getTipueSearch(0, true);' + LineEnding +
'               });' + LineEnding +
'               $(this).keyup(function(event)' + LineEnding +
'               {' + LineEnding +
'                    if(event.keyCode == ''13'')' + LineEnding +
'                    {' + LineEnding +
'                         getTipueSearch(0, true);' + LineEnding +
'                    }' + LineEnding +
'               });' + LineEnding +
'' + LineEnding +
'               function getTipueSearch(start, replace)' + LineEnding +
'               {' + LineEnding +
'                    $(''#tipue_search_content'').hide();' + LineEnding +
'                    var out = '''';' + LineEnding +
'                    var results = '''';' + LineEnding +
'                    var show_replace = false;' + LineEnding +
'                    var show_stop = false;' + LineEnding +
'                    ' + LineEnding +
'                    var d = $(''#tipue_search_input'').val().toLowerCase();' + LineEnding +
'                    d = $.trim(d);' + LineEnding +
'                    var d_w = d.split('' '');' + LineEnding +
'                    d = '''';' + LineEnding +
'                    for (var i = 0; i < d_w.length; i++)' + LineEnding +
'                    {' + LineEnding +
'                         var a_w = true;' + LineEnding +
'                         for (var f = 0; f < tipuesearch_stop_words.length; f++)' + LineEnding +
'                         {' + LineEnding +
'                              if (d_w[i] == tipuesearch_stop_words[f])' + LineEnding +
'                              {' + LineEnding +
'                                   a_w = false;' + LineEnding +
'                                   show_stop = true;          ' + LineEnding +
'                              }' + LineEnding +
'                         }' + LineEnding +
'                         if (a_w)' + LineEnding +
'                         {' + LineEnding +
'                              d = d + '' '' + d_w[i];' + LineEnding +
'                         }' + LineEnding +
'                    }' + LineEnding +
'                    d = $.trim(d);' + LineEnding +
'                    d_w = d.split('' '');' + LineEnding +
'                    ' + LineEnding +
'                    if (d.length >= set.minimumLength)' + LineEnding +
'                    {' + LineEnding +
'                         if (replace)' + LineEnding +
'                         {' + LineEnding +
'                              var d_r = d;' + LineEnding +
'                              for (var i = 0; i < d_w.length; i++)' + LineEnding +
'                              {' + LineEnding +
'                                   for (var f = 0; f < tipuesearch_replace.words.length; f++)' + LineEnding +
'                                   {' + LineEnding +
'                                        if (d_w[i] == tipuesearch_replace.words[f].word)' + LineEnding +
'                                        {' + LineEnding +
'                                             d = d.replace(d_w[i], tipuesearch_replace.words[f].replace_with);' + LineEnding +
'                                             show_replace = true;' + LineEnding +
'                                        }' + LineEnding +
'                                   }' + LineEnding +
'                              }' + LineEnding +
'                              d_w = d.split('' '');' + LineEnding +
'                         }                   ' + LineEnding +
'                         ' + LineEnding +
'                         var d_t = d;' + LineEnding +
'                         for (var i = 0; i < d_w.length; i++)' + LineEnding +
'                         {' + LineEnding +
'                              for (var f = 0; f < tipuesearch_stem.words.length; f++)' + LineEnding +
'                              {' + LineEnding +
'                                   if (d_w[i] == tipuesearch_stem.words[f].word)' + LineEnding +
'                                   {' + LineEnding +
'                                        d_t = d_t + '' '' + tipuesearch_stem.words[f].stem;' + LineEnding +
'                                   }' + LineEnding +
'                              }' + LineEnding +
'                         }' + LineEnding +
'                         d_w = d_t.split('' '');' + LineEnding +
'' + LineEnding +
'                         var c = 0;' + LineEnding +
'                         found = new Array();' + LineEnding +
'                         for (var i = 0; i < tipuesearch_in.pages.length; i++)' + LineEnding +
'                         {' + LineEnding +
'                              var score = 1000000000;' + LineEnding +
'                              var s_t = tipuesearch_in.pages[i].text;' + LineEnding +
'                              for (var f = 0; f < d_w.length; f++)' + LineEnding +
'                              {' + LineEnding +
'                                   var pat = new RegExp(d_w[f], ''i'');' + LineEnding +
'                                   if (tipuesearch_in.pages[i].title.search(pat) != -1)' + LineEnding +
'                                   {' + LineEnding +
'                                        score -= (200000 - i);' + LineEnding +
'                                   }' + LineEnding +
'                                   if (tipuesearch_in.pages[i].text.search(pat) != -1)' + LineEnding +
'                                   {' + LineEnding +
'                                        score -= (150000 - i);' + LineEnding +
'                                   }' + LineEnding +
'                                   ' + LineEnding +
'                                   if (set.highlightTerms)' + LineEnding +
'                                   {' + LineEnding +
'                                        if (set.highlightEveryTerm) ' + LineEnding +
'                                        {' + LineEnding +
'                                             var patr = new RegExp(''('' + d_w[f] + '')'', ''gi'');' + LineEnding +
'                                        }' + LineEnding +
'                                        else' + LineEnding +
'                                        {' + LineEnding +
'                                             var patr = new RegExp(''('' + d_w[f] + '')'', ''i'');' + LineEnding +
'                                        }' + LineEnding +
'                                        s_t = s_t.replace(patr, "<b>$1</b>");' + LineEnding +
'                                   }' + LineEnding +
'                                   if (tipuesearch_in.pages[i].tags.search(pat) != -1)' + LineEnding +
'                                   {' + LineEnding +
'                                        score -= (100000 - i);' + LineEnding +
'                                   }' + LineEnding +
'                    ' + LineEnding +
'                              }' + LineEnding +
'                              if (score < 1000000000)' + LineEnding +
'                              {' + LineEnding +
'                                   found[c++] = score + ''^'' + tipuesearch_in.pages[i].title + ''^'' + s_t + ''^'' + tipuesearch_in.pages[i].loc;                                                                   ' + LineEnding +
'                              }' + LineEnding +
'                         }                         ' + LineEnding +
'                         ' + LineEnding +
'                         if (c != 0)' + LineEnding +
'                         {' + LineEnding +
'                              if (show_replace == 1)' + LineEnding +
'                              {' + LineEnding +
'                                   out += ''<div id="tipue_search_warning_head">Showing results for '' + d + ''</div>'';' + LineEnding +
'                                   out += ''<div id="tipue_search_warning">Search for <a href="javascript:void(0)" id="tipue_search_replaced">'' + d_r + ''</a></div>''; ' + LineEnding +
'                              }' + LineEnding +
'                              if (c == 1)' + LineEnding +
'                              {' + LineEnding +
'                                   out += ''<div id="tipue_search_results_count">1 result</div>'';' + LineEnding +
'                              }' + LineEnding +
'                              else' + LineEnding +
'                              {' + LineEnding +
'                                   c_c = c.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");' + LineEnding +
'                                   out += ''<div id="tipue_search_results_count">'' + c_c + '' results</div>'';' + LineEnding +
'                              }' + LineEnding +
'                              ' + LineEnding +
'                              found.sort();' + LineEnding +
'                              var l_o = 0;' + LineEnding +
'                              for (var i = 0; i < found.length; i++)' + LineEnding +
'                              {' + LineEnding +
'                                   var fo = found[i].split(''^'');' + LineEnding +
'                                   if (l_o >= start && l_o < set.show + start)' + LineEnding +
'                                   {' + LineEnding +
'                                        out += ''<div class="tipue_search_content_title"><a href="'' + fo[3] + ''"'' + tipue_search_w + ''>'' +  fo[1] + ''</a></div>'';' + LineEnding +
'                                                                                ' + LineEnding +
'                                        var t = fo[2];' + LineEnding +
'                                        var t_d = '''';' + LineEnding +
'                                        var t_w = t.split('' '');' + LineEnding +
'                                        if (t_w.length < set.descriptiveWords)' + LineEnding +
'                                        {' + LineEnding +
'                                             t_d = t;' + LineEnding +
'                                        }' + LineEnding +
'                                        else' + LineEnding +
'                                        {' + LineEnding +
'                                             for (var f = 0; f < set.descriptiveWords; f++)' + LineEnding +
'                                             {' + LineEnding +
'                                                  t_d += t_w[f] + '' ''; 	' + LineEnding +
'                                             }' + LineEnding +
'                                        }' + LineEnding +
'                                        t_d = $.trim(t_d);' + LineEnding +
'                                        if (t_d.charAt(t_d.length - 1) != ''.'')' + LineEnding +
'                                        {' + LineEnding +
'                                             t_d += '' ...'';' + LineEnding +
'                                        }' + LineEnding +
'                                        out += ''<div class="tipue_search_content_text">'' + t_d + ''</div>'';' + LineEnding +
'                                        ' + LineEnding +
'                                        if (set.showURL)' + LineEnding +
'                                        {' + LineEnding +
'                                             t_url = fo[3];' + LineEnding +
'                                             if (t_url.length > 45)' + LineEnding +
'                                             {' + LineEnding +
'                                                  t_url = fo[3].substr(0, 45) + '' ...'';' + LineEnding +
'                                             }    ' + LineEnding +
'                                             out += ''<div class="tipue_search_content_loc"><a href="'' + fo[3] + ''"'' + tipue_search_w + ''>'' + t_url + ''</a></div>'';' + LineEnding +
'                                        }' + LineEnding +
'                                   }' + LineEnding +
'                                   l_o++;     ' + LineEnding +
'                              }' + LineEnding +
'                              ' + LineEnding +
'                              if (c > set.show)' + LineEnding +
'                              {' + LineEnding +
'                                   var pages = Math.ceil(c / set.show);' + LineEnding +
'                                   var page = (start / set.show);' + LineEnding +
'                                   out += ''<div id="tipue_search_foot"><ul id="tipue_search_foot_boxes">'';' + LineEnding +
'                                   ' + LineEnding +
'                                   if (start > 0)' + LineEnding +
'                                   {' + LineEnding +
'                                       out += ''<li><a href="javascript:void(0)" class="tipue_search_foot_box" id="'' + (start - set.show) + ''_'' + replace + ''">Prev</a></li>''; ' + LineEnding +
'                                   }' + LineEnding +
'                                                       ' + LineEnding +
'                                   if (page <= 2)' + LineEnding +
'                                   {' + LineEnding +
'                                        var p_b = pages;' + LineEnding +
'                                        if (pages > 3)' + LineEnding +
'                                        {' + LineEnding +
'                                             p_b = 3;' + LineEnding +
'                                        }                    ' + LineEnding +
'                                        for (var f = 0; f < p_b; f++)' + LineEnding +
'                                        {' + LineEnding +
'                                             if (f == page)' + LineEnding +
'                                             {' + LineEnding +
'                                                  out += ''<li class="current">'' + (f + 1) + ''</li>'';' + LineEnding +
'                                             }' + LineEnding +
'                                             else' + LineEnding +
'                                             {' + LineEnding +
'                                                  out += ''<li><a href="javascript:void(0)" class="tipue_search_foot_box" id="'' + (f * set.show) + ''_'' + replace + ''">'' + (f + 1) + ''</a></li>'';' + LineEnding +
'                                             }' + LineEnding +
'                                        }' + LineEnding +
'                                   }' + LineEnding +
'                                   else' + LineEnding +
'                                   {' + LineEnding +
'                                        var p_b = page + 3;' + LineEnding +
'                                        if (p_b > pages)' + LineEnding +
'                                        {' + LineEnding +
'                                             p_b = pages; ' + LineEnding +
'                                        }' + LineEnding +
'                                        for (var f = page; f < p_b; f++)' + LineEnding +
'                                        {' + LineEnding +
'                                             if (f == page)' + LineEnding +
'                                             {' + LineEnding +
'                                                  out += ''<li class="current">'' + (f + 1) + ''</li>'';' + LineEnding +
'                                             }' + LineEnding +
'                                             else' + LineEnding +
'                                             {' + LineEnding +
'                                                  out += ''<li><a href="javascript:void(0)" class="tipue_search_foot_box" id="'' + (f * set.show) + ''_'' + replace + ''">'' + (f + 1) + ''</a></li>'';' + LineEnding +
'                                             }' + LineEnding +
'                                        }                         ' + LineEnding +
'                                   }' + LineEnding +
'                                                      ' + LineEnding +
'                                   if (page + 1 != pages)' + LineEnding +
'                                   {' + LineEnding +
'                                       out += ''<li><a href="javascript:void(0)" class="tipue_search_foot_box" id="'' + (start + set.show) + ''_'' + replace + ''">Next</a></li>''; ' + LineEnding +
'                                   }                    ' + LineEnding +
'                                   ' + LineEnding +
'                                   out += ''</ul></div>'';' + LineEnding +
'                              }                        ' + LineEnding +
'                         }' + LineEnding +
'                         else' + LineEnding +
'                         {' + LineEnding +
'                              out += ''<div id="tipue_search_warning_head">Nothing found</div>''; ' + LineEnding +
'                         }' + LineEnding +
'                    }' + LineEnding +
'                    else' + LineEnding +
'                    {' + LineEnding +
'                         if (show_stop)' + LineEnding +
'                         {' + LineEnding +
'                              out += ''<div id="tipue_search_warning_head">Nothing found</div><div id="tipue_search_warning">Common words are largely ignored</div>'';     ' + LineEnding +
'                         }' + LineEnding +
'                         else' + LineEnding +
'                         {' + LineEnding +
'                              out += ''<div id="tipue_search_warning_head">Search too short</div>'';' + LineEnding +
'                              if (set.minimumLength == 1)' + LineEnding +
'                              {' + LineEnding +
'                                   out += ''<div id="tipue_search_warning">Should be one character or more</div>'';' + LineEnding +
'                              }' + LineEnding +
'                              else' + LineEnding +
'                              {' + LineEnding +
'                                   out += ''<div id="tipue_search_warning">Should be '' + set.minimumLength + '' characters or more</div>'';' + LineEnding +
'                              }' + LineEnding +
'                         }' + LineEnding +
'                    }' + LineEnding +
'               ' + LineEnding +
'                    $(''#tipue_search_content'').html(out);' + LineEnding +
'                    $(''#tipue_search_content'').slideDown(200);' + LineEnding +
'                    ' + LineEnding +
'                    $(''#tipue_search_replaced'').click(function()' + LineEnding +
'                    {' + LineEnding +
'                         getTipueSearch(0, false);' + LineEnding +
'                    });                ' + LineEnding +
'               ' + LineEnding +
'                    $(''.tipue_search_foot_box'').click(function()' + LineEnding +
'                    {' + LineEnding +
'                         var id_v = $(this).attr(''id'');' + LineEnding +
'                         var id_a = id_v.split(''_'');' + LineEnding +
'                    ' + LineEnding +
'                         getTipueSearch(parseInt(id_a[0]), id_a[1]);' + LineEnding +
'                    });                                                       ' + LineEnding +
'               }          ' + LineEnding +
'          ' + LineEnding +
'          });' + LineEnding +
'     };' + LineEnding +
'   ' + LineEnding +
'})(jQuery);' + LineEnding +
'' + LineEnding +
'' + LineEnding +
'' + LineEnding +
'' + LineEnding +
''
