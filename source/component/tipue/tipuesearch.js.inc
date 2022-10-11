{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "tipuesearch.js" }
'' + LineEnding +
'/*' + LineEnding +
'Tipue Search 7.1' + LineEnding +
'Copyright (c) 2019 Tipue' + LineEnding +
'Tipue Search is released under the MIT License' + LineEnding +
'http://www.tipue.com/search' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'' + LineEnding +
'(function($) {' + LineEnding +
'' + LineEnding +
'     $.fn.tipuesearch = function(options) {' + LineEnding +
'' + LineEnding +
'          var set = $.extend( {' + LineEnding +
'          ' + LineEnding +
'          ''contextBuffer''          : 60,' + LineEnding +
'          ''contextLength''          : 60,' + LineEnding +
'          ''contextStart''           : 90,' + LineEnding +
'          ''debug''                  : false,' + LineEnding +
'          ''descriptiveWords''       : 25,' + LineEnding +
'          ''footerPages''            : 3,' + LineEnding +
'          ''highlightTerms''         : true,' + LineEnding +
'          ''imageZoom''              : true,' + LineEnding +
'          ''minimumLength''          : ${TIPUE_MINIMUM_SEARCH_LENGTH},' + LineEnding +
'          ''newWindow''              : false,' + LineEnding +
'          ''show''                   : 10,' + LineEnding +
'          ''showContext''            : true,' + LineEnding +
'          ''showRelated''            : true,' + LineEnding +
'          ''showTime''               : true,' + LineEnding +
'          ''showTitleCount''         : true,' + LineEnding +
'          ''showURL''                : true,' + LineEnding +
'          ''wholeWords''             : true' + LineEnding +
'          }, options);' + LineEnding +
'          ' + LineEnding +
'          return this.each(function() {' + LineEnding +
'               ' + LineEnding +
'               var tipuesearch_t_c = 0;                         ' + LineEnding +
'               ' + LineEnding +
'               var tipue_search_w = '''';' + LineEnding +
'               if (set.newWindow)' + LineEnding +
'               {' + LineEnding +
'                    tipue_search_w = '' target="_blank"'';      ' + LineEnding +
'               }' + LineEnding +
'' + LineEnding +
'               function getURLP(name)' + LineEnding +
'               {' + LineEnding +
'                    var locSearch = location.search;' + LineEnding +
'                    var splitted = (new RegExp(''[?|&]'' + name + ''='' + ''([^&;]+?)(&|#|;|$)'').exec(locSearch)||[,""]);' + LineEnding +
'                    var searchString = splitted[1].replace(/\+/g, ''%20'');' + LineEnding +
'                    try' + LineEnding +
'                    {' + LineEnding +
'                         searchString = decodeURIComponent(searchString);' + LineEnding +
'                    }' + LineEnding +
'                    catch(e)' + LineEnding +
'                    {' + LineEnding +
'                         searchString = unescape(searchString);' + LineEnding +
'                    }' + LineEnding +
'                    return searchString || null;' + LineEnding +
'               }' + LineEnding +
'               ' + LineEnding +
'               if (getURLP(''q''))' + LineEnding +
'               {' + LineEnding +
'                    $(''#tipue_search_input'').val(getURLP(''q''));' + LineEnding +
'                    getTipueSearch(0, true);' + LineEnding +
'               }               ' + LineEnding +
'               ' + LineEnding +
'               $(this).keyup(function(event)' + LineEnding +
'               {' + LineEnding +
'                    if(event.keyCode == ''13'')' + LineEnding +
'                    {' + LineEnding +
'                         getTipueSearch(0, true);' + LineEnding +
'                    }' + LineEnding +
'               });' + LineEnding +
'               ' + LineEnding +
'' + LineEnding +
'               function getTipueSearch(start, replace)' + LineEnding +
'               {' + LineEnding +
'                    window.scrollTo(0, 0);' + LineEnding +
'                    ' + LineEnding +
'                    var out = '''';' + LineEnding +
'                    var show_replace = false;' + LineEnding +
'                    var show_stop = false;' + LineEnding +
'                    var standard = true;' + LineEnding +
'                    var c = 0;' + LineEnding +
'                    var found = [];' + LineEnding +
'                    ' + LineEnding +
'                    var d_o = $(''#tipue_search_input'').val();' + LineEnding +
'                    d_o = d_o.replace(/\+/g, '' '').replace(/\s\s+/g, '' '');' + LineEnding +
' ' + LineEnding +
'                    d_o = $.trim(d_o);' + LineEnding +
'                    var d = d_o.toLowerCase();' + LineEnding +
'                    ' + LineEnding +
'                    if ((d.match("^\"") && d.match("\"$")) || (d.match("^''") && d.match("''$")))' + LineEnding +
'                    {' + LineEnding +
'                         standard = false;' + LineEnding +
'                    }' + LineEnding +
'                    ' + LineEnding +
'                    var d_w = d.split('' '');' + LineEnding +
'                    ' + LineEnding +
'                    if (standard)' + LineEnding +
'                    {' + LineEnding +
'                         d = '''';' + LineEnding +
'                         for (var i = 0; i < d_w.length; i++)' + LineEnding +
'                         {' + LineEnding +
'                              var a_w = true;' + LineEnding +
'                              for (var f = 0; f < tipuesearch_stop_words.length; f++)' + LineEnding +
'                              {' + LineEnding +
'                                   if (d_w[i] == tipuesearch_stop_words[f])' + LineEnding +
'                                   {' + LineEnding +
'                                        a_w = false;' + LineEnding +
'                                        show_stop = true;          ' + LineEnding +
'                                   }' + LineEnding +
'                              }' + LineEnding +
'                              if (a_w)' + LineEnding +
'                              {' + LineEnding +
'                                   d = d + '' '' + d_w[i];' + LineEnding +
'                              }' + LineEnding +
'                         }' + LineEnding +
'                         d = $.trim(d);' + LineEnding +
'                         d_w = d.split('' '');' + LineEnding +
'                    }' + LineEnding +
'                    else' + LineEnding +
'                    {' + LineEnding +
'                         d = d.substring(1, d.length - 1);' + LineEnding +
'                    }' + LineEnding +
'               ' + LineEnding +
'                    if (d.length >= set.minimumLength)' + LineEnding +
'                    {' + LineEnding +
'                         if (standard)' + LineEnding +
'                         {' + LineEnding +
'                              if (replace)' + LineEnding +
'                              {' + LineEnding +
'                                   var d_r = d;' + LineEnding +
'                                   for (var i = 0; i < d_w.length; i++)' + LineEnding +
'                                   {' + LineEnding +
'                                        for (var f = 0; f < tipuesearch_replace.words.length; f++)' + LineEnding +
'                                        {' + LineEnding +
'                                             if (d_w[i] == tipuesearch_replace.words[f].word)' + LineEnding +
'                                             {' + LineEnding +
'                                                  d = d.replace(d_w[i], tipuesearch_replace.words[f].replace_with);' + LineEnding +
'                                                  show_replace = true;' + LineEnding +
'                                             }' + LineEnding +
'                                        }' + LineEnding +
'                                   }' + LineEnding +
'                                   d_w = d.split('' '');' + LineEnding +
'                              }                   ' + LineEnding +
'                    ' + LineEnding +
'                              var d_t = d;' + LineEnding +
'                              for (var i = 0; i < d_w.length; i++)' + LineEnding +
'                              {' + LineEnding +
'                                   for (var f = 0; f < tipuesearch_stem.words.length; f++)' + LineEnding +
'                                   {' + LineEnding +
'                                        if (d_w[i] == tipuesearch_stem.words[f].word)' + LineEnding +
'                                        {' + LineEnding +
'                                             d_t = d_t + '' '' + tipuesearch_stem.words[f].stem;' + LineEnding +
'                                        }' + LineEnding +
'                                   }' + LineEnding +
'                              }' + LineEnding +
'                              d_w = d_t.split('' '');' + LineEnding +
'' + LineEnding +
'                              for (var i = 0; i < tipuesearch.pages.length; i++)' + LineEnding +
'                              {' + LineEnding +
'                                   var score = 0;' + LineEnding +
'                                   var s_t = tipuesearch.pages[i].text;' + LineEnding +
'                                   for (var f = 0; f < d_w.length; f++)' + LineEnding +
'                                   {' + LineEnding +
'                                        if (set.wholeWords)' + LineEnding +
'                                        {' + LineEnding +
'                                             var pat = new RegExp(''\\b'' + d_w[f] + ''\\b'', ''gi'');' + LineEnding +
'                                        }' + LineEnding +
'                                        else' + LineEnding +
'                                        {' + LineEnding +
'                                             var pat = new RegExp(d_w[f], ''gi'');' + LineEnding +
'                                        }' + LineEnding +
'                                        if (tipuesearch.pages[i].title.search(pat) != -1)' + LineEnding +
'                                        {' + LineEnding +
'                                             var m_c = tipuesearch.pages[i].title.match(pat).length;' + LineEnding +
'                                             score += (20 * m_c);' + LineEnding +
'                                        }' + LineEnding +
'                                        if (tipuesearch.pages[i].text.search(pat) != -1)' + LineEnding +
'                                        {' + LineEnding +
'                                             var m_c = tipuesearch.pages[i].text.match(pat).length;' + LineEnding +
'                                             score += (20 * m_c);' + LineEnding +
'                                        }' + LineEnding +
'                                        if (tipuesearch.pages[i].tags)' + LineEnding +
'                                        {' + LineEnding +
'                                             if (tipuesearch.pages[i].tags.search(pat) != -1)' + LineEnding +
'                                             {' + LineEnding +
'                                                  var m_c = tipuesearch.pages[i].tags.match(pat).length;' + LineEnding +
'                                                  score += (10 * m_c);' + LineEnding +
'                                             }' + LineEnding +
'                                        }' + LineEnding +
'                                        if (tipuesearch.pages[i].url.search(pat) != -1)' + LineEnding +
'                                        {' + LineEnding +
'                                             score += 20;' + LineEnding +
'                                        }' + LineEnding +
'                                        ' + LineEnding +
'                                        if (score != 0)' + LineEnding +
'                                        {' + LineEnding +
'                                             for (var e = 0; e < tipuesearch_weight.weight.length; e++)' + LineEnding +
'                                             {' + LineEnding +
'                                                  if (tipuesearch.pages[i].url == tipuesearch_weight.weight[e].url)' + LineEnding +
'                                                  {' + LineEnding +
'                                                       score += tipuesearch_weight.weight[e].score;' + LineEnding +
'                                                  }' + LineEnding +
'                                             }' + LineEnding +
'                                        }' + LineEnding +
'                                        ' + LineEnding +
'                                        if (d_w[f].match(''^-''))' + LineEnding +
'                                        {' + LineEnding +
'                                             pat = new RegExp(d_w[f].substring(1), ''i'');' + LineEnding +
'                                             if (tipuesearch.pages[i].title.search(pat) != -1 || tipuesearch.pages[i].text.search(pat) != -1 || tipuesearch.pages[i].tags.search(pat) != -1)' + LineEnding +
'                                             {' + LineEnding +
'                                                  score = 0;     ' + LineEnding +
'                                             }    ' + LineEnding +
'                                        }' + LineEnding +
'                                   }' + LineEnding +
'                                   ' + LineEnding +
'                                   if (score != 0)' + LineEnding +
'                                   {' + LineEnding +
'                                        found.push(' + LineEnding +
'                                        {' + LineEnding +
'                                             "score": score,' + LineEnding +
'                                             "title": tipuesearch.pages[i].title,' + LineEnding +
'                                             "desc": s_t,' + LineEnding +
'                                             "img": tipuesearch.pages[i].img, ' + LineEnding +
'                                             "url": tipuesearch.pages[i].url,' + LineEnding +
'                                             "note": tipuesearch.pages[i].note' + LineEnding +
'                                        });' + LineEnding +
'                                        c++;                                                                   ' + LineEnding +
'                                   }' + LineEnding +
'                              }' + LineEnding +
'                         }' + LineEnding +
'                         else' + LineEnding +
'                         {' + LineEnding +
'                              for (var i = 0; i < tipuesearch.pages.length; i++)' + LineEnding +
'                              {' + LineEnding +
'                                   var score = 0;' + LineEnding +
'                                   var s_t = tipuesearch.pages[i].text;' + LineEnding +
'                                   var pat = new RegExp(d, ''gi'');' + LineEnding +
'                                   if (tipuesearch.pages[i].title.search(pat) != -1)' + LineEnding +
'                                   {' + LineEnding +
'                                        var m_c = tipuesearch.pages[i].title.match(pat).length;' + LineEnding +
'                                        score += (20 * m_c);' + LineEnding +
'                                   }' + LineEnding +
'                                   if (tipuesearch.pages[i].text.search(pat) != -1)' + LineEnding +
'                                   {' + LineEnding +
'                                        var m_c = tipuesearch.pages[i].text.match(pat).length;' + LineEnding +
'                                        score += (20 * m_c);' + LineEnding +
'                                   }' + LineEnding +
'                                   if (tipuesearch.pages[i].tags)' + LineEnding +
'                                   {' + LineEnding +
'                                        if (tipuesearch.pages[i].tags.search(pat) != -1)' + LineEnding +
'                                        {' + LineEnding +
'                                             var m_c = tipuesearch.pages[i].tags.match(pat).length;' + LineEnding +
'                                             score += (10 * m_c);' + LineEnding +
'                                        }' + LineEnding +
'                                   }' + LineEnding +
'                                   if (tipuesearch.pages[i].url.search(pat) != -1)' + LineEnding +
'                                   {' + LineEnding +
'                                        score += 20;' + LineEnding +
'                                   }' + LineEnding +
'                                   ' + LineEnding +
'                                   if (score != 0)' + LineEnding +
'                                   {' + LineEnding +
'                                        for (var e = 0; e < tipuesearch_weight.weight.length; e++)' + LineEnding +
'                                        {' + LineEnding +
'                                             if (tipuesearch.pages[i].url == tipuesearch_weight.weight[e].url)' + LineEnding +
'                                             {' + LineEnding +
'                                                  score += tipuesearch_weight.weight[e].score;' + LineEnding +
'                                             }' + LineEnding +
'                                        }' + LineEnding +
'                                   }' + LineEnding +
'                              ' + LineEnding +
'                                   if (score != 0)' + LineEnding +
'                                   {' + LineEnding +
'                                        found.push(' + LineEnding +
'                                        {' + LineEnding +
'                                             "score": score,' + LineEnding +
'                                             "title": tipuesearch.pages[i].title,' + LineEnding +
'                                             "desc": s_t,' + LineEnding +
'                                             "img": tipuesearch.pages[i].img,' + LineEnding +
'                                             "url": tipuesearch.pages[i].url,' + LineEnding +
'                                             "note": tipuesearch.pages[i].note' + LineEnding +
'                                        });' + LineEnding +
'                                        c++;                                                                  ' + LineEnding +
'                                   }                              ' + LineEnding +
'                              }' + LineEnding +
'                         }                         ' + LineEnding +
'                         ' + LineEnding +
'                         if (c != 0)' + LineEnding +
'                         {' + LineEnding +
'                              if (set.showTitleCount && tipuesearch_t_c == 0)' + LineEnding +
'                              {' + LineEnding +
'                                   var title = document.title;' + LineEnding +
'                                   document.title = ''('' + c + '') '' + title;' + LineEnding +
'                                   tipuesearch_t_c++;' + LineEnding +
'                              }                         ' + LineEnding +
'' + LineEnding +
'                              if (c == 1)' + LineEnding +
'                              {' + LineEnding +
'                                   out += ''<div id="tipue_search_results_count">'' + tipuesearch_string_4;' + LineEnding +
'                              }' + LineEnding +
'                              else' + LineEnding +
'                              {' + LineEnding +
'                                   var c_c = c.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");' + LineEnding +
'                                   out += ''<div id="tipue_search_results_count">'' + c_c + '' '' + tipuesearch_string_5;' + LineEnding +
'                              }' + LineEnding +
'                              if (set.showTime)' + LineEnding +
'                              {' + LineEnding +
'                                   var endTimer = new Date().getTime();' + LineEnding +
'                                   var time = (endTimer - startTimer) / 1000;' + LineEnding +
'                                   out += '' ('' + time.toFixed(2) + '' '' + tipuesearch_string_14 + '')'';' + LineEnding +
'                                   set.showTime = false;' + LineEnding +
'                              }' + LineEnding +
'                              out += ''</div>'';' + LineEnding +
'                                   ' + LineEnding +
'                              if (set.showRelated && standard)' + LineEnding +
'                              {' + LineEnding +
'                                   var ront = '''';' + LineEnding +
'                                   f = 0;' + LineEnding +
'                                   for (var i = 0; i < tipuesearch_related.Related.length; i++)' + LineEnding +
'                                   {' + LineEnding +
'                                        if (d == tipuesearch_related.Related[i].search)' + LineEnding +
'                                        {' + LineEnding +
'                                             if (!f)' + LineEnding +
'                                             {' + LineEnding +
'                                                  out += ''<div class="tipue_search_related">'' + tipuesearch_string_10 + '': '';' + LineEnding +
'                                             }' + LineEnding +
'                                             if (show_replace)' + LineEnding +
'                                             {' + LineEnding +
'                                                  d_o = d;' + LineEnding +
'                                             }' + LineEnding +
'                                             ' + LineEnding +
'                                             if (tipuesearch_related.Related[i].include)' + LineEnding +
'                                             {' + LineEnding +
'                                                  var r_d = d_o + '' '' + tipuesearch_related.Related[i].related;' + LineEnding +
'                                             }' + LineEnding +
'                                             else' + LineEnding +
'                                             {' + LineEnding +
'                                                  var r_d = tipuesearch_related.Related[i].related;' + LineEnding +
'                                             }                                             ' + LineEnding +
'                                             ' + LineEnding +
'                                             ront += ''<a class="tipue_search_related_btn" id="'' + r_d + ''">'' + tipuesearch_related.Related[i].related + ''</a>, '';' + LineEnding +
'                                             f++;         ' + LineEnding +
'                                        }' + LineEnding +
'                                   }' + LineEnding +
'                                   if (f)' + LineEnding +
'                                   {' + LineEnding +
'                                        ront = ront.slice(0, -2);' + LineEnding +
'                                        ront += ''.</div>'';' + LineEnding +
'                                        out += ront;' + LineEnding +
'                                   }   ' + LineEnding +
'                              }' + LineEnding +
'' + LineEnding +
'                              if (show_replace)' + LineEnding +
'                              {' + LineEnding +
'                                   out += ''<div id="tipue_search_replace">'' + tipuesearch_string_2 + '' '' + d + ''. '' + tipuesearch_string_3 + '' <a id="tipue_search_replaced">'' + d_r + ''</a></div>'';' + LineEnding +
'                              }' + LineEnding +
'                              ' + LineEnding +
'                              found.sort(function(a, b) { return b.score - a.score } );' + LineEnding +
'                              ' + LineEnding +
'                              var l_o = 0;' + LineEnding +
'                              ' + LineEnding +
'                              if (set.imageZoom)' + LineEnding +
'                              {' + LineEnding +
'                                   out += ''<div id="tipue_search_image_modal"><div class="tipue_search_image_close">&#10005;</div><div class="tipue_search_image_block"><a id="tipue_search_zoom_url"><img id="tipue_search_zoom_img"></a><div id="tipue_search' + 
'_zoom_text"></div></div></div>'';    ' + 
LineEnding +
'                              }' + LineEnding +
'                              ' + LineEnding +
'                              for (var i = 0; i < found.length; i++)' + LineEnding +
'                              {' + LineEnding +
'                                   if (l_o >= start && l_o < set.show + start)' + LineEnding +
'                                   {' + LineEnding +
'                                        out += ''<div class="tipue_search_result">'';' + LineEnding +
'                                                                           ' + LineEnding +
'                                        out += ''<div class="tipue_search_content_title"><a href="'' + found[i].url + ''"'' + tipue_search_w + ''>'' +  found[i].title + ''</a></div>'';' + LineEnding +
' ' + LineEnding +
'                                        if (set.debug)' + LineEnding +
'                                        {                                             ' + LineEnding +
'                                             out += ''<div class="tipue_search_content_debug">Score: '' + found[i].score + ''</div>'';' + LineEnding +
'                                        }' + LineEnding +
'                                        ' + LineEnding +
'                                        if (set.showURL)' + LineEnding +
'                                        {' + LineEnding +
'                                             var s_u = found[i].url.toLowerCase();' + LineEnding +
'                                             if (s_u.indexOf(''http://'') == 0)' + LineEnding +
'                                             {' + LineEnding +
'                                                  s_u = s_u.slice(7);' + LineEnding +
'                                             }                                             ' + LineEnding +
'                                             out += ''<div class="tipue_search_content_url"><a href="'' + found[i].url + ''"'' + tipue_search_w + ''>'' + s_u + ''</a></div>'';' + LineEnding +
'                                        }' + LineEnding +
'                                        ' + LineEnding +
'                                        if (found[i].img)' + LineEnding +
'                                        {' + LineEnding +
'                                             if (set.imageZoom)' + LineEnding +
'                                             {' + LineEnding +
'                                                  out += ''<div class="tipue_search_image"><img class="tipue_search_img tipue_search_image_zoom" src="'' + found[i].img + ''" alt="'' + found[i].title + ''" data-url="'' + found[i].url + ''"></div>'';     ' + LineEnding +
'                                             }' + LineEnding +
'                                             else' + LineEnding +
'                                             {' + LineEnding +
'                                                  out += ''<div class="tipue_search_image"><a href="'' + found[i].url + ''"'' + tipue_search_w + ''><img class="tipue_search_img" src="'' + found[i].img + ''" alt="'' + found[i].title + ''"></a></div>'';' + LineEnding +
'                                             }' + LineEnding +
'                                        }' + LineEnding +
'                                        ' + LineEnding +
'                                        if (found[i].desc)' + LineEnding +
'                                        {                                        ' + LineEnding +
'                                             var t = found[i].desc;' + LineEnding +
'' + LineEnding +
'                                             if (set.showContext)' + LineEnding +
'                                             {' + LineEnding +
'                                                  d_w = d.split('' '');' + LineEnding +
'                                                  var s_1 = found[i].desc.toLowerCase().indexOf(d_w[0]);' + LineEnding +
'                                                  if (s_1 > set.contextStart)' + LineEnding +
'                                                  {' + LineEnding +
'                                                       var t_1 = t.substr(s_1 - set.contextBuffer);' + LineEnding +
'                                                       var s_2 = t_1.indexOf('' '');' + LineEnding +
'                                                       t_1 = t.substr(s_1 - set.contextBuffer + s_2);' + LineEnding +
'                                                       t_1 = $.trim(t_1);' + LineEnding +
'                                                       ' + LineEnding +
'                                                       if (t_1.length > set.contextLength)' + LineEnding +
'                                                       {                                                      ' + LineEnding +
'                                                            t = ''... '' + t_1;' + LineEnding +
'                                                       }' + LineEnding +
'                                                  }   ' + LineEnding +
'                                             }' + LineEnding +
'                                             ' + LineEnding +
'                                             if (standard)' + LineEnding +
'                                             {' + LineEnding +
'                                                  d_w = d.split('' '');' + LineEnding +
'                                                  for (var f = 0; f < d_w.length; f++)' + LineEnding +
'                                                  {' + LineEnding +
'                                                       if (set.highlightTerms)' + LineEnding +
'                                                       {' + LineEnding +
'                                                            var patr = new RegExp(''('' + d_w[f] + '')'', ''gi'');' + LineEnding +
'                                                            t = t.replace(patr, "<h0011>$1<h0012>");' + LineEnding +
'                                                       }' + LineEnding +
'                                                  }' + LineEnding +
'                                             }' + LineEnding +
'                                             else if (set.highlightTerms)' + LineEnding +
'                                             {' + LineEnding +
'                                                  var patr = new RegExp(''('' + d + '')'', ''gi'');' + LineEnding +
'                                                  t = t.replace(patr, "<span class=\"tipue_search_content_bold\">$1</span>");' + LineEnding +
'                                             }' + LineEnding +
'                                                                                  ' + LineEnding +
'                                             var t_d = '''';' + LineEnding +
'                                             var t_w = t.split('' '');' + LineEnding +
'                                             if (t_w.length < set.descriptiveWords)' + LineEnding +
'                                             {' + LineEnding +
'                                                  t_d = t;' + LineEnding +
'                                             }' + LineEnding +
'                                             else' + LineEnding +
'                                             {' + LineEnding +
'                                                  for (var f = 0; f < set.descriptiveWords; f++)' + LineEnding +
'                                                  {' + LineEnding +
'                                                       t_d += t_w[f] + '' ''; 	' + LineEnding +
'                                                  }' + LineEnding +
'                                             }' + LineEnding +
'                                             t_d = $.trim(t_d);' + LineEnding +
'                                             if (t_d.charAt(t_d.length - 1) != ''.'')' + LineEnding +
'                                             {' + LineEnding +
'                                                  t_d += '' ...'';' + LineEnding +
'                                             }' + LineEnding +
'                                             ' + LineEnding +
'                                             t_d = t_d.replace(/h0011/g, ''span class=\"tipue_search_content_bold\"'');' + LineEnding +
'                                             t_d = t_d.replace(/h0012/g, ''/span'');' + LineEnding +
'                                             ' + LineEnding +
'                                             out += ''<div class="tipue_search_content_text">'' + t_d + ''</div>'';' + LineEnding +
'                                        }' + LineEnding +
'                                        ' + LineEnding +
'                                        if (found[i].note)' + LineEnding +
'                                        {' + LineEnding +
'                                             out += ''<div class="tipue_search_note">'' + found[i].note + ''</div>'';    ' + LineEnding +
'                                        }                                       ' + LineEnding +
'                                        ' + LineEnding +
'                                        out += ''</div>'';' + LineEnding +
'                                   }' + LineEnding +
'                                   l_o++;     ' + LineEnding +
'                              }                              ' + LineEnding +
'' + LineEnding +
'                              if (c > set.show)' + LineEnding +
'                              {' + LineEnding +
'                                   var pages = Math.ceil(c / set.show);' + LineEnding +
'                                   var page = (start / set.show);' + LineEnding +
'                                   if (set.footerPages < 3)' + LineEnding +
'                                   {' + LineEnding +
'                                        set.footerPages = 3;' + LineEnding +
'                                   }' + LineEnding +
'                                   ' + LineEnding +
'                                   out += ''<div id="tipue_search_foot"><ul id="tipue_search_foot_boxes">'';' + LineEnding +
'                                   ' + LineEnding +
'                                   if (start > 0)' + LineEnding +
'                                   {' + LineEnding +
'                                       out += ''<li role="navigation"><a class="tipue_search_foot_box" accesskey="b" id="'' + (start - set.show) + ''_'' + replace + ''">'' + tipuesearch_string_6 + ''</a></li>''; ' + LineEnding +
'                                   }' + LineEnding +
'                                                       ' + LineEnding +
'                                   if (page <= 2)' + LineEnding +
'                                   {' + LineEnding +
'                                        var p_b = pages;' + LineEnding +
'                                        if (pages > set.footerPages)' + LineEnding +
'                                        {' + LineEnding +
'                                             p_b = set.footerPages;' + LineEnding +
'                                        }                    ' + LineEnding +
'                                        for (var f = 0; f < p_b; f++)' + LineEnding +
'                                        {' + LineEnding +
'                                             if (f == page)' + LineEnding +
'                                             {' + LineEnding +
'                                                  out += ''<li class="current" role="navigation">'' + (f + 1) + ''</li>'';' + LineEnding +
'                                             }' + LineEnding +
'                                             else' + LineEnding +
'                                             {' + LineEnding +
'                                                  out += ''<li role="navigation"><a class="tipue_search_foot_box" id="'' + (f * set.show) + ''_'' + replace + ''">'' + (f + 1) + ''</a></li>'';' + LineEnding +
'                                             }' + LineEnding +
'                                        }' + LineEnding +
'                                   }' + LineEnding +
'                                   else' + LineEnding +
'                                   {' + LineEnding +
'                                        var p_b = page + set.footerPages - 1;' + LineEnding +
'                                        if (p_b > pages)' + LineEnding +
'                                        {' + LineEnding +
'                                             p_b = pages; ' + LineEnding +
'                                        }' + LineEnding +
'                                        for (var f = page - 1; f < p_b; f++)' + LineEnding +
'                                        {' + LineEnding +
'                                             if (f == page)' + LineEnding +
'                                             {' + LineEnding +
'                                                  out += ''<li class="current" role="navigation">'' + (f + 1) + ''</li>'';' + LineEnding +
'                                             }' + LineEnding +
'                                             else' + LineEnding +
'                                             {' + LineEnding +
'                                                  out += ''<li role="navigation"><a class="tipue_search_foot_box" id="'' + (f * set.show) + ''_'' + replace + ''">'' + (f + 1) + ''</a></li>'';' + LineEnding +
'                                             }' + LineEnding +
'                                        }' + LineEnding +
'                                   }                         ' + LineEnding +
'                                                      ' + LineEnding +
'                                   if (page + 1 != pages)' + LineEnding +
'                                   {' + LineEnding +
'                                       out += ''<li role="navigation"><a class="tipue_search_foot_box" accesskey="m" id="'' + (start + set.show) + ''_'' + replace + ''">'' + tipuesearch_string_7 + ''</a></li>''; ' + LineEnding +
'                                   }                    ' + LineEnding +
'                                   ' + LineEnding +
'                                   out += ''</ul></div>'';' + LineEnding +
'                              }' + LineEnding +
'' + LineEnding +
'                         }' + LineEnding +
'                         else' + LineEnding +
'                         {' + LineEnding +
'                              out += ''<div id="tipue_search_error">'' + tipuesearch_string_8 + ''</div>''; ' + LineEnding +
'                         }' + LineEnding +
'                    }' + LineEnding +
'                    else' + LineEnding +
'                    {' + LineEnding +
'                         if (show_stop)' + LineEnding +
'                         {' + LineEnding +
'                              out += ''<div id="tipue_search_error">'' + tipuesearch_string_8 + '' '' + tipuesearch_string_9 + ''</div>'';     ' + LineEnding +
'                         }' + LineEnding +
'                         else' + LineEnding +
'                         {' + LineEnding +
'                              if (set.minimumLength == 1)' + LineEnding +
'                              {' + LineEnding +
'                                   out += ''<div id="tipue_search_error">'' + tipuesearch_string_11 + ''</div>'';' + LineEnding +
'                              }' + LineEnding +
'                              else' + LineEnding +
'                              {' + LineEnding +
'                                   out += ''<div id="tipue_search_error">'' + tipuesearch_string_12 + '' '' + set.minimumLength + '' '' + tipuesearch_string_13 + ''</div>'';' + LineEnding +
'                              }' + LineEnding +
'                         }' + LineEnding +
'                    }                ' + LineEnding +
'                    ' + LineEnding +
'                    $(''#tipue_search_content'').hide().html(out).slideDown(200);' + LineEnding +
'                    ' + LineEnding +
'                    $(''#tipue_search_replaced'').click(function()' + LineEnding +
'                    {' + LineEnding +
'                         getTipueSearch(0, false);' + LineEnding +
'                    });' + LineEnding +
'                    ' + LineEnding +
'                    $(''.tipue_search_related_btn'').click(function()' + LineEnding +
'                    {' + LineEnding +
'                         $(''#tipue_search_input'').val($(this).attr(''id''));' + LineEnding +
'                         getTipueSearch(0, true);' + LineEnding +
'                    });' + LineEnding +
'                    ' + LineEnding +
'                    $(''.tipue_search_image_zoom'').click(function()' + LineEnding +
'                    {' + LineEnding +
'                         $(''#tipue_search_image_modal'').fadeIn(300);' + LineEnding +
'                         $(''#tipue_search_zoom_img'').attr(''src'', this.src);' + LineEnding +
'                         ' + LineEnding +
'                         var z_u = $(this).attr(''data-url'');' + LineEnding +
'                         $(''#tipue_search_zoom_url'').attr(''href'', z_u);' + LineEnding +
'                         ' + LineEnding +
'                         var z_o = this.alt + ''<div class="tipue_search_zoom_options"><a href="'' + this.src + ''" target="_blank">'' + tipuesearch_string_15 + ''</a>&nbsp; <a href="'' + z_u + ''">'' + tipuesearch_string_16 + ''</a></div>'';' + LineEnding +
'                         ' + LineEnding +
'                         $(''#tipue_search_zoom_text'').html(z_o);' + LineEnding +
'                    });' + LineEnding +
'                    ' + LineEnding +
'                    $(''.tipue_search_image_close'').click(function()' + LineEnding +
'                    {' + LineEnding +
'                         $(''#tipue_search_image_modal'').fadeOut(300);' + LineEnding +
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
''
