{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "tip_search.js" }
'// Tipue 1.63 (modified for pasdoc)' + LineEnding + 
'' + LineEnding + 
'' + LineEnding + 
'// ---------- script properties ----------' + LineEnding + 
'' + LineEnding + 
'' + LineEnding + 
'var results_location = "_tipue_results.html";' + LineEnding + 
'var return_results = 10;' + LineEnding + 
'var include_num = 1;' + LineEnding + 
'var bold_query = 0;' + LineEnding + 
'var include_url = 0;' + LineEnding + 
'' + LineEnding + 
'' + LineEnding + 
'// ---------- end of script properties ----------' + LineEnding + 
'' + LineEnding + 
'' + LineEnding + 
'var cookies = document.cookie;' + LineEnding + 
'var tp = cookies.indexOf(''tid='');' + LineEnding + 
'var pn = cookies.indexOf(''tin='');' + LineEnding + 
'' + LineEnding + 
'var lnf = ''Your search did not match any documents.<p>Make sure all keywords are spelled correctly.<br>Try different or more general keywords.'';' + LineEnding + 
'var lp = ''Previous '';' + LineEnding + 
'var ln = ''Next '';' + LineEnding + 
'' + LineEnding + 
'if (tp != -1) {' + LineEnding + 
'	var st = tp + 4;' + LineEnding + 
'	var en = cookies.indexOf('';'', st);' + LineEnding + 
'	if (en == -1) {' + LineEnding + 
'		en = cookies.length;' + LineEnding + 
'	}' + LineEnding + 
'	var dit = cookies.substring(st, en);' + LineEnding + 
'	dit = unescape(dit);' + LineEnding + 
'}' + LineEnding + 
'if (pn != -1) {' + LineEnding + 
'	var st = pn + 4;' + LineEnding + 
'	var en = cookies.indexOf('';'', st);' + LineEnding + 
'	if (en == -1) {' + LineEnding + 
'		en = cookies.length;' + LineEnding + 
'	}' + LineEnding + 
'	var tn = cookies.substring(st, en);' + LineEnding + 
'}' + LineEnding + 
'' + LineEnding + 
'var od = dit;' + LineEnding + 
'var nr = return_results;' + LineEnding + 
'tn = parseInt(tn);' + LineEnding + 
'var nb = tn + nr;' + LineEnding + 
'var nc = 0;' + LineEnding + 
'var nd = 0;' + LineEnding + 
'var tr = new Array();' + LineEnding + 
'var rt = new Array();' + LineEnding + 
'var co = 0;' + LineEnding + 
'var tm = 0;' + LineEnding + 
'' + LineEnding + 
'if (dit.charAt(0) == ''"'' && dit.charAt(dit.length - 1) == ''"'') {' + LineEnding + 
'	tm = 1;' + LineEnding + 
'}' + LineEnding + 
'var rn = dit.search(/ or /i);' + LineEnding + 
'if (rn >= 0) {' + LineEnding + 
'	tm = 2;' + LineEnding + 
'}' + LineEnding + 
'rn = dit.search(/-/i);' + LineEnding + 
'if (rn >= 0 && tm != 1) {' + LineEnding + 
'	rn = dit.search(/ /i);' + LineEnding + 
'	if (rn != 0) {' + LineEnding + 
'		dit = dit.replace(/-/gi, '' -'');' + LineEnding + 
'	}' + LineEnding + 
'}' + LineEnding + 
'rn = dit.search(/ not /i);' + LineEnding + 
'if (rn >= 0 && tm != 1) {' + LineEnding + 
'	dit = dit.replace(/ not /gi, '' -'');' + LineEnding + 
'}' + LineEnding + 
'rn = dit.search(/\+/i);' + LineEnding + 
'if (rn >= 0) {' + LineEnding + 
'	rn = dit.search(/ /i);' + LineEnding + 
'	if (rn != 0) {' + LineEnding + 
'		dit = dit.replace(/\+/gi, '' +'');' + LineEnding + 
'	}' + LineEnding + 
'}' + LineEnding + 
'' + LineEnding + 
'if (tm == 0) {' + LineEnding + 
'	var woin = new Array();' + LineEnding + 
'	dit = dit.replace(/ and /gi, '' '');' + LineEnding + 
'	var wt = dit.split('' '');' + LineEnding + 
'	for (var a = 0; a < wt.length; a++) {' + LineEnding + 
'		woin[a] = 0;' + LineEnding + 
'		if (wt[a].charAt(0) == ''-'') {' + LineEnding + 
'			woin[a] = 1;' + LineEnding + 
'		}' + LineEnding + 
'	}' + LineEnding + 
'	for (var a = 0; a < wt.length; a++) {' + LineEnding + 
'		wt[a] = wt[a].replace(/^\-|^\+/gi, '''');' + LineEnding + 
'	}' + LineEnding + 
'	a = 0;' + LineEnding + 
'	for (var c = 0; c < s.length; c++) {' + LineEnding + 
'		var es = s[c].split(''^'');' + LineEnding + 
'		var rk = 100;' + LineEnding + 
'		if (es[5] == null) {' + LineEnding + 
'			es[5] = ''0'';' + LineEnding + 
'		}' + LineEnding + 
'		if (parseInt(es[5]) > 10) {' + LineEnding + 
'			es[5] = ''10'';' + LineEnding + 
'		}	' + LineEnding + 
'		var pa = 0;' + LineEnding + 
'		var nh = 0;' + LineEnding + 
'		for (var i = 0; i < woin.length; i++) {' + LineEnding + 
'			if (woin[i] == 0) {' + LineEnding + 
'				nh++;' + LineEnding + 
'				var nt = 0;' + LineEnding + 
'				var pat = new RegExp(wt[i], ''i'');' + LineEnding + 
'				rn = es[0].search(pat);' + LineEnding + 
'				if (rn >= 0) {' + LineEnding + 
'					rk = rk - 11;' + LineEnding + 
'					rk = rk - parseInt(es[5]);					' + LineEnding + 
'					nt = 1;' + LineEnding + 
'				}' + LineEnding + 
'				rn = es[2].search(pat);' + LineEnding + 
'				if (rn >= 0) {' + LineEnding + 
'					rk = rk - 11;' + LineEnding + 
'					rk = rk - parseInt(es[5]);					' + LineEnding + 
'					nt = 1;' + LineEnding + 
'				}' + LineEnding + 
'				rn = es[3].search(pat);' + LineEnding + 
'				if (rn >= 0) {' + LineEnding + 
'					rk = rk - 11;' + LineEnding + 
'					rk = rk - parseInt(es[5]);					' + LineEnding + 
'					nt = 1;' + LineEnding + 
'				}' + LineEnding + 
'				if (nt == 1) {' + LineEnding + 
'					pa++;' + LineEnding + 
'				}' + LineEnding + 
'			}' + LineEnding + 
'			if (woin[i] == 1) {' + LineEnding + 
'				var pat = new RegExp(wt[i], ''i'');' + LineEnding + 
'				rn = es[0].search(pat);' + LineEnding + 
'				if (rn >= 0) {' + LineEnding + 
'					pa = 0;' + LineEnding + 
'				}' + LineEnding + 
'				rn = es[2].search(pat);' + LineEnding + 
'				if (rn >= 0) {' + LineEnding + 
'					pa = 0;' + LineEnding + 
'				}' + LineEnding + 
'				rn = es[3].search(pat);' + LineEnding + 
'				if (rn >= 0) {' + LineEnding + 
'					pa = 0;' + LineEnding + 
'				}' + LineEnding + 
'			}' + LineEnding + 
'		}' + LineEnding + 
'		if (pa == nh) {' + LineEnding + 
'			tr[a] = rk + ''^'' + s[c];' + LineEnding + 
'			a++;' + LineEnding + 
'		}' + LineEnding + 
'	}' + LineEnding + 
'	tr.sort();' + LineEnding + 
'	co = a;' + LineEnding + 
'}' + LineEnding + 
'' + LineEnding + 
'if (tm == 1) {' + LineEnding + 
'	dit = dit.replace(/"/gi, '''');' + LineEnding + 
'	var a = 0;' + LineEnding + 
'	var pat = new RegExp(dit, ''i'');' + LineEnding + 
'	for (var c = 0; c < s.length; c++) {' + LineEnding + 
'		var es = s[c].split(''^'');' + LineEnding + 
'		var rk = 100;' + LineEnding + 
'		if (es[5] == null) {' + LineEnding + 
'			es[5] = ''0'';' + LineEnding + 
'		}' + LineEnding + 
'		if (parseInt(es[5]) > 10) {' + LineEnding + 
'			es[5] = ''10'';' + LineEnding + 
'		}' + LineEnding + 
'		rn = es[0].search(pat);' + LineEnding + 
'		if (rn >= 0) {' + LineEnding + 
'			rk = rk - 11;' + LineEnding + 
'			rk = rk - parseInt(es[5]);' + LineEnding + 
'		}' + LineEnding + 
'		rn = es[2].search(pat);' + LineEnding + 
'		if (rn >= 0) {' + LineEnding + 
'			rk = rk - 11;' + LineEnding + 
'			rk = rk - parseInt(es[5]);' + LineEnding + 
'		}' + LineEnding + 
'		rn = es[3].search(pat);' + LineEnding + 
'		if (rn >= 0) {' + LineEnding + 
'			rk = rk - 11;' + LineEnding + 
'			rk = rk - parseInt(es[5]);' + LineEnding + 
'		}' + LineEnding + 
'		if (rk < 100) {' + LineEnding + 
'			tr[a] = rk + ''^'' + s[c];' + LineEnding + 
'			a++;		' + LineEnding + 
'		}' + LineEnding + 
'	}' + LineEnding + 
'	tr.sort();' + LineEnding + 
'	co = a;' + LineEnding + 
'}' + LineEnding + 
'' + LineEnding + 
'if (tm == 2) {' + LineEnding + 
'	dit = dit.replace(/ or /gi, '' '');' + LineEnding + 
'	var wt = dit.split('' '');' + LineEnding + 
'	var a = 0;' + LineEnding + 
'	for (var i = 0; i < wt.length; i++) {' + LineEnding + 
'		var pat = new RegExp(wt[i], ''i'');' + LineEnding + 
'		for (var c = 0; c < s.length; c++) {' + LineEnding + 
'			var es = s[c].split(''^'');' + LineEnding + 
'			var rk = 100;' + LineEnding + 
'			if (es[5] == null) {' + LineEnding + 
'				es[5] = ''0'';' + LineEnding + 
'			}' + LineEnding + 
'			if (parseInt(es[5]) > 10) {' + LineEnding + 
'				es[5] = ''10'';' + LineEnding + 
'			}' + LineEnding + 
'			var pa = 0;' + LineEnding + 
'			var rn = es[0].search(pat);' + LineEnding + 
'			if (rn >= 0) {' + LineEnding + 
'				rk = rk - 11;' + LineEnding + 
'				rk = rk - parseInt(es[5]);		' + LineEnding + 
'				if (rn >= 0) {' + LineEnding + 
'					for (var e = 0; e < rt.length; e++) {' + LineEnding + 
'						if (s[c] == rt[e]) {' + LineEnding + 
'							pa = 1;' + LineEnding + 
'						}' + LineEnding + 
'					}' + LineEnding + 
'				}' + LineEnding + 
'			}' + LineEnding + 
'			rn = es[2].search(pat);' + LineEnding + 
'			if (rn >= 0) {' + LineEnding + 
'				rk = rk - 11;' + LineEnding + 
'				rk = rk - parseInt(es[5]);		' + LineEnding + 
'				if (rn >= 0) {' + LineEnding + 
'					for (var e = 0; e < rt.length; e++) {' + LineEnding + 
'						if (s[c] == rt[e]) {' + LineEnding + 
'							pa = 1;' + LineEnding + 
'						}' + LineEnding + 
'					}' + LineEnding + 
'				}' + LineEnding + 
'			}' + LineEnding + 
'			var rn = es[3].search(pat);' + LineEnding + 
'			if (rn >= 0) {' + LineEnding + 
'				rk = rk - 11;' + LineEnding + 
'				rk = rk - parseInt(es[5]);		' + LineEnding + 
'				if (rn >= 0) {' + LineEnding + 
'					for (var e = 0; e < rt.length; e++) {' + LineEnding + 
'						if (s[c] == rt[e]) {' + LineEnding + 
'							pa = 1;' + LineEnding + 
'						}' + LineEnding + 
'					}' + LineEnding + 
'				}' + LineEnding + 
'			}' + LineEnding + 
'			if (rk < 100 && pa == 0) {' + LineEnding + 
'				rt[a] = s[c];' + LineEnding + 
'				tr[a] = rk + ''^'' + s[c];' + LineEnding + 
'				a++;' + LineEnding + 
'				co++;' + LineEnding + 
'			}' + LineEnding + 
'		}' + LineEnding + 
'	}' + LineEnding + 
'	tr.sort();' + LineEnding + 
'}' + LineEnding + 
'' + LineEnding + 
'function write_cookie(nw) {' + LineEnding + 
'	document.cookie = ''tid='' + escape(od) + ''; path=/'';' + LineEnding + 
'	document.cookie = ''tin='' + nw + ''; path=/'';' + LineEnding + 
'}' + LineEnding + 
'' + LineEnding + 
'' + LineEnding + 
'// ---------- External references ----------' + LineEnding + 
'' + LineEnding + 
'' + LineEnding + 
'var tip_Num = co;' + LineEnding + 
'' + LineEnding + 
'function tip_query() {' + LineEnding + 
'	if (od != ''undefined'' && od != null) document.tip_Form.d.value = od;' + LineEnding + 
'}' + LineEnding + 
'' + LineEnding + 
'function tip_num() {' + LineEnding + 
'	document.write(co);' + LineEnding + 
'}' + LineEnding + 
'' + LineEnding + 
'function tip_out() {' + LineEnding + 
'	if (co == 0) {' + LineEnding + 
'		document.write(lnf);' + LineEnding + 
'		return;' + LineEnding + 
'	}' + LineEnding + 
'	if (tn + nr > co) {' + LineEnding + 
'		nd = co;	' + LineEnding + 
'	} else {' + LineEnding + 
'		nd = tn + nr;' + LineEnding + 
'	}' + LineEnding + 
'	for (var a = tn; a < nd; a++) {' + LineEnding + 
'		var os = tr[a].split(''^'');' + LineEnding + 
'		if (os[5] == null) {' + LineEnding + 
'			os[5] = ''0'';' + LineEnding + 
'		}' + LineEnding + 
'		if (bold_query == 1 && tm == 0) {' + LineEnding + 
'			for (var i = 0; i < wt.length; i++) {' + LineEnding + 
'				var lw = wt[i].length;' + LineEnding + 
'				var tw = new RegExp(wt[i], ''i'');' + LineEnding + 
'				rn = os[3].search(tw);' + LineEnding + 
'				if (rn >= 0) {' + LineEnding + 
'					var o1 = os[3].slice(0, rn);' + LineEnding + 
'					var o2 = os[3].slice(rn, rn + lw);' + LineEnding + 
'					var o3 = os[3].slice(rn + lw);' + LineEnding + 
'					os[3] = o1 + ''<b>'' + o2 + ''</b>'' + o3; ' + LineEnding + 
'				}' + LineEnding + 
'			}' + LineEnding + 
'		}' + LineEnding + 
'		if (bold_query == 1 && tm == 1) {' + LineEnding + 
'			var lw = dit.length;' + LineEnding + 
'			var tw = new RegExp(dit, ''i'');' + LineEnding + 
'			rn = os[3].search(tw);' + LineEnding + 
'			if (rn >= 0) {' + LineEnding + 
'				var o1 = os[3].slice(0, rn);' + LineEnding + 
'				var o2 = os[3].slice(rn, rn + lw);' + LineEnding + 
'				var o3 = os[3].slice(rn + lw);' + LineEnding + 
'				os[3] = o1 + ''<b>'' + o2 + ''</b>'' + o3;' + LineEnding + 
'			}' + LineEnding + 
'		}' + LineEnding + 
'		if (include_num == 1) {' + LineEnding + 
'			document.write(a + 1, ''. '');' + LineEnding + 
'		}' + LineEnding + 
'		if (os[5] == ''0'') {' + LineEnding + 
'			document.write(''<a href="'', os[2], ''">'', os[1], ''</a>'');' + LineEnding + 
'		}' + LineEnding + 
'		if (os[5] == ''1'') {' + LineEnding + 
'			document.write(''<a href="'', os[2], ''" target="_blank">'', os[1], ''</a>'');' + LineEnding + 
'		}' + LineEnding + 
'		if (os[5] != ''0'' && os[5] != ''1'') {' + LineEnding + 
'			document.write(''<a href="'', os[2], ''" target="'', os[5], ''">'', os[1], ''</a>'');' + LineEnding + 
'		}		' + LineEnding + 
'		if (os[3].length > 1) {' + LineEnding + 
'			document.write(''<br>'', os[3]);' + LineEnding + 
'		}' + LineEnding + 
'		if (include_url == 1) {' + LineEnding + 
'			if (os[5] == ''0'') {' + LineEnding + 
'				document.write(''<br><a href="'', os[2], ''">'', os[2], ''</a><p>'');' + LineEnding + 
'			}			' + LineEnding + 
'			if (os[5] == ''1'') {' + LineEnding + 
'				document.write(''<br><a href="'', os[2], ''" target="_blank">'', os[2], ''</a><p>'');' + LineEnding + 
'			}' + LineEnding + 
'			if (os[5] != ''0'' && os[5] != ''1'') {' + LineEnding + 
'				document.write(''<br><a href="'', os[2], ''" target="'', os[5], ''">'', os[2], ''</a><p>'');' + LineEnding + 
'			}' + LineEnding + 
'		} else {' + LineEnding + 
'			document.write(''<p>'');' + LineEnding + 
'		}' + LineEnding + 
'	}' + LineEnding + 
'	if (co > nr) {' + LineEnding + 
'		nc = co - nb;' + LineEnding + 
'		if (nc > nr) {' + LineEnding + 
'			nc = nr;' + LineEnding + 
'		}' + LineEnding + 
'		document.write(''<p>'');' + LineEnding + 
'	}' + LineEnding + 
'	if (tn > 1) {' + LineEnding + 
'		document.write(''<a href="'', results_location, ''" onclick="write_cookie('', tn - nr, '')">'', lp, nr, ''</a> &nbsp;'');' + LineEnding + 
'	}' + LineEnding + 
'	if (nc > 0) {' + LineEnding + 
'		document.write(''<a href="'', results_location, ''" onclick="write_cookie('', tn + nr, '')">'', ln, nc, ''</a>'');' + LineEnding + 
'	}' + LineEnding + 
'}' + LineEnding + 
''
