# See README for docs how to use this Makefile.

# Making docs ----------------------------------------

.PHONY: html pdf latex dvi htmlhelp simplexml

html:
	scripts/mk_tests.sh html

dvi: latex/docs.dvi

pdf: latex/docs.pdf

latex: latex/docs.tex

latex2rtf: latex2rtf/docs.tex

htmlhelp:
	scripts/mk_tests.sh htmlhelp

simplexml:
	scripts/mk_tests.sh simplexml

latex/docs.tex:
	scripts/mk_tests.sh latex

# (Yes, I must do `cd latex' before calling pdflatex, I can't simply call
# `pdflatex latex/docs.tex' because then many output files of pdflatex
# would be placed in current dir)
latex/docs.pdf: latex/docs.tex
	cd latex; pdflatex --file-line-error-style -interaction=nonstopmode docs.tex
	cd latex; pdflatex --file-line-error-style -interaction=nonstopmode docs.tex

latex/docs.dvi: latex/docs.tex
	cd latex; latex --file-line-error-style -interaction=nonstopmode docs.tex
	cd latex; latex --file-line-error-style -interaction=nonstopmode docs.tex

latex2rtf/docs.tex:
	scripts/mk_tests.sh latex2rtf

# Other targets ----------------------------------------

validate_html:
	scripts/validate_html.sh

validate_simplexml:
	scripts/validate_simplexml.sh

clean:
	rm -Rf html/ latex/ htmlhelp/ latex2rtf/ simplexml/ \
	  scripts/check_cache_tmp/ \
	  scripts/upload_correct_tests_output_tmp/

# eof ----------------------------------------