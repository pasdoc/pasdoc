# See README for docs how to use this Makefile.

# TODO: all pasdoc's output formats should be added here,
# including latex2rtf and htmlhelp.

# Making docs ----------------------------------------

# Pascal files to process.
# Remember that you can override it at command-line,
# e.g. run `make PAS=error_line_number.pas'
PAS=*.pas

# These options will be passed to every pasdoc invocation,
# i.e. for every output format.
PASDOC_OPTIONS=--exclude-generator $(PAS)

.PHONY: html pdf latex dvi

html:
	mkdir -p html/
	pasdoc --format html --output=html/ $(PASDOC_OPTIONS)

dvi: latex/docs.dvi

pdf: latex/docs.pdf

latex: latex/docs.tex

# (Yes, I must do `cd latex' before calling pdflatex, I can't simply call
# `pdflatex latex/docs.tex' because then many output files of pdflatex
# would be placed in current dir)
latex/docs.pdf: latex/docs.tex
	cd latex; pdflatex --file-line-error-style -interaction=nonstopmode docs.tex
	cd latex; pdflatex --file-line-error-style -interaction=nonstopmode docs.tex

latex/docs.dvi: latex/docs.tex
	cd latex; latex --file-line-error-style -interaction=nonstopmode docs.tex
	cd latex; latex --file-line-error-style -interaction=nonstopmode docs.tex

latex/docs.tex:
	mkdir -p latex/
	pasdoc --format latex --output=latex/ $(PASDOC_OPTIONS)

# Other targets ----------------------------------------

validate_html:
	cd html/; ../scripts/validate_html.sh

clean:
	rm -Rf html/ latex/ scripts/check_cache_tmp/ \
	  scripts/upload_correct_tests_output_tmp/

# eof ----------------------------------------