# This Makefile creates PasDoc generated documentation for PasDoc itself.
# Documentation is created in subdirectories named after pasdoc's --format
# names (html/, latex/ etc.)
#
# Includes a Classes and Uses hierarchy graph (requires GraphViz).
# TODO: for now this GraphViz graph's are only included in Html output.
#
# TODO: It would be nice if this Makefile would share some source
# with Makefile in ../../tests/Makefile
#
# Targets:
# - html latex pdf dvi ps html-latex
#   Make documentation in requested format
# - clean
#   Deletes generated documentation

# Name of pasdoc binary.
# Remember you can override it at make's command-line, like `make PASDOC=pasdoc'
PASDOC=../console/PasDoc_Console

# These options will be passed to every pasdoc invocation,
# i.e. for every output format.
PASDOC_OPTIONS=--include ../component \
  ../component/*.pas ../OptionParser/*.pas \
  --title "PasDoc's autodoc" --write-uses-list

.PHONY: html latex pdf dvi ps html-latex clean

html:
	mkdir -p html/
	$(PASDOC) $(PASDOC_OPTIONS) \
	  --graphviz-classes --graphviz-uses \
	  --link-gv-classes jpg --link-gv-uses jpg \
	  --output=html/
	dot -Tjpg -ohtml/GVClasses.jpg html/GVClasses.dot
	dot -Tjpg -ohtml/GVUses.jpg html/GVUses.dot

# Just a shortcuts
latex: latex/docs.tex
pdf: latex/docs.pdf
dvi: latex/docs.dvi
ps: latex/docs.ps
html-latex: latex/docs.html

latex/docs.tex:
	mkdir -p latex/
	$(PASDOC) $(PASDOC_OPTIONS) \
	  --format=latex --output=latex/

# (Yes, I must do `cd latex' before calling pdflatex, I can't simply call
# `pdflatex latex/docs.tex' because then many output files of pdflatex
# would be placed in current dir)
latex/docs.pdf: latex/docs.tex
	cd latex; pdflatex --file-line-error-style -interaction=nonstopmode docs.tex
	cd latex; pdflatex --file-line-error-style -interaction=nonstopmode docs.tex

latex/docs.dvi: latex/docs.tex
	cd latex; latex --file-line-error-style -interaction=nonstopmode docs.tex
	cd latex; latex --file-line-error-style -interaction=nonstopmode docs.tex

latex/docs.ps: latex/docs.dvi
	cd latex; dvips docs.dvi -o docs.ps

# When working htlatex overrides latex/docs.dvi with a version
# that is not really a useful to browse (it's only for htlatex internals).
# That's why I `rm' it afterwards.
latex/docs.html: latex/docs.tex
	cd latex; htlatex docs.tex
	cd latex; htlatex docs.tex
	rm latex/docs.dvi

clean:
	rm -Rf html/ latex/ latex2rtf/ htmlhelp/
