# TODO: I'll do this Makefile tomorrow, and will add more comments to README.
# Today I'm just committing my work: README and some starting test units,
# contents of this Makefile are now only some trivial skeleton.

.PHONY: html

# Pascal files to process.
# Remember that you can override it at command-line,
# e.g. run `make PAS=error_line_number.pas'
#PAS=*.pas
#PAS=ok_*.pas
#PAS=warning_*.pas
#PAS=error_*.pas
#PAS=todo/ok_*.pas
PAS=todo/warning_*.pas
#PAS=todo/error_*.pas

html:
	mkdir -p html/
	pasdoc --format html --output=html/ $(PAS)

clean:
	rm -Rf html/