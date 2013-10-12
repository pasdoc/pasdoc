# Generates all *.inc files from text/binary files in this directory.

ALL_OUTPUT := tipuesearch.css.inc tipuesearch.js.inc \
  tipuesearch_set.js.inc _tipue_results.html.inc \
  search.png.inc loader.gif.inc jquery.min.js.inc

.PHONY: all clean

all: $(ALL_OUTPUT)

_tipue_results.html.inc: _tipue_results.html
	../../tools/file_to_pascal_string $< $@

# We use file_to_pascal_data even for text files, to workaround
# Delphi problems with long strings, see
# https://sourceforge.net/p/pasdoc/bugs/82/

%.inc: %
	../../tools/file_to_pascal_data $< $@

clean:
	rm -f $(ALL_OUTPUT)