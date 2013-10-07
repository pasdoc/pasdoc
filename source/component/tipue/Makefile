# Generates all *.inc files from text/binary files in this directory.

ALL_OUTPUT := tipuesearch.css.inc tipuesearch.js.inc \
  tipuesearch_set.js.inc _tipue_results.html.inc \
  search.png.inc loader.gif.inc jquery.min.js.inc

.PHONY: all clean

all: $(ALL_OUTPUT)

search.png.inc: search.png
	../../tools/file_to_pascal_data $< $@

loader.gif.inc: loader.gif
	../../tools/file_to_pascal_data $< $@

%.inc: %
	../../tools/file_to_pascal_string $< $@

clean:
	rm -f $(ALL_OUTPUT)