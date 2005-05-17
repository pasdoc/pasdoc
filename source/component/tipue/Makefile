# Generates all *.inc files from text/binary files in this directory.

ALL_OUTPUT := tip_form.js.inc tip_search.js.inc _tipue_results.html.inc tipue_b1.png.inc

.PHONY: all clean

all: $(ALL_OUTPUT)

tipue_b1.png.inc: tipue_b1.png
	../../tools/file_to_pascal_data $< $@

%.inc: %
	../../tools/file_to_pascal_string $< $@

clean:
	rm -f $(ALL_OUTPUT)