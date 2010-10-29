# Generates all *.inc files from text/binary files in this directory.

ALL_OUTPUT := pasdoc.css.inc

.PHONY: all clean

all: $(ALL_OUTPUT)

pasdoc.css.inc: pasdoc.css
	../tools/file_to_pascal_string $< $@

clean:
	rm -f $(ALL_OUTPUT)
