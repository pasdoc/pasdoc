# Generates all *.inc files from text/binary files in this directory.

ALL_OUTPUT := bootstrap.min.css.inc bootstrap.bundle.min.js.inc

.PHONY: all clean

all: $(ALL_OUTPUT)

%.inc: %
	../../../bin/file_to_pascal_string $< $@

clean:
	rm -f $(ALL_OUTPUT)
