# Generates all *.inc files from text/binary files in this directory.
#
# We use file_to_pascal_data, not file_to_pascal_string for these files,
# as bootstrap.min.css has some Unicode characters that cause differences
# when being output back by Delphi with STRING_UNICODE with StringToFile .
# It's simplest to avoid it by just treating these files as binary for this
# conversion, PasDoc doesn't do any processing of these contents.

ALL_OUTPUT := bootstrap.min.css.inc bootstrap.bundle.min.js.inc

.PHONY: all clean

all: $(ALL_OUTPUT)

%.inc: %
	../../../bin/file_to_pascal_data $< $@

clean:
	rm -f $(ALL_OUTPUT)
