# Generates all *.inc files from text/binary files in this directory.
# Also, generate automatic translation files (convert encodings).

ALL_OUTPUT := pasdoc.css.inc PasDoc_Languages_Czech_CP1250.inc

.PHONY: all clean

all: $(ALL_OUTPUT)

pasdoc.css.inc: pasdoc.css
	../tools/file_to_pascal_string $< $@

PasDoc_Languages_Czech_CP1250.inc: PasDoc_Languages_Czech_ISO_8859_2.inc
	iconv --from ISO_8859-2 --to WINDOWS-1250 < $< > $@

clean:
	rm -f $(ALL_OUTPUT)