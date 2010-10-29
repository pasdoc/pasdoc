# Generate automatic translation files (convert encodings).
# The plan is to automatically make here as much as possible.

ALL_OUTPUT := PasDoc_Languages_Czech_1250.inc

.PHONY: all clean
all: $(ALL_OUTPUT)
clean:
	rm -f $(ALL_OUTPUT)

PasDoc_Languages_Czech_1250.inc: PasDoc_Languages_Czech_iso_8859_2.inc
	iconv --from ISO_8859-2 --to WINDOWS-1250 < $< > $@
