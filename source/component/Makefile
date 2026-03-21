# Generates all *.inc files from text/binary files in this directory.
# Also recursively cleans/generates files in subdirectories.

ALL_OUTPUT := pasdoc.css.inc external_class_hierarchy.txt.inc

.PHONY: all clean

all: $(ALL_OUTPUT)
	$(MAKE) -C bootstrap
	$(MAKE) -C lang
	$(MAKE) -C tipue

clean:
	rm -f $(ALL_OUTPUT)
	$(MAKE) -C bootstrap clean
	$(MAKE) -C lang clean
	$(MAKE) -C tipue clean

%.inc: %
	../../bin/file_to_pascal_string $< $@
