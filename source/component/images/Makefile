# Generate all *.gif.inc files from *.gif files in this directory.
# Uses ../../tools/file_to_pascal_data program, so be sure that it's
# compiled.

ALL_OUTPUT := automated.gif.inc \
  private.gif.inc \
  protected.gif.inc \
  public.gif.inc \
  published.gif.inc

.PHONY: all clean

all: $(ALL_OUTPUT)

clean:
	rm -f $(ALL_OUTPUT)

%.gif.inc: %.gif
	../../../bin/file_to_pascal_data $< $@
