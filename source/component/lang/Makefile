# ------------------------------------------------------------------------------
# Generate automatic translation files (convert encodings).
# We generate all non-UTF8-BOM files from xxx_utf8_bom.inc here.
# See https://pasdoc.github.io/OutputLanguage about the encodings used
# for each language.
#
# Why do need separate files without UTF-8 BOM,
# like PasDoc_Languages_Brasilian_utf8.inc
#  and PasDoc_Languages_Brasilian_utf8_bom.inc? (same thing but with BOM?)
#
# - FPC cannot build PasDoc_Languages.pas if we include
#   PasDoc_Languages_Brasilian_utf8_bom.inc inside.
#   It complains "Fatal: It is not possible to include a file that starts
#   with an UTF-8 BOM in a module that uses a different code page"
#   On the other side, if we place UTF-8 in PasDoc_Languages.pas,
#   it complains about non-UTF-8 files,
#   "PasDoc_Languages_Bosnia_1250.inc(53,21) Error: Malformed UTF-8 string".
#
#   What we want from FPC is to place in literal Strings the bytes we have
#   in the source files, without trying to interpret them as anything.
#   To do this, we cannot have BOM.
#
# - Delphi OTOH needs UTF-8 BOM to actually recognize the files as UTF-8.
#   It cannot include non-UTF-8 encodings at all.
# ------------------------------------------------------------------------------

ALL_OUTPUT := PasDoc_Languages_Bosnia_1250.inc \
	PasDoc_Languages_Brasilian_1252.inc \
	PasDoc_Languages_Catalan_1252.inc \
	PasDoc_Languages_Chinese_gb2312.inc \
	PasDoc_Languages_Croatia_1250.inc \
	PasDoc_Languages_Czech_1250.inc \
	PasDoc_Languages_Czech_iso_8859_2.inc \
	PasDoc_Languages_Danish_1252.inc \
	PasDoc_Languages_Dutch_1252.inc \
	PasDoc_Languages_French_1252.inc \
	PasDoc_Languages_French_ISO_8859_15.inc \
	PasDoc_Languages_German_ISO_8859_15.inc \
	PasDoc_Languages_Hungarian_1250.inc \
	PasDoc_Languages_Indonesian_1252.inc \
	PasDoc_Languages_Italian_1252.inc \
	PasDoc_Languages_Javanese_1250.inc \
	PasDoc_Languages_Polish_1250.inc \
	PasDoc_Languages_Polish_iso_8859_2.inc \
	PasDoc_Languages_Russian_1251.inc \
	PasDoc_Languages_Russian_866.inc \
	PasDoc_Languages_Russian_koi8r.inc \
	PasDoc_Languages_Slovak_1250.inc \
	PasDoc_Languages_Spanish_1252.inc \
	PasDoc_Languages_Swedish_1252.inc \
	PasDoc_Languages_Brasilian_utf8.inc \
	PasDoc_Languages_French_utf8.inc \
	PasDoc_Languages_Bulgarian_utf8.inc \
	PasDoc_Languages_German_utf8.inc \
	PasDoc_Languages_English_utf8.inc \
	PasDoc_Languages_Russian_utf8.inc

.PHONY: all clean
all: $(ALL_OUTPUT)
clean:
	rm -f $(ALL_OUTPUT)

# Generate non-UTF8-files from UTF-8-BOM files.

PasDoc_Languages_%_1250.inc: PasDoc_Languages_%_utf8_bom.inc
	./convert_encoding.sh $< $@ WINDOWS-1250
PasDoc_Languages_%_1252.inc: PasDoc_Languages_%_utf8_bom.inc
	./convert_encoding.sh $< $@ WINDOWS-1252
PasDoc_Languages_%_iso_8859_2.inc: PasDoc_Languages_%_utf8_bom.inc
	./convert_encoding.sh $< $@ ISO-8859-2
PasDoc_Languages_%_gb2312.inc: PasDoc_Languages_%_utf8_bom.inc
	./convert_encoding.sh $< $@ GB2312
PasDoc_Languages_%_1251.inc: PasDoc_Languages_%_utf8_bom.inc
	./convert_encoding.sh $< $@ WINDOWS-1251
PasDoc_Languages_%_866.inc: PasDoc_Languages_%_utf8_bom.inc
	./convert_encoding.sh $< $@ CP866
PasDoc_Languages_%_koi8r.inc: PasDoc_Languages_%_utf8_bom.inc
	./convert_encoding.sh $< $@ KOI8-R
PasDoc_Languages_%_ISO_8859_15.inc: PasDoc_Languages_%_utf8_bom.inc
	./convert_encoding.sh $< $@ ISO-8859-15

# Generate UTF8-without-BOM files from UTF-8-BOM files.

PasDoc_Languages_%_utf8.inc: PasDoc_Languages_%_utf8_bom.inc
	./remove_bom.sh $< $@
