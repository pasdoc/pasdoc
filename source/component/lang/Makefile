# Generate automatic translation files (convert encodings).
# We generate all non-UTF8 files from UTF8 here.
# See https://pasdoc.github.io/OutputLanguage about the encodings used for each language.

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
	PasDoc_Languages_Swedish_1252.inc

.PHONY: all clean
all: $(ALL_OUTPUT)
clean:
	rm -f $(ALL_OUTPUT)

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

.PHONY: test
test:
	file *.inc > /tmp/current_encodings.txt
	diff -uw /tmp/current_encodings.txt correct_encodings.txt
