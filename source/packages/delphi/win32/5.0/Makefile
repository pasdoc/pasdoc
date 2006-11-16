pasdoc_package.dpk: ../7.0/pasdoc_package.dpk
	sed $< \
	  -e 's/{$$R \*.res}/{$$R *.RES} {$$ALIGN ON}/ig' \
	  -e 's/rtl,//ig' \
	  -e 's/vclx,/vclx50,/ig' \
	  -e 's/vcl;/vcl50;/ig' \
	  > $@

.PHONY: clean
clean:
	rm -f pasdoc_package.dpk