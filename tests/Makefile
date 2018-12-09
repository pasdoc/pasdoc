.PHONY: default
default:
	./run_all_tests.sh

.PHONY: validate_html
validate_html:
	scripts/validate_html.sh

.PHONY: validate_simplexml
validate_simplexml:
	scripts/validate_simplexml.sh

.PHONY: clean
clean:
	rm -Rf \
	  scripts/check_cache_tmp/ \
	  scripts/upload_correct_tests_output_tmp/
