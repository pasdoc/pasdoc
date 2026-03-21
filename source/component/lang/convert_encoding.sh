#!/bin/bash
set -eu

# --------------------------------------------------------------------
# Convert file $1 (from UTF-8 with BOM)
# to file $2 (with encoding indicated in $3, with name understood by iconv)
# --------------------------------------------------------------------

IN_FILE="$1"
OUT_FILE="$2"
ENCODING="$3"
shift 3

AUTO_GENERATED_NOTICE="{ This file is auto-generated, do not edit it directly. Edit instead the source: ${IN_FILE} }"
echo "${AUTO_GENERATED_NOTICE}" > "${OUT_FILE}"

# sed removes the UTF-8 BOM (as iconv cannot handle it)

sed "1s|^\xEF\xBB\xBF||" < "${IN_FILE}" | iconv --from UTF-8 --to "${ENCODING}" >> "${OUT_FILE}"
