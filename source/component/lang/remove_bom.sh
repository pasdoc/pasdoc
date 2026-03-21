#!/bin/bash
set -eu

# --------------------------------------------------------------------
# Convert file $1 (UTF-8 with BOM) to $2 (UTF-8 without BOM).
# --------------------------------------------------------------------

IN_FILE="$1"
OUT_FILE="$2"
shift 2

AUTO_GENERATED_NOTICE="{ This file is auto-generated, do not edit it directly. Edit instead the source: ${IN_FILE} }"
echo "${AUTO_GENERATED_NOTICE}" > "${OUT_FILE}"

sed "1s|^\xEF\xBB\xBF||" < "${IN_FILE}" >> "${OUT_FILE}"
