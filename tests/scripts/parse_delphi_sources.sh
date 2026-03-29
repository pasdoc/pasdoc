#!/usr/bin/env bash
set -eu

# --------------------------------------------------------------------
# Parse Delphi source code.
# You need to have Delphi installed for this to work.
# Edit SOURCES_DIR below to point to the Delphi source code on your system.
# --------------------------------------------------------------------

# Assume pasdoc is on $PATH, if PASDOC_BIN not set.
PASDOC_BIN="${PASDOC_BIN:-pasdoc}"

SOURCES_DIR="C:/Program Files (x86)/Embarcadero/Studio/37.0/source/"
OUTPUT_DIR="delphi_sources"

mkdir -p "${OUTPUT_DIR}"
find "${SOURCES_DIR}" -type f -name "*.pas" > "${OUTPUT_DIR}/sources.txt"

# Note that we define MSWINDOWS, because System.Curl for parsing
# requires either MSWINDOWS or POSIX . It cannot be parsed without
# either of those defined.

# shellcheck disable=SC2086
"${PASDOC_BIN}" \
  --define MSWINDOWS \
  --output="${OUTPUT_DIR}" \
  --source="${OUTPUT_DIR}/sources.txt" \
  --include "${SOURCES_DIR}/Indy10/Core/" \
  --include "${SOURCES_DIR}/soap/" \
  | tee "${OUTPUT_DIR}/pasdoc.log"
