#!/usr/bin/env bash
set -eu

# --------------------------------------------------------------------
# Parse FPC source code.
# Edit SOURCES_DIR to point to the FPC source code on your system.
# --------------------------------------------------------------------

# Assume pasdoc is on $PATH, if PASDOC_BIN not set.
PASDOC_BIN="${PASDOC_BIN:-pasdoc}"

SOURCES_DIR="C:/cygwin64/home/michalis/installed/fpclazarus/fpc322/fpcsrc/"
OUTPUT_DIR="fpc_sources"

# OS and CPU which we should assume for parsing files.
# Note: This is not completely configurable by only these variables,
# below we also hardcode the assumption that it's Unix and some related defines.
# More work would be needed to make OS / CPU trivially and correctly switchable
# for this test.
OS='linux'
CPU='x86_64'

mkdir -p "${OUTPUT_DIR}"
# Non-recursive searching, to only account for single OS/CPU in RTL.
find "${SOURCES_DIR}/rtl" \
     "${SOURCES_DIR}/rtl/unix" \
     "${SOURCES_DIR}/rtl/objpas" \
     "${SOURCES_DIR}/rtl/${OS}" \
     "${SOURCES_DIR}/rtl/${CPU}" \
     "${SOURCES_DIR}/rtl/${OS}/${CPU}" \
     -maxdepth 1 \
      -type f '(' -name "*.pas" -o -name "*.pp" -')' > "${OUTPUT_DIR}/sources.txt"

"${PASDOC_BIN}" \
  --define LINUX \
  --define UNIX \
  --define FPC_FULLVERSION=32200 \
  --output="${OUTPUT_DIR}" \
  --source="${OUTPUT_DIR}/sources.txt" \
  --include "${SOURCES_DIR}/rtl/inc/" \
  --include "${SOURCES_DIR}/rtl/unix/" \
  --include "${SOURCES_DIR}/rtl/objpas/" \
  --include "${SOURCES_DIR}/rtl/objpas/classes/" \
  --include "${SOURCES_DIR}/rtl/objpas/sysutils/" \
  --include "${SOURCES_DIR}/rtl/${OS}/" \
  --include "${SOURCES_DIR}/rtl/${CPU}/" \
  --include "${SOURCES_DIR}/rtl/${OS}/${CPU}/" \
  | tee "${OUTPUT_DIR}/pasdoc.log"
