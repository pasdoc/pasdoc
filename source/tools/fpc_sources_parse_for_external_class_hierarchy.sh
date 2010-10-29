#!/bin/bash
set -eu

# Process some of the FPC RTL and FCL with PasDoc. For Linux i386 target.
# You probably want to adjust paths below before running.
# This may be used as a demo (that PasDoc can handle FPC sources).
#
# It was also used to make ../component/external_class_hierarchy.txt
# Just take resulting GVClasses.dot, and process it with regexp.

FPC_SOURCES=/home/michalis/sources/fpc/trunk/
OUTPUT_DIR=/tmp/

cd "$FPC_SOURCES"
pasdoc --output="$OUTPUT_DIR" --graphviz-classes \
  -I rtl/objpas/sysutils/ -I rtl/inc/ -I rtl/i386/ -I rtl/unix/ -I rtl/linux/ \
  -I rtl/objpas/classes/ \
  -D UNIX -D cpui386 \
  rtl/unix/sysutils.pp rtl/objpas/objpas.pp rtl/unix/classes.pp \
  rtl/inc/dynlibs.pas rtl/objpas/math.pp rtl/inc/matrix.pp \
  rtl/objpas/typinfo.pp \
  packages/fcl-base/src/contnrs.pp

# System unit fails because of [INTERNPROC: xxx], which is special
# for FPC system unit.
# rtl/linux/system.pp
