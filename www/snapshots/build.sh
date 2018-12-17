#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# Script run by Jenkins, see
# https://github.com/castle-engine/castle-engine/wiki/Cloud-Builds-(Jenkins) ,
# to update PasDoc snapshots
# visible on http://michalis.ii.uni.wroc.pl/pasdoc-snapshots/ .

rm -f pasdoc-*.tar.gz pasdoc-*.zip

make clean
make dist-src
make dist-linux-x86_64
make dist-win32
make dist-win64
