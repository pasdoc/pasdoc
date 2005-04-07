#!/bin/sh
set -e

#./prepare-fpc.sh

fpc "$@" @pasdoc-fpc.cfg console/PasDoc_Console.dpr
