#!/bin/bash
set -eu

# Always run this script with current directory set to
# directory where this script is,
# i.e. tests/scripts/ inside pasdoc sources.
#
# $1 is pasdoc's format name, like for pasdoc's --format option.
#
# This script
# - cleans ../correct_output/$1/
# - downloads [http://pasdoc.sourceforge.net/correct_tests_output/$1.tar.gz]
# - and unpacks it to ../correct_output/$1/
#
# Requisites: downloading is done using `wget'.

FORMAT="$1"
shift 1

rm -Rf ../correct_output/"$FORMAT"/ ../correct_output/"$FORMAT".tar.gz
mkdir -p ../correct_output/

cd ../correct_output/
wget http://pasdoc.sourceforge.net/correct_tests_output/"$FORMAT".tar.gz
tar xzvf "$FORMAT".tar.gz