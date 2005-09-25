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

download_one_format ()
{
  # Parse options
  FORMAT="$1"
  shift 1

  ARCHIVE_FILENAME_NONDIR="$FORMAT".tar.gz

  rm -Rf ../correct_output/"$FORMAT"/ ../correct_output/"$ARCHIVE_FILENAME_NONDIR"
  mkdir -p ../correct_output/

  cd ../correct_output/
  echo "Downloading $ARCHIVE_FILENAME_NONDIR ..."
  wget http://pasdoc.sourceforge.net/correct_tests_output/"$ARCHIVE_FILENAME_NONDIR"

  echo "Unpacking $ARCHIVE_FILENAME_NONDIR ..."
  tar xzf "$ARCHIVE_FILENAME_NONDIR"
}

for FORMAT; do
  download_one_format "$FORMAT"
done