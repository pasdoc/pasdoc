#!/bin/sh

echo -n Creating links for FPC compilation...

find -type f -iname '*.pas' -exec bash prepare-fpc-link.sh "{}" ';'
find -type f -iname '*.inc' -exec bash prepare-fpc-link.sh "{}" ';'

echo done.
