#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

OUTPUT_BASE_PATH=/var/www/pasdoc-snapshots/
OUTPUT_PATH="${OUTPUT_BASE_PATH}"`date +%F`/
mkdir -p "$OUTPUT_PATH"

# Copy archives to output dir (keep them also here, to allow Jenkins archiveArtifacts)
cp -f pasdoc-*.tar.gz pasdoc-*.zip "$OUTPUT_PATH"

# Create "latest" link, comfortable for users.
rm -f "${OUTPUT_BASE_PATH}"latest
ln -s `date +%F` "${OUTPUT_BASE_PATH}"latest
echo "Updated ${OUTPUT_BASE_PATH}latest symlink to point to" `date +%F`

# Clean old snapshots, to conserve disk space.
# Keep only snapshots from last couple of days.
# Commented out: pointless for now, we don't have so many commits to PasDoc.
# pushd .
# cd "${OUTPUT_BASE_PATH}"
# set +e
# find . -mindepth 1 -maxdepth 1 \
#   -type d -and \
#   -name '????-??-??' -and \
#   '(' -not -iname `date +%F` ')' -and \
#   '(' -not -iname `date --date='-1 day' +%F` ')' -and \
#   '(' -not -iname `date --date='-2 day' +%F` ')' -and \
#   '(' -not -iname `date --date='-3 day' +%F` ')' -and \
#   '(' -not -iname `date --date='-4 day' +%F` ')' -and \
#   '(' -not -iname `date --date='-5 day' +%F` ')' -and \
#   '(' -not -iname `date --date='-6 day' +%F` ')' -and \
#   '(' -not -iname `date --date='-7 day' +%F` ')' -and \
#   -exec rm -Rf '{}' ';'
# set -e
# popd

# Setting snapshots permissions
chmod a+rX "${OUTPUT_BASE_PATH}"
echo "Updated permissions of snapshots files."
