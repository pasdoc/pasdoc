#! /bin/sh

#
#  Copyright 2020 PasDoc developers.
#
#  This file is part of "PasDoc".
#
#  "PasDoc" is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  "PasDoc" is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with "PasDoc"; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
#

# Author: Silvio Clecio (silvioprog)

# TODO: add "pasdoc_gui.appdata.xml" as soon as this issue is solved: https://github.com/linuxdeploy/linuxdeploy/issues/31

set -e

app_desc="PasDoc GUI"
app_name="pasdoc_gui"
app_version="$(sed -n "s/^  PASDOC_VERSION = '\(.*\)'.*$/\1/ p" ../../source/component/PasDoc_Versions.pas)"

app_basedir="$(realpath ./)"
app_icons=$(ls -d "$app_basedir"/pngs/*)
app_arch=$(uname -m)
app_image="$app_desc-$app_version-$app_arch.AppImage"
app_image="$(echo "$app_image" | tr -s ' ' | tr ' ' '_')"
app_desktop="$app_basedir/$app_name.desktop"
app_exe="$app_basedir/$app_name"
app_dir="$app_basedir/AppDir"

tmpdir=$(dirname "$(mktemp -u)")
linuxdeploy="$tmpdir/linuxdeploy-x86_64.AppImage"

if [ ! -f "$app_exe" ]; then
    echo "Executable does not exist"
    exit 1
fi

cleanup() {
    rm -rf "$app_dir"
    rm -f "$app_desktop"
}

cleanup_all() {
    cleanup
    rm -f "$app_basedir/$app_image"
}

cleanup_all

if [ ! -s "$linuxdeploy" ]; then
    wget -c "https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage" -O "$linuxdeploy"
fi

if [ ! -x "$linuxdeploy" ]; then
    chmod a+x "$linuxdeploy"
fi

unset icons
for icon in $app_icons; do
    icons="$icons --icon-file $icon/$app_name.png"
done

echo "[Desktop Entry]
Name=$app_desc
Exec=$app_name
TryExec=$app_name
Icon=$app_name
Type=Application
Terminal=false
StartupNotify=true
Categories=Development;
X-AppImage-Name=$app_desc
X-AppImage-Arch=$app_arch" >"$app_desktop"

VERSION='' OUTPUT="$app_basedir/$app_image" "$linuxdeploy" \
    --executable "$app_exe" \
    $icons \
    --desktop-file "$app_desktop" \
    --appdir "$app_dir" \
    --output appimage

cleanup

echo ""
echo "-- Created file: $app_basedir/$app_image"
