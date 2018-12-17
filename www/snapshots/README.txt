These are things used to generate nightly builds
on http://michalis.ii.uni.wroc.pl/pasdoc-snapshots/ .

.HEADER.html is just a welcome text for visitors.

build.sh upload.sh are bash scripts that do the actual job.
They contain some paths hardcoded for the michalis.ii.uni.wroc.pl
server configuration, but still it may serve as a useful example how to
setup snapshots, cross-compile etc. project like pasdoc.
