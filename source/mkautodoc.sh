#!/bin/sh

# this script creates PasDoc generated documentation for PasDoc
# including a Classes and Uses hierarchy graph (requires GraphViz)
# in the subdirectory autodoc

[ -d autodoc ] || mkdir autodoc
console/PasDoc_Console "$@" --graphviz-classes --graphviz-uses --write-uses-list --output=autodoc --include component component/*.pas
dot -Tjpg -oautodoc/GVClasses.jpg autodoc/GVClasses.dot
dot -Tjpg -oautodoc/GVUses.jpg autodoc/GVUses.dot
