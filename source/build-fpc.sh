#!/bin/sh

#./prepare-fpc.sh

/usr/local/bin/ppc386 -Mobjfpc console/PasDoc_Console.dpr -Fucomponent -FuOptionParser -Ficomponent -Sh \
  -Cr -g -gl -gg -Ci -Co -Ct -CR\
  $@
