#!/bin/sh

#./prepare-fpc.sh

fpc -S2 console/PasDoc_Console.dpr -Fucomponent -FuOptionParser -Ficomponent -Sh \
  -Cr -g -gl -gg -Ci -Co -Ct -CR\
  $@
