#!/bin/sh

./prepare-fpc.sh

fpc -Sd console/PasDoc_Console.dpr -Fucomponent -FuOptionParser -Ficomponent
