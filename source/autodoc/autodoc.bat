@echo off

rem Generate PasDoc autodoc under Windows.
rem
rem Note that it's adviced to use Makefile (you will need GNU `make')
rem in this directory instead of this BAT script.
rem Makefile is portable and will be always more up-to-date than this
rem BAT script, not to mention that Makefile gives you many more possibilities.

set PASDOC=..\console\pasdoc
set INCLUDE=..\component
set SOURCE=..\component\*.pas ..\component\tipue\*.pas ..\console\*.pas
set OPTIONS=--write-uses-list --auto-abstract --title "PasDoc's autodoc" --introduction=introduction.txt --use-tipue-search
set GRAPHVIZ=--graphviz-classes --graphviz-uses --link-gv-classes jpg --link-gv-uses jpg
set OUTPUT=html\
%PASDOC% --include=%INCLUDE% --output=%OUTPUT% %OPTIONS% %GRAPHVIZ% %SOURCE%

dot -Grankdir=LR -Tjpg -ohtml\GVClasses.jpg html\GVClasses.dot
dot -Grankdir=LR -Tjpg -ohtml\GVUses.jpg html\GVUses.dot
pause
