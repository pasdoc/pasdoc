@echo off
rem generate PasDoc autodoc under Windows
set PASDOC=..\console\pasdoc_console
set INCLUDE=..\component
set SOURCE=..\component\*.pas ..\component\tiptue\*.pas ..\optionparser\*.pas ..\console\*.pas
set OPTIONS=--write-uses-list --auto-abstract --title "PasDoc's autodoc" --introduction=introduction.txt --use-tipue-search
set GRAPHVIZ=--graphviz-classes --graphviz-uses --link-gv-classes jpg --link-gv-uses jpg
set OUTPUT=html\
%PASDOC% --include=%INCLUDE% --output=%OUTPUT% %OPTIONS% %GRAPHVIZ% %SOURCE%

dot -Grankdir=LR -Tjpg -ohtml\GVClasses.jpg html\GVClasses.dot
dot -Grankdir=LR -Tjpg -ohtml\GVUses.jpg html\GVUses.dot
