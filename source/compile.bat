ppc386 -Fucomponent -Fuconsole -FuOptionParser -S2 -CR -Cr -Sh .\console\Pasdoc_console.dpr
if ERRORLEVEL 1 goto error
cd console
move pasdoc_console.exe ..\pasdoc.exe
cd..
del /s *.ow
del /s *.ppw
goto end
:error
Echo Error!
:end



