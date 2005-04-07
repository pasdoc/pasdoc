ppc386 @pasdoc-fpc.cfg .\console\Pasdoc_console.dpr
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



