ifndef VERSION
VERSION=0.0
endif

#######################################################################
# USER CONFIGURATION : These should be configured manually 
#######################################################################
# PACKAGENAME : The name of the package / file name
# VERSION : The version of the package
# DOCTITLE : The title of the final documentation
# PATHSEP : Operating system path separator
# UNITDIRS : Location of units source code and units
# FILE : Base file to compile
# BINDIR : Base directory where binaries will go
# The following is for creating the final package, comment out
# if that particular section is not used.
# BINFILES: Files that will go into the resulting bin directory
# SRCFILES: Files that will go into the resulting src directory
# DOCFILES: Files that will go into the resulting docs directory
# OUTDIR : Base directory where libs, units, objects files will go
# DATAFILES : Files to copy for the tests
# DATE : Utility name to get the ISO Date
PACKAGENAME:=pasdoc
DOCTITLE:=Pasdoc documentation
UNITDIRS := ./source ./source/component ./source/console ./source/OptionParser
FILE:=./source/console/pasdoc_console.dpr
BINFILES:=./bin/pasdoc_console
DOCFILES:= LICENSE ChangeLog ./docs/pasdoc.css ./docs/pasdoc.html ./docs/pasdoc.pdf ./docs/pasdoc.html
#SRCFILES:=./source/*


PATHSEP:=\\
BINDIR := ./bin
OUTDIR := ./lib
DATE := getdate.exe

############################################################################
# Change the paths to the correct types
PACKAGEDIR := $(TEMP)/$(PACKAGENAME)
ZIPPKGDIR := $(subst /,$(PATHSEP),$(PACKAGEDIR))
ifdef OUTDIR
OUTDIR:= $(subst /,$(PATHSEP),$(OUTDIR))
endif
ifdef BINDIR
BINDIR:= $(subst /,$(PATHSEP),$(BINDIR))
endif
FILE:=$(subst /,$(PATHSEP),$(FILE))
UNITDIRS:=$(subst /,$(PATHSEP),$(UNITDIRS))
ifdef BINFILES
BINFILES:=$(subst /,$(PATHSEP),$(BINFILES))
endif
ifdef DOCFILES
DOCFILES:=$(subst /,$(PATHSEP),$(DOCFILES))
endif
ifdef SRCFILES
SRCFILES:=$(subst /,$(PATHSEP),$(SRCFILES))
endif

# Get the current date
DATESTR:= $(shell $(DATE))
CURRENTDIR:=$(shell pwd)


# FREEPASCAL CONFIGURATION
FPCWIN32    = fpc
FPCGO32 = C:\pp\cross\i386-go32v2\bin\ppc386.exe
FPCLINUXX86 = C:\pp\cross\i386-linux\bin\ppc386.exe
FPCLINUXM68K = C:\pp\cross\m68k-linux\bin\ppc68k.exe
FPCAMIGA = C:\pp\cross\m68k-amiga\bin\ppc68k.exe
FPCBEOS = C:\pp\cross\i386-beos\bin\ppc386.exe
FPCOS2 = C:\pp\cross\i386-os2\bin\ppc386.exe

FPC2   = C:\pp2\bin\win32\ppc386.exe
FPCUNITDIRS = $(foreach units,$(UNITDIRS),-Fu$(units))
FPCFLAGS = -FE$(BINDIR) -FU$(OUTDIR)  -S2 -vihwn -Sh -Ct


# DELPHI CONFIGURATION
DCC	= dcc32
DCCFLAGS = -E$(BINDIR) -N$(OUTDIR) -L$(OUTDIR) -M -H -W -$$J+ -$$R+ -U..\common\src\delphi -DCPU86 -DENDIAN_LITTLE
DCCUNITDIRS = $(foreach units,$(UNITDIRS),-U$(units))

# Virtual Pascal configuration
VPC	= F:\vp21\bin.w32\vpc.exe
VPCRTLWIN32UNITDIR = F:\vp21\units.w32
VPCRTLWIN32LIBDIR = -LF:\vp21\units.w32 -LF:\vp21\lib.w32
VPCRTLOS2UNITDIR = F:\vp21\units.os2
VPCRTLOS2LIBDIR = -LF:\vp21\units.os2 -LF:\vp21\lib.os2
VPCUNITDIRS = $(foreach units,$(UNITDIRS),-U$(units))
VPCINCDIRS = $(foreach units,$(UNITDIRS),-I$(units))
VPCFLAGS = -E$(BINDIR) -M -$$J+ -$$R+ -DCPU86 -DENDIAN_LITTLE -O$(OUTDIR) $(VPCINCDIRS) -L$(OUTDIR)

# Clean up the output files.
clean:
ifdef OUTDIR
	del /Q $(OUTDIR)\*.*
endif	
ifdef BINDIR
	del /Q $(BINDIR)\*.*
endif	


build-fpc-win32:
	$(FPCWIN32) $(FPCFLAGS) $(FPCUNITDIRS) $(FILE)
	
build-fpc-go32:
	$(FPCGO32) $(FPCFLAGS) $(FPCUNITDIRS) $(FILE)
	
build-fpc-linux:
	$(FPCLINUXX86) $(FPCFLAGS) $(FPCUNITDIRS) $(FILE)
	
build-fpc-linuxm68k:
	$(FPCLINUXM68K) $(FPCFLAGS) $(FPCUNITDIRS) $(FILE)
	
build-fpc-amiga:
	$(FPCAMIGA) $(FPCFLAGS) $(FPCUNITDIRS) $(FILE)
	
build-fpc-beos:
	$(FPCBEOS) $(FPCFLAGS) $(FPCUNITDIRS) $(FILE)
	
build-fpc-os2:
	$(FPCOS2) $(FPCFLAGS) $(FPCUNITDIRS) $(FILE)
	

build-fpc2:
	$(FPC2) $(FPCFLAGS) $(FPCUNITDIRS) $(FILE)
	
build-dcc:	
	$(DCC) $(DCCFLAGS) $(DCCUNITDIRS) $(FILE)
	
build-vpc-win32:	
	$(VPC) -CW $(VPCFLAGS)  $(VPCRTLWIN32LIBDIR) -U$(VPCRTLWIN32UNITDIR) $(VPCUNITDIRS) $(FILE)
	
build-vpc-os2:	
	$(VPC) -CO $(VPCFLAGS)  $(VPCRTLOS2LIBDIR) -U$(VPCRTLOS2UNITDIR) $(VPCUNITDIRS) $(FILE)
	

help:
	@echo Commands for building the targets.
	@echo Important commands ----------------------------------
	@echo make buildall : Test compilation on different compiler types.
	@echo make makeall: Create a package compiled for all KNOWN targets
	@echo make clean : Clean all unused files
	@echo Other commands --------------------------------------
	@echo make makewin32: Create a package compiled for Win32 (FPC)
	@echo make makeos2: Create a package compiled for OS/2 (VPC)
	@echo make makego32: Create a package compiled for GO32V2 (FPC)
	@echo make makebeos: Create a package compiled for BeOS x86 (FPC)
	@echo make makelinuxm68k: Create a package compiled for Linux-m68k (FPC)
	@echo make makelinux: Create a package compiled for Linux-x86 (FPC)
	@echo make makeamiga: Create a package compiled for AmigaOS (FPC)
	

buildall:
	echo Building all
	$(MAKE) -s -C . clean
	$(MAKE) -s -C . build-fpc-win32
	$(MAKE) -s -C . clean
	$(MAKE) -s -C . build-fpc2
	$(MAKE) -s -C . clean
	$(MAKE) -s -C . build-dcc
	$(MAKE) -s -C . clean
	$(MAKE) -s -C . clean
	
makepkg:	
	rm -rf $(ZIPPKGDIR)
	mkdir $(ZIPPKGDIR)
ifdef BINFILES	
	mkdir $(ZIPPKGDIR)$(PATHSEP)bin
	mv $(BINFILES)$(EXE) pasdoc$(EXE)
	cp -R pasdoc$(EXE) $(ZIPPKGDIR)$(PATHSEP)bin
endif	
ifdef DOCFILES
	mkdir $(ZIPPKGDIR)$(PATHSEP)docs
	cp -R $(DOCFILES) $(ZIPPKGDIR)$(PATHSEP)docs
endif
ifdef SRCFILES
	mkdir $(ZIPPKGDIR)$(PATHSEP)src
	cp -R $(SRCFILES) $(ZIPPKGDIR)$(PATHSEP)src
endif	
	rmcvsdir $(ZIPPKGDIR)
	echo cd /D $(ZIPPKGDIR) > zipit.bat
	echo cd.. >> zipit.bat
	echo zip -r $(PACKAGENAME) $(PACKAGENAME)/* >> zipit.bat
	echo cd /D %%1 >> zipit.bat
	zipit $(CURRENTDIR)
	rm -f zipit.bat
	cp $(TEMP)$(PATHSEP)$(PACKAGENAME).zip .
	rm -f $(TEMP)$(PATHSEP)$(PACKAGENAME).zip
	
makego32:	
	$(MAKE) -C . clean
	$(MAKE) -C . build-fpc-go32
	$(MAKE) -C . makepkg EXE=.exe
	mv -f $(PACKAGENAME).zip $(PACKAGENAME)$(VERSION)-go32.zip
	
makewin32:	
	$(MAKE) -C . clean
	$(MAKE) -C . build-fpc-win32
	$(MAKE) -C . makepkg EXE=.exe 
	mv -f $(PACKAGENAME).zip $(PACKAGENAME)$(VERSION)-win32.zip

makeos2:
	$(MAKE) -C . clean
	$(MAKE) -C . build-fpc-os2
	$(MAKE) -C . makepkg EXE=.exe
	mv -f $(PACKAGENAME).zip $(PACKAGENAME)$(VERSION)-os2.zip
	
makebeos:
	$(MAKE) -C . clean
	$(MAKE) -C . build-fpc-beos
	$(MAKE) -C . makepkg
	mv -f $(PACKAGENAME).zip $(PACKAGENAME)$(VERSION)-be-x86.zip
	
makelinuxm68k:
	$(MAKE) -C . clean
	$(MAKE) -C . build-fpc-linuxm68k
	$(MAKE) -C . makepkg
	mv -f $(PACKAGENAME).zip $(PACKAGENAME)$(VERSION)-linux-m68k.zip

makelinux:
	$(MAKE) -C . clean
	$(MAKE) -C . build-fpc-linux
	$(MAKE) -C . makepkg
	mv -f $(PACKAGENAME).zip $(PACKAGENAME)$(VERSION)-linux-x86.zip


makeamiga:
	$(MAKE) -C . clean
	$(MAKE) -C . build-fpc-amiga
	$(MAKE) -C . makepkg
	mv -f $(PACKAGENAME).zip $(PACKAGENAME)$(VERSION)-amiga-m68k.zip
	
makesrc:
	$(MAKE) -C . clean
	$(MAKE) -C . makepkg
	mv -f $(PACKAGENAME).zip $(PACKAGENAME)$(VERSION)src.zip
	


makeall:
	$(MAKE) -C . clean
	$(MAKE) -s -C . buildall  
	$(MAKE) -C . clean
	$(MAKE) -C . makego32
	$(MAKE) -C . makewin32
	$(MAKE) -C . makebeos
	$(MAKE) -C . makelinuxm68k
	$(MAKE) -C . makelinux
	$(MAKE) -C . makeamiga
	$(MAKE) -C . makesrc BINFILES=
	
