#######################################################################
# BASE CONFIGURATION
#######################################################################

# The version of the package.
# This must be changed on each version change,
# documented at ReleaseMaking wiki page.
VERSION := 0.8.8.3

# The name of the package / file name
PACKAGENAME := pasdoc

# Location of units source code.
UNITDIRS := ./source/component ./source/console ./source/OptionParser \
  ./source/component/tipue

INCLUDEDIRS := ./source/component

# Base file to compile
FILE := ./source/console/PasDoc_Console.dpr

# Operating system path separator.
# Note that default value, /, is good not only for Unices but also for Win32.
PATHSEP := /

# Base directory where binaries will go
BINDIR := bin

# Base directory where libs, units, objects files will go
OUTDIR := lib

# The following is for creating the final package, comment out
# if that particular section is not used.
# BINFILES: Files that will go into the resulting bin directory
# SRCFILES: Files that will go into the resulting src directory
# DOCFILES: Files that will go into the resulting docs directory
BINFILES := $(BINDIR)/pasdoc_console
DOCFILES := LICENSE ChangeLog ./docs/pasdoc.css ./docs/pasdoc.html ./docs/pasdoc.pdf
SRCFILES := ./source/*

# Temporary directory used for preparing archives
PACKAGEDIR := $(TEMP)/$(PACKAGENAME)

############################################################################
# Change the paths to the correct types
#######################################################################

PACKAGEDIR := $(subst /,$(PATHSEP),$(PACKAGEDIR))
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

CURRENTDIR:=$(shell pwd)

############################################################################
# FreePascal Configuration
############################################################################

# FPCDEFAULT means "use current os and processor",
# calling just fpc binary on the path
FPCDEFAULT = fpc

# By default all of below variables are set to $(FPCDEFAULT), because
# $(FPCDEFAULT) is the only good default value that can possibly work
# for everyone. You can override them at `make' command-line,
# e.g. if you have different FPC versions and you use them to do cross-compiling.
FPCWIN32 = $(FPCDEFAULT)
FPCGO32 = $(FPCDEFAULT)
FPCLINUXX86 = $(FPCDEFAULT)
FPCLINUXM68K = $(FPCDEFAULT)
FPCAMIGA = $(FPCDEFAULT)
FPCBEOS = $(FPCDEFAULT)
FPCOS2 = $(FPCDEFAULT)

FPCUNITDIRS = $(foreach units,$(UNITDIRS),-Fu$(units))
FPCINCLUDEDIRS = $(foreach units,$(INCLUDEDIRS),-Fi$(units))

# TODO: provide variables and targets to compile by default in debug mode.

FPCFLAGS = -FE$(BINDIR) -FU$(OUTDIR) -dRELEASE @pasdoc-fpc.cfg $(FPCUNITDIRS) $(FPCINCLUDEDIRS)

############################################################################
# Delphi configuration
############################################################################

DCC	= dcc32
DCCFLAGS = -E$(BINDIR) -N$(OUTDIR) -L$(OUTDIR) -M -H -W -$$J+ -$$R+ -U..\common\src\delphi -DCPU86 -DENDIAN_LITTLE
DCCUNITDIRS = $(foreach units,$(UNITDIRS),-U$(units))

############################################################################
# Virtual Pascal configuration
############################################################################

# TODO: check this, either remove (if vpc does not work anymore),
# or add to CompilingPasDoc as supported compiler,
# remove hardcoded paths to vpc installation.

VPC	= F:\vp21\bin.w32\vpc.exe
VPCRTLWIN32UNITDIR = F:\vp21\units.w32
VPCRTLWIN32LIBDIR = -LF:\vp21\units.w32 -LF:\vp21\lib.w32
VPCRTLOS2UNITDIR = F:\vp21\units.os2
VPCRTLOS2LIBDIR = -LF:\vp21\units.os2 -LF:\vp21\lib.os2
VPCUNITDIRS = $(foreach units,$(UNITDIRS),-U$(units))
VPCINCDIRS = $(foreach units,$(UNITDIRS),-I$(units))
VPCFLAGS = -E$(BINDIR) -M -$$J+ -$$R+ -DCPU86 -DENDIAN_LITTLE -O$(OUTDIR) $(VPCINCDIRS) -L$(OUTDIR)

############################################################################
# Targets
############################################################################

.PHONY: default clean build-fpc-default build-fpc-win32 build-fpc-go32 \
  build-fpc-linux build-fpc-linuxm68k build-fpc-amiga build-fpc-beos \
  build-fpc-os2 build-dcc build-vpc-win32 build-vpc-os2 help \
  makepkg makego32 makewin32 makeos2 makebeos makelinuxm68k \
  makelinux makeamiga makesrc makeall

# Default target
default: build-fpc-default

# Clean up the output files.
clean:
ifdef OUTDIR
	rm -f $(OUTDIR)/*
endif
ifdef BINDIR
	rm -f $(BINDIR)/*
endif

build-fpc-default:
	$(FPCDEFAULT) $(FPCFLAGS) $(FILE)

build-fpc-win32:
	$(FPCWIN32) $(FPCFLAGS) $(FILE)

build-fpc-go32:
	$(FPCGO32) $(FPCFLAGS) $(FILE)

build-fpc-linux:
	$(FPCLINUXX86) $(FPCFLAGS) $(FILE)

build-fpc-linuxm68k:
	$(FPCLINUXM68K) $(FPCFLAGS) $(FILE)

build-fpc-amiga:
	$(FPCAMIGA) $(FPCFLAGS) $(FILE)

build-fpc-beos:
	$(FPCBEOS) $(FPCFLAGS) $(FILE)

build-fpc-os2:
	$(FPCOS2) $(FPCFLAGS) $(FILE)

build-dcc:
	$(DCC) $(DCCFLAGS) $(DCCUNITDIRS) $(FILE)

build-vpc-win32:
	$(VPC) -CW $(VPCFLAGS)  $(VPCRTLWIN32LIBDIR) -U$(VPCRTLWIN32UNITDIR) $(VPCUNITDIRS) $(FILE)

build-vpc-os2:
	$(VPC) -CO $(VPCFLAGS)  $(VPCRTLOS2LIBDIR) -U$(VPCRTLOS2UNITDIR) $(VPCUNITDIRS) $(FILE)


help:
	@echo Commands for building the targets.
	@echo Important commands ----------------------------------
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

makepkg:
	rm -rf $(PACKAGEDIR)
	mkdir $(PACKAGEDIR)
ifdef BINFILES
	mkdir $(PACKAGEDIR)$(PATHSEP)bin
	mv $(BINFILES)$(EXE) pasdoc$(EXE)
	cp -R pasdoc$(EXE) $(PACKAGEDIR)$(PATHSEP)bin
endif
ifdef DOCFILES
	mkdir $(PACKAGEDIR)$(PATHSEP)docs
	cp -R $(DOCFILES) $(PACKAGEDIR)$(PATHSEP)docs
endif
ifdef SRCFILES
	mkdir $(PACKAGEDIR)$(PATHSEP)src
	cp -R $(SRCFILES) $(PACKAGEDIR)$(PATHSEP)src
	find $(PACKAGEDIR)$(PATHSEP)src -name CVS -prune -exec rm -fR '{}' ';'
endif
	cd $(PACKAGEDIR)$(PATHSEP)..; zip -r $(PACKAGENAME) $(PACKAGENAME)/*
	mv $(PACKAGEDIR)$(PATHSEP)..$(PATHSEP)$(PACKAGENAME).zip .

makego32:
	$(MAKE) -C . clean
	$(MAKE) -C . build-fpc-go32
	$(MAKE) -C . makepkg EXE=.exe SRCFILES=
	mv -f $(PACKAGENAME).zip $(PACKAGENAME)-$(VERSION)-go32.zip

makewin32:
	$(MAKE) -C . clean
	$(MAKE) -C . build-fpc-win32
	$(MAKE) -C . makepkg EXE=.exe SRCFILES=
	mv -f $(PACKAGENAME).zip $(PACKAGENAME)-$(VERSION)-win32.zip

makeos2:
	$(MAKE) -C . clean
	$(MAKE) -C . build-fpc-os2
	$(MAKE) -C . makepkg EXE=.exe SRCFILES=
	mv -f $(PACKAGENAME).zip $(PACKAGENAME)-$(VERSION)-os2.zip

makebeos:
	$(MAKE) -C . clean
	$(MAKE) -C . build-fpc-beos
	$(MAKE) -C . makepkg SRCFILES=
	mv -f $(PACKAGENAME).zip $(PACKAGENAME)-$(VERSION)-be-x86.zip

makelinuxm68k:
	$(MAKE) -C . clean
	$(MAKE) -C . build-fpc-linuxm68k
	$(MAKE) -C . makepkg SRCFILES=
	mv -f $(PACKAGENAME).zip $(PACKAGENAME)-$(VERSION)-linux-m68k.zip

makelinux:
	$(MAKE) -C . clean
	$(MAKE) -C . build-fpc-linux
	$(MAKE) -C . makepkg SRCFILES=
	mv -f $(PACKAGENAME).zip $(PACKAGENAME)-$(VERSION)-linux-x86.zip


makeamiga:
	$(MAKE) -C . clean
	$(MAKE) -C . build-fpc-amiga
	$(MAKE) -C . makepkg SRCFILES=
	mv -f $(PACKAGENAME).zip $(PACKAGENAME)-$(VERSION)-amiga-m68k.zip

makesrc:
	$(MAKE) -C . clean
	$(MAKE) -C . makepkg BINFILES=
	mv -f $(PACKAGENAME).zip $(PACKAGENAME)-$(VERSION)-src.zip



makeall:
	$(MAKE) -C . clean
	$(MAKE) -C . makego32
	$(MAKE) -C . makewin32
	$(MAKE) -C . makebeos
	$(MAKE) -C . makelinuxm68k
	$(MAKE) -C . makelinux
	$(MAKE) -C . makeamiga
	$(MAKE) -C . makesrc BINFILES=

