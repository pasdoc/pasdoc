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
DOCFILES := LICENSE ChangeLog docs/README
SRCFILES := ./source/*

# Temporary directory used for preparing archives
ifdef TEMP
PACKAGEBASEDIR := $(TEMP)
else
# /tmp is a good guess for Unices, and for Win32 if someone uses Cygwin's make
PACKAGEBASEDIR := /tmp
endif

PACKAGE_BASENAME := $(PACKAGENAME)-$(VERSION)-$(PACKAGE_BASENAME_SUFFIX)

############################################################################
# Variables below are *not* configurable, i.e. don't change them (unless
# you know what you're doing and you're able to accordingly change
# some rules later in this Makefile, that may depend on given variable's
# value).
############################################################################

PACKAGEDIR := $(PACKAGEBASEDIR)/$(PACKAGENAME)

############################################################################
# Change the paths to the correct types
#######################################################################

PACKAGEBASEDIR := $(subst /,$(PATHSEP),$(PACKAGEBASEDIR))
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

# Note that this -opasdoc_console is needed, otherwise FPC >= 1.9.x
# would produce `PasDoc_Console', and this is of course something
# different than 'pasdoc_console' on case-sens filesystem.
#
# This all confusion will be removed after pasdoc 0.9.0 release
# when I will change the dafault .dpr of pasdoc to be pasdoc.dpr
# (the unit PasDoc.pas will have to be renamed then too),
# then all compiler versions, fpc and others, will just produce
# "pasdoc[.exe]" binary. Johannes says to do it after pasdoc 0.9.0
# release [http://sourceforge.net/mailarchive/message.php?msg_id=11455093],
# so I'm waiting.
FPCFLAGS = -FE$(BINDIR) -FU$(OUTDIR) -dRELEASE @pasdoc-fpc.cfg \
  $(FPCUNITDIRS) $(FPCINCLUDEDIRS) -opasdoc_console

# TODO: provide variables and targets to compile by default in debug mode.

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

# Implementation notes: note that zip will add files to existing zip archive,
# if it already exists, so for safety below I'm first `rm -f ...' zip archive,
# then creating it.
makepkg:
	rm -rf $(PACKAGEDIR)
	mkdir $(PACKAGEDIR)
ifdef BINFILES
	mkdir $(PACKAGEDIR)$(PATHSEP)bin
	cp $(BINFILES)$(EXE) $(PACKAGEDIR)$(PATHSEP)bin$(PATHSEP)pasdoc$(EXE)
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
	rm -f $(PACKAGEBASEDIR)$(PATHSEP)$(PACKAGE_BASENAME).zip
	cd $(PACKAGEBASEDIR); zip -r $(PACKAGE_BASENAME).zip $(PACKAGENAME)/*
	mv $(PACKAGEBASEDIR)$(PATHSEP)$(PACKAGE_BASENAME).zip .

makego32: cleanbuild-fpc-go32
	$(MAKE) --no-print-directory \
	  makepkg EXE=.exe SRCFILES= PACKAGE_BASENAME_SUFFIX=go32

makewin32: clean build-fpc-win32
	$(MAKE) --no-print-directory \
	  makepkg EXE=.exe SRCFILES= PACKAGE_BASENAME_SUFFIX=win32

makeos2: clean build-fpc-os2
	$(MAKE) --no-print-directory \
	  makepkg EXE=.exe SRCFILES= PACKAGE_BASENAME_SUFFIX=os2

makebeos: clean build-fpc-beos
	$(MAKE) --no-print-directory \
	  makepkg SRCFILES= PACKAGE_BASENAME_SUFFIX=be-x86

makelinuxm68k: clean build-fpc-linuxm68k
	$(MAKE) --no-print-directory \
	  makepkg SRCFILES= PACKAGE_BASENAME_SUFFIX=linux-m68k

makelinux: clean build-fpc-linux
	$(MAKE) --no-print-directory \
	  makepkg SRCFILES= PACKAGE_BASENAME_SUFFIX=linux-x86

makeamiga: clean build-fpc-amiga
	$(MAKE) --no-print-directory \
	  makepkg SRCFILES= PACKAGE_BASENAME_SUFFIX=amiga-m68k

makesrc: clean
	$(MAKE) --no-print-directory \
	  makepkg BINFILES= PACKAGE_BASENAME_SUFFIX=src

makeall: clean makego32 makewin32 makebeos makelinuxm68k makelinux \
  makeamiga makesrc
