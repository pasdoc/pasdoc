include Makefile-autodetect

#######################################################################
# BASE CONFIGURATION
#######################################################################

# The version of the package.
# This must be changed on each version change,
# documented at ReleaseMaking wiki page.
VERSION := 0.9.0

# The name of the package / file name
PACKAGENAME := pasdoc

# Location of units source code.
UNIT_DIRS := ./source/component ./source/console ./source/OptionParser \
  ./source/component/tipue

INCLUDE_DIRS := ./source/component

# Base file to compile
FILE := ./source/console/PasDoc_Console.dpr

# Base directory where binaries will go
BINDIR := bin

# Base directory where libs, units, objects files will go
OUTDIR := lib

# The following is for creating the final package, comment out
# if that particular section is not used.
# BINFILES: Files that will go into the resulting bin directory
# DOCFILES: Files that will go into the resulting docs directory
BINFILES := $(BINDIR)/pasdoc_console
DOCFILES := LICENSE ChangeLog docs/README

PACKAGE_BASENAME := $(PACKAGENAME)-$(VERSION)-$(PACKAGE_BASENAME_SUFFIX)

############################################################################
# Variables below are *not* configurable, i.e. don't change them (unless
# you know what you're doing and you're able to accordingly change
# some rules later in this Makefile, that may depend on given variable's
# value).
############################################################################

PACKAGEDIR := $(PACKAGEBASEDIR)$(PATHSEP)$(PACKAGENAME)

############################################################################
# Change the paths to the correct types
#######################################################################

ifdef OUTDIR
OUTDIR := $(subst /,$(PATHSEP),$(OUTDIR))
endif
ifdef BINDIR
BINDIR := $(subst /,$(PATHSEP),$(BINDIR))
endif
FILE := $(subst /,$(PATHSEP),$(FILE))
UNIT_DIRS := $(subst /,$(PATHSEP),$(UNIT_DIRS))
INCLUDE_DIRS := $(subst /,$(PATHSEP),$(INCLUDE_DIRS))
ifdef BINFILES
BINFILES := $(subst /,$(PATHSEP),$(BINFILES))
endif
ifdef DOCFILES
DOCFILES := $(subst /,$(PATHSEP),$(DOCFILES))
endif

############################################################################
# FreePascal Configuration
############################################################################

# FPC_DEFAULT means "use current os and processor",
# calling just fpc binary on the path
FPC_DEFAULT := fpc

# By default all of below variables are set to $(FPC_DEFAULT), because
# $(FPC_DEFAULT) is the only good default value that can possibly work
# for everyone. You can override them at `make' command-line,
# e.g. if you have different FPC versions and you use them to do cross-compiling.
FPC_WIN32 := $(FPC_DEFAULT)
FPC_GO32 := $(FPC_DEFAULT)
FPC_LINUX_X86 := $(FPC_DEFAULT)
FPC_LINUX_M68K := $(FPC_DEFAULT)
FPC_LINUX_PPC := $(FPC_DEFAULT)
FPC_AMIGA := $(FPC_DEFAULT)
FPC_BEOS := $(FPC_DEFAULT)
FPC_OS2 := $(FPC_DEFAULT)
FPC_FREEBSD_X86 := $(FPC_DEFAULT)

FPC_UNIT_DIRS := $(foreach units,$(UNIT_DIRS),-Fu$(units))
FPC_INCLUDE_DIRS := $(foreach units,$(INCLUDE_DIRS),-Fi$(units))

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
FPC_COMMON_FLAGS := -FE$(BINDIR) -FU$(OUTDIR) @pasdoc-fpc.cfg \
  $(FPC_UNIT_DIRS) $(FPC_INCLUDE_DIRS) -opasdoc_console

FPC_DEBUG_FLAGS := $(FPC_COMMON_FLAGS)

FPC_RELEASE_FLAGS := -dRELEASE $(FPC_COMMON_FLAGS)

############################################################################
# Delphi configuration
############################################################################

# Don't ask me why, but Borland named Delphi/Win32 command-line compiler 
# dcc32 and Delphi/Linux (aka Kylix) as dcc (without 32).
DCC_WIN32 := dcc32
DCC_LINUX := dcc
DCC_UNIT_DIRS := $(foreach units,$(UNIT_DIRS),-U$(units))
DCC_INCLUDE_DIRS := $(foreach units,$(INCLUDE_DIRS),-I$(units))

DCC_COMMON_FLAGS := -E$(BINDIR) -N$(OUTDIR) -L$(OUTDIR) -M -H -W \
  -DCPU86 -DENDIAN_LITTLE $(DCC_UNIT_DIRS) $(DCC_INCLUDE_DIRS)

DCC_DEBUG_FLAGS := -$$Q+ -$$R+ $(DCC_COMMON_FLAGS)

DCC_RELEASE_FLAGS := -$$O+ $(DCC_COMMON_FLAGS)

############################################################################
# Virtual Pascal configuration
############################################################################

# TODO: check this, either remove (if vpc does not work anymore),
# or add to CompilingPasDoc as supported compiler,
# remove hardcoded paths to vpc installation.

VPC := F:\vp21\bin.w32\vpc.exe
VPCRTLWIN32UNITDIR := F:\vp21\units.w32
VPCRTLWIN32LIBDIR := -LF:\vp21\units.w32 -LF:\vp21\lib.w32
VPCRTLOS2UNITDIR := F:\vp21\units.os2
VPCRTLOS2LIBDIR := -LF:\vp21\units.os2 -LF:\vp21\lib.os2
VPCUNITDIRS := $(foreach units,$(UNIT_DIRS),-U$(units))
VPCINCDIRS := $(foreach units,$(INCLUDE_DIRS),-I$(units))
VPCFLAGS := -E$(BINDIR) -M -$$J+ -$$R+ -DCPU86 -DENDIAN_LITTLE -O$(OUTDIR) \
  $(VPCINCDIRS) -L$(OUTDIR)

############################################################################
# Targets to build (and clean after build)
############################################################################

.PHONY: default clean build-fpc-default-debug build-fpc-default \
  build-fpc-win32 build-fpc-go32 \
  build-fpc-linux-x86 build-fpc-linux-m68k build-fpc-amiga build-fpc-beos \
  build-fpc-os2 build-fpc-linux-ppc build-fpc-freebsd-x86 \
  build-delphi-win32 build-delphi-linux-x86 \
  build-vpc-win32 build-vpc-os2 make-dirs

# Default target
default: build-fpc-default-debug

# Clean up the output files.
clean:
ifdef OUTDIR
	rm -Rf $(OUTDIR)
endif
ifdef BINDIR
	rm -Rf $(BINDIR)
endif

# Make sure that $(BINDIR) and $(OUTDIR) exist, create them if necessary.
# This is executed before executing any `build-xxx' target,
# to make sure that compilation works "out of the box".
#
# Always using special directories for $(BINDIR) and $(OUTDIR) is handy
# -- reduces clutter, allows us to easily write `clean' target.
#
# Note that, unless you override $(BINDIR) and $(OUTDIR) at command-line,
# they don't really help you when you have various compilers, for
# various os/arch etc. because $(BINDIR) and $(OUTDIR) are always the same
# anyway, so all compilers reuse the same dirs... In the future various
# build-<compiler>-<os/arch> targets may be tweaked to use different
# $(BINDIR) and $(OUTDIR).
make-dirs:
ifdef OUTDIR
	$(MKDIRPROG) -p $(OUTDIR)
endif
ifdef BINDIR
	$(MKDIRPROG) -p $(BINDIR)
endif

# fpc- build targets

build-fpc-default-debug: make-dirs
	$(FPC_DEFAULT) $(FPC_DEBUG_FLAGS) $(FILE)

build-fpc-default: make-dirs
	$(FPC_DEFAULT) $(FPC_RELEASE_FLAGS) $(FILE)

build-fpc-win32: make-dirs
	$(FPC_WIN32) $(FPC_RELEASE_FLAGS) $(FILE)

build-fpc-go32: make-dirs
	$(FPC_GO32) $(FPC_RELEASE_FLAGS) $(FILE)

build-fpc-linux-x86: make-dirs
	$(FPC_LINUX_X86) $(FPC_RELEASE_FLAGS) $(FILE)

build-fpc-linux-m68k: make-dirs
	$(FPC_LINUX_M68K) $(FPC_RELEASE_FLAGS) $(FILE)

build-fpc-amiga: make-dirs
	$(FPC_AMIGA) $(FPC_RELEASE_FLAGS) $(FILE)

build-fpc-beos: make-dirs
	$(FPC_BEOS) $(FPC_RELEASE_FLAGS) $(FILE)

build-fpc-os2: make-dirs
	$(FPC_OS2) $(FPC_RELEASE_FLAGS) $(FILE)

build-fpc-linux-ppc: make-dirs
	$(FPC_LINUX_PPC) $(FPC_RELEASE_FLAGS) $(FILE)

build-fpc-freebsd-x86: make-dirs
	$(FPC_FREEBSD_X86) $(FPC_RELEASE_FLAGS) $(FILE)

# Delphi/Kylix build targets

# Implementation note: this $(subst...) is needed, otherwise under Windows 
# dcc dumbly prints "file not found" when $(FILE) uses "/" (yes, "/" is allowed
# path separator in all normal Windows programs...) (and $(FILE) uses
# "/" because this is sensible default value for $(PATHSEP), otherwise we would 
# have to do dirty checks to guess whether we're used under Unix or Win32 in 
# this Makefile).
build-delphi-win32: make-dirs
	$(DCC_WIN32) $(DCC_RELEASE_FLAGS) $(subst $(PATHSEP),\\,$(FILE))

build-delphi-linux-x86: make-dirs
	$(DCC_LINUX) $(DCC_RELEASE_FLAGS) $(FILE)

# vpc build targets

build-vpc-win32: make-dirs
	$(VPC) -CW $(VPCFLAGS)  $(VPCRTLWIN32LIBDIR) -U$(VPCRTLWIN32UNITDIR) $(VPCUNITDIRS) $(FILE)

build-vpc-os2: make-dirs
	$(VPC) -CO $(VPCFLAGS)  $(VPCRTLOS2LIBDIR) -U$(VPCRTLOS2UNITDIR) $(VPCUNITDIRS) $(FILE)

############################################################################
# Help targets
############################################################################

.PHONY: help

help:
	@echo "Available targets of this Makefile:"
	@echo 
	@echo "Compiling:"
	@echo
	@echo "  default, build-fpc-default-debug:"
	@echo "    Compile debug version with FPC. This is the default target."
	@echo
	@echo "  build-fpc-default:"
	@echo "    Compile release version with FPC."
	@echo
	@echo "  build-<compiler>-<os/arch>:"
	@echo "    Compile release version with given compiler for given OS"
	@echo "    and architecture. Available values for <compiler> are:"
	@echo "      fpc"
	@echo "      delphi"
	@echo "      vpc"
	@echo "    Available values for <os/arch> are:"
	@echo "      win32"
	@echo "      go32"
	@echo "      linux-x86"
	@echo "      linux-m68k"
	@echo "      amiga"
	@echo "      beos"
	@echo "      os2"
	@echo "      linux-ppc"
	@echo "      freebsd-x86"
	@echo "    Of course, not all combinations of <compiler> and <os/arch>"
	@echo "    are available..."
	@echo
	@echo "    Note that in case of FPC, these targets assume that variable"
	@echo "    FPC_<os/arch> points to a compiler that produces"
	@echo "    a binary for given <os/arch>. So if you want to cross-compile"
	@echo "    with FPC, make sure that these variables are correctly set."
	@echo
	@echo "  clean:"
	@echo "    Clean files produced during compilation."
	@echo
	@echo "Archiving a release:"
	@echo
	@echo "  dist-<os/arch>:"
	@echo "    This calls \"clean\", then \"build-<compiler>-<os/arch>\""
	@echo "    (using the preferred <compiler> for making a release on"
	@echo "    this <os/arch> -- currently, always FPC)"
	@echo "    and then makes a release archive for given <os/arch>."
	@echo
	@echo "  dist-src:"
	@echo "    This creates source archive, by checking out whole pasdoc sources"
	@echo "    from pasdoc public CVS. It checks out using cvs tag PASDOC_x_y_z"
	@echo "    (where x_y_z is derived from VERSION variable in this Makefile)."
	@echo "    This way user can easily update downloaded sources tree by"
	@echo "    simple \"cvs -z3 update -Pd\"' command."
	@echo
	@echo "    Note: before using this target for the 1st time, you should"
	@echo "    manually do cvs...login to pasdoc public CVS"
	@echo "    (see [http://sourceforge.net/cvs/?group_id=4213])"

############################################################################
# Targets to make distribution archives
#
# There are some general targets here, and there are targets
# that build and archive for particular target. Note that they assume
# that according build-xxx target really produces a pasdoc binary
# that works under xxx target. If you want to use cross-compiling
# to build releases archives, you must make sure that proper FPC_<target-name>
# variable is properly set.
#
# Note that dist targets generally try to use the "most common"
# archive format for given target. E.g. for Unices this is tar.gz,
# for Win32/DOS this is zip. E.g. there are no problems with creating
# zip for Unices, it's just a common practice to use tar.gz instead
# of zip for Unices.
#
############################################################################

.PHONY: dist-prepare dist-zip dist-tar-gz dist-go32 dist-win32 dist-os2 \
  dist-beos dist-linux-m68k dist-linux-x86 dist-amiga dist-freebsd-x86 \
  dist-src dist-all

# This target creates and fills directory $(PACKAGEDIR)
# (it's *always* the subdirectory $(PACKAGENAME) inside $(PACKAGEBASEDIR)).
# Use this to prepare file tree before archiving --- the only remaining
# thing after executing this target is to archive $(PACKAGEDIR).
dist-prepare:
	rm -rf $(PACKAGEDIR)
	$(MKDIRPROG) $(PACKAGEDIR)
ifdef BINFILES
	$(MKDIRPROG) $(PACKAGEDIR)$(PATHSEP)bin
	cp $(BINFILES)$(EXE) $(PACKAGEDIR)$(PATHSEP)bin$(PATHSEP)pasdoc$(EXE)
endif
ifdef DOCFILES
	$(MKDIRPROG) $(PACKAGEDIR)$(PATHSEP)docs
	cp -R $(DOCFILES) $(PACKAGEDIR)$(PATHSEP)docs
endif

# This target archives distribution into a zip file.
#
# Implementation notes: note that zip will add files to existing zip archive,
# if it already exists, so for safety below I'm first `rm -f ...' zip archive,
# then creating it.
dist-zip: dist-prepare
	rm -f $(PACKAGEBASEDIR)$(PATHSEP)$(PACKAGE_BASENAME).zip
	cd $(PACKAGEBASEDIR); zip -r $(PACKAGE_BASENAME).zip $(PACKAGENAME)/*
	mv $(PACKAGEBASEDIR)$(PATHSEP)$(PACKAGE_BASENAME).zip .

# This target archives distribution into a tar.gz file.
dist-tar-gz: dist-prepare
	cd $(PACKAGEBASEDIR); tar czvf $(PACKAGE_BASENAME).tar.gz $(PACKAGENAME)/
	mv $(PACKAGEBASEDIR)$(PATHSEP)$(PACKAGE_BASENAME).tar.gz .

dist-go32: clean build-fpc-go32
	$(MAKE) --no-print-directory \
	  dist-zip EXE=.exe PACKAGE_BASENAME_SUFFIX=go32

dist-win32: clean build-fpc-win32
	$(MAKE) --no-print-directory \
	  dist-zip EXE=.exe PACKAGE_BASENAME_SUFFIX=win32

dist-os2: clean build-fpc-os2
	$(MAKE) --no-print-directory \
	  dist-zip EXE=.exe PACKAGE_BASENAME_SUFFIX=os2

dist-beos: clean build-fpc-beos
	$(MAKE) --no-print-directory \
	  dist-zip PACKAGE_BASENAME_SUFFIX=be-x86

dist-linux-m68k: clean build-fpc-linux-m68k
	$(MAKE) --no-print-directory \
	  dist-tar-gz PACKAGE_BASENAME_SUFFIX=linux-m68k

dist-linux-x86: clean build-fpc-linux-x86
	$(MAKE) --no-print-directory \
	  dist-tar-gz PACKAGE_BASENAME_SUFFIX=linux-x86

dist-amiga: clean build-fpc-amiga
	$(MAKE) --no-print-directory \
	  dist-zip PACKAGE_BASENAME_SUFFIX=amiga-m68k

dist-linux-ppc: clean build-fpc-linux-ppc
	$(MAKE) --no-print-directory \
	  dist-tar-gz PACKAGE_BASENAME_SUFFIX=linux-ppc

dist-freebsd-x86: clean build-fpc-freebsd-x86
	$(MAKE) --no-print-directory \
	  dist-tar-gz PACKAGE_BASENAME_SUFFIX=freebsd-x86

SOURCE_PACKAGE_BASENAME := $(PACKAGENAME)-$(VERSION)-src

dist-src:
	rm -Rf $(PACKAGEBASEDIR)$(PATHSEP)PasDoc-0.8/
	cd $(PACKAGEBASEDIR); \
	  cvs -z3 -d:pserver:anonymous@cvs.sourceforge.net:/cvsroot/pasdoc \
	    co -P -r PASDOC_$(subst .,_,$(VERSION)) PasDoc-0.8
	cd $(PACKAGEBASEDIR); tar czvf $(SOURCE_PACKAGE_BASENAME).tar.gz PasDoc-0.8/
	mv $(PACKAGEBASEDIR)$(PATHSEP)$(SOURCE_PACKAGE_BASENAME).tar.gz .

dist-all: dist-go32 dist-win32 dist-beos dist-linux-m68k dist-linux-x86 \
  dist-amiga dist-linux-ppc dist-freebsd-x86 dist-src
