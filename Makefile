# Build Pasdoc.
# Assumptions:
# You have basic Unix tools.
# On Windows, we assume you have cygpath from Cygwin / MSys2 etc.,
# you have mkdir etc.

#######################################################################
# BASE CONFIGURATION
#######################################################################

# The version of the package.
# This must be changed on each version bump,
# as documented on https://pasdoc.github.io/ReleaseMaking .
VERSION := 0.17.0.snapshot

# The name of the package / file name
PACKAGENAME := pasdoc

# Location of units source code.
UNIT_DIRS := ./source/component ./source/console \
	./source/component/tipue

INCLUDE_DIRS := ./source/component ./source/component/images

# Base file to compile
FILE := ./source/console/pasdoc.dpr

# Base directory where binaries will go.
# May be set on make command-line too.
ifndef BINDIR
BINDIR := bin
endif

# Base directory where libs, units, objects files will go.
# May be set on make command-line too.
ifndef OUTDIR
OUTDIR := lib
endif

# OS and CPU (in FPC naming conventions) to build for.
# May be set on make command-line too (potentially to cause cross-compilation).
ifndef FPC_OS
FPC_OS := $(shell fpc -iTO)
endif
ifndef FPC_CPU
FPC_CPU := $(shell fpc -iTP)
endif

# The following is for creating the final package, comment out
# if that particular section is not used.
# BINFILES: Files that will go into the resulting bin directory
# DOCFILES: Files that will go into the resulting docs directory
BINFILES := $(BINDIR)/pasdoc$(EXE) \
	$(BINDIR)/pascal_pre_proc$(EXE) \
	$(BINDIR)/file_to_pascal_string$(EXE) \
	$(BINDIR)/file_to_pascal_data$(EXE)
DOCFILES := LICENSE ChangeLog.md README.md

PACKAGE_BASENAME := $(PACKAGENAME)-$(VERSION)-$(PACKAGE_BASENAME_SUFFIX)

MKDIRPROG := mkdir

############################################################################
# Variables below are *not* configurable, i.e. don't change them (unless
# you know what you're doing and you're able to accordingly change
# some rules later in this Makefile, that may depend on given variable's
# value).
############################################################################

###########################################################################
# Calculate $(PACKAGEBASEDIR), temporary directory used for preparing archives.
# Calculate $(PACKAGEDIR), which is a subdirectory of above with pasdoc/ .
###########################################################################

ifdef TEMP
PACKAGEBASEDIR := $(TEMP)

# Assume we have cygpath (from Cygwin, MSys2 etc.) on Windows.
ifeq ($(OS),Windows_NT)
# Make $(PACKAGEBASEDIR) directory as native Windows path
# because we may use it with some non-Cygwin tools.
#
# Also, using "--mixed" instead of "--windows", we get path with forward slashes,
# easier to use in Makefile due to uncertain how to quote things reliably in Makefile.
PACKAGEBASEDIR := $(shell cygpath --mixed $(PACKAGEBASEDIR))
endif

else
# /tmp is a good guess for Unix.
# Use a subdir in /tmp/, to allow using this from multiple user accounts
# (especially important since we may not clean after ourselves properly,
# so with simple /tmp/ other user would not have permissions to remove /tmp/pasdoc)
PACKAGEBASEDIR := /tmp/pasdoc-$(USER)
endif

PACKAGEDIR := $(PACKAGEBASEDIR)/$(PACKAGENAME)

############################################################################
# FreePascal Configuration
############################################################################

# FPC_DEFAULT means "use current os and processor",
# calling just fpc binary on the path
FPC_DEFAULT := fpc

FPC_UNIT_DIRS := $(foreach units,$(UNIT_DIRS),-Fu$(units))
FPC_INCLUDE_DIRS := $(foreach units,$(INCLUDE_DIRS),-Fi$(units))

FPC_COMMON_FLAGS := -FE$(BINDIR) -FU$(OUTDIR) @pasdoc-fpc.cfg \
	$(FPC_UNIT_DIRS) $(FPC_INCLUDE_DIRS) \
	-T$(FPC_OS) -P$(FPC_CPU)

FPC_DEBUG_FLAGS := $(FPC_COMMON_FLAGS)
ifdef CHECK_MEM_LEAK
	FPC_DEBUG_FLAGS := -dCHECK_MEM_LEAK $(FPC_DEBUG_FLAGS)
endif

FPC_RELEASE_FLAGS := -dRELEASE $(FPC_COMMON_FLAGS)

############################################################################
# Delphi configuration
############################################################################

# It seems Borland named Delphi/Win32 command-line compiler
# dcc32 and Delphi/Linux (aka Kylix) as dcc (without 32).
DCC_WIN32 := dcc32
DCC_LINUX := dcc

DCC_UNIT_DIRS := $(foreach units,$(UNIT_DIRS),-U$(units))
DCC_INCLUDE_DIRS := $(foreach units,$(INCLUDE_DIRS),-I$(units))

# Command-line dcc prints non-errorlines while it works,
# I guess that it was meant to somehow indicate
# compilation progress, although it's rather confusing and hides
# meaningfull error/warning lines. That's why we pass -Q below.

DCC_COMMON_FLAGS := -E$(BINDIR) -N$(OUTDIR) -L$(OUTDIR) -M -H -W -Q \
	-DCPU86 -DENDIAN_LITTLE $(DCC_UNIT_DIRS) $(DCC_INCLUDE_DIRS)

DCC_DEBUG_FLAGS := -$$Q+ -$$R+ $(DCC_COMMON_FLAGS)

DCC_RELEASE_FLAGS := -$$O+ $(DCC_COMMON_FLAGS)

############################################################################
# Building (and cleaning after building)
############################################################################

# Default target
.PHONY: default
default: build-fpc-debug

# Clean up the output files.
.PHONY: clean
clean:
	rm -Rf source/console/pasdoc.compiled \
				 source/packages/lazarus/lib/ \
				 source/gui/pasdoc_gui.compiled \
				 source/gui/pasdoc_gui \
				 source/gui/pasdoc_gui.exe \
				 source/gui/pasdoc_gui.app \
				 source/gui/*.o \
				 source/gui/*.or \
				 source/gui/*.ppu \
				 source/gui/*.res
	$(MAKE) clean -C tests/
	$(MAKE) clean -C source/autodoc/
ifdef OUTDIR
	rm -Rf $(OUTDIR)
	mkdir $(OUTDIR)
	touch $(OUTDIR)/.gitkeep
endif
ifdef BINDIR
	rm -Rf $(BINDIR)
endif

# Make sure that $(BINDIR) and $(OUTDIR) exist, create them if necessary.
# This is executed before executing any `build-xxx' target,
# to make sure that compilation works "out of the box".
#
# Always using special directories for $(BINDIR) and $(OUTDIR) is handy.
# Reduces clutter, allows us to easily write `clean' target.
#
# Note that these BINDIR and OUTDIR are not automatically OS/CPU specific.
# All OS/CPU builds reuse the same directories, by default.
.PHONY: make-dirs
make-dirs:
ifdef OUTDIR
	$(MKDIRPROG) -p $(OUTDIR)
endif
ifdef BINDIR
	$(MKDIRPROG) -p $(BINDIR)
endif

# fpc- build targets
.PHONY: build-fpc-debug
build-fpc-debug: make-dirs
	$(FPC_DEFAULT) $(FPC_DEBUG_FLAGS) $(FILE)

.PHONY: build-fpc-release
build-fpc-release: make-dirs
	$(FPC_DEFAULT) $(FPC_RELEASE_FLAGS) $(FILE)

# Deprecated (defined only temporarily for backward compatibility)
# name for build-fpc-default .
.PHONY: build-fpc-default
build-fpc-default: build-fpc-release

# Delphi/Kylix build targets

# Implementation note: this $(subst...) is needed, otherwise under Windows
# dcc prints "file not found" when $(FILE) uses "/" (even though "/" is allowed
# path separator in all normal Windows programs).
.PHONY: build-delphi-win32
build-delphi-win32: make-dirs
	$(DCC_WIN32) $(DCC_RELEASE_FLAGS) $(subst /,\\,$(FILE))

.PHONY: build-delphi-linux-x86
build-delphi-linux-x86: make-dirs
	$(DCC_LINUX) $(DCC_RELEASE_FLAGS) $(FILE)

.PHONY: build-tools
build-tools: make-dirs
	$(FPC_DEFAULT) $(FPC_DEBUG_FLAGS) ./source/tools/pascal_pre_proc.dpr
	$(FPC_DEFAULT) $(FPC_DEBUG_FLAGS) ./source/tools/file_to_pascal_data.dpr
	$(FPC_DEFAULT) $(FPC_DEBUG_FLAGS) ./source/tools/file_to_pascal_string.dpr

.PHONY: build-gui
build-gui:
	if ! lazbuild $(LAZBUILD_OPTIONS) source/packages/lazarus/pasdoc_package.lpk; then \
		echo 'lazbuild sometimes fails with Access Violation, retrying' && \
		rm -Rf source/packages/lazarus/lib/ && \
		lazbuild $(LAZBUILD_OPTIONS) source/packages/lazarus/pasdoc_package.lpk; \
	fi
	if ! lazbuild $(LAZBUILD_OPTIONS) source/gui/pasdoc_gui.lpi; then \
		echo 'lazbuild sometimes fails with Access Violation, retrying' && \
		rm -Rf source/gui/lib/ && \
		lazbuild $(LAZBUILD_OPTIONS) source/gui/pasdoc_gui.lpi; \
	fi
	strip source/gui/pasdoc_gui$(EXE)

.PHONY: tests-fpcunit
tests-fpcunit: make-dirs
	$(FPC_DEFAULT) $(FPC_DEBUG_FLAGS) ./tests/fpcunit/test_pasdoc.lpr
	bin/test_pasdoc -a

.PHONY: tests
tests:
	cd tests/ && ./run_all_tests.sh

############################################################################
# Help targets
############################################################################

.PHONY: help
help:
	@echo "Available targets of this Makefile:"
	@echo
	@echo "Compiling:"
	@echo
	@echo "  default, build-fpc-debug:"
	@echo "    Compile debug version with FPC. This is the default target."
	@echo
	@echo "  build-fpc-release:"
	@echo "    Compile release version with FPC."
	@echo
	@echo "    You can pass FPC_OS=xxx FPC_CPU=xxx to cause cross-compilation"
	@echo "    e.g.: 'make build-fpc-release FPC_OS=win64 FPC_CPU=x86_64'"
	@echo
	@echo "  build-gui:"
	@echo "    Compile pasdoc_gui with lazbuild (Lazarus build tool)."
	@echo
	@echo "  clean:"
	@echo "    Clean files produced during compilation."
	@echo
	@echo "Archiving a release:"
	@echo
	@echo "  dist-<os/arch>:"
	@echo "    This calls \"clean\", then \"build-fpc-release\","
	@echo "    and then makes a release archive for given <os/arch>."
	@echo
	@echo "  dist-src:"
	@echo "    This creates source archive for the current sources."

.PHONY: version
version:
	@echo $(VERSION)

.PHONY: tag
tag:
	git log --pretty=oneline HEAD^..HEAD
	git tag -a v$(VERSION) -m "Tagging the $(VERSION) version of PasDoc."
	git push origin --tags

############################################################################
# Targets to make release archives
#
# Some general targets are present here, and targets
# that build and archive for particular platform.
#
# Note that dist targets use the most common archive format for given platform.
# E.g. for Unices this is tar.gz, for Win32/DOS this is zip.
# There would be no problems with creating zip archives under Unices
# (or tar.gz under Win32/DOS), but it's just more common
# to use tar.gz under Unices.
############################################################################

# Do common distribution steps.
# - cleanup
# - build pasdoc
# - build tools (pascal_pre_proc, file_to_pascal_string, file_to_pascal_data)
# - build pasdoc_gui (if ADD_PASDOC_GUI is set)
# - create and fill $(PACKAGEDIR)
#   (it's *always* the subdirectory $(PACKAGENAME) inside $(PACKAGEBASEDIR)).
# The only remaining thing after executing this target is to archive $(PACKAGEDIR).
.PHONY: dist-common
dist-common:
	$(MAKE) clean
	$(MAKE) build-fpc-release
	rm -rf $(PACKAGEDIR)
	$(MKDIRPROG) -p $(PACKAGEDIR)
	$(MAKE) build-tools
ifdef BINFILES
	$(MKDIRPROG) $(PACKAGEDIR)/bin
	cp $(BINFILES) $(PACKAGEDIR)/bin/
endif
ifdef DOCFILES
	$(MKDIRPROG) $(PACKAGEDIR)/docs
	cp -R $(DOCFILES) $(PACKAGEDIR)/docs
endif
ifdef ADD_PASDOC_GUI
	$(MAKE) build-gui
ifdef PASDOC_GUI_BUNDLE
	rm -Rf source/gui/pasdoc_gui.app/
	cd source/gui/ && macos/create_bundle.sh $(VERSION)
	cp -R source/gui/pasdoc_gui.app $(PACKAGEDIR)/bin/
else
	cp source/gui/pasdoc_gui$(EXE) $(PACKAGEDIR)/bin/
endif
endif

# This target archives distribution into a zip file.
#
# Implementation notes: note that zip will add files to existing zip archive,
# if it already exists, so for safety below I'm first `rm -f ...' zip archive,
# then creating it.
.PHONY: dist-zip
dist-zip:
	$(MAKE) dist-common
	rm -f $(PACKAGEBASEDIR)/$(PACKAGE_BASENAME).zip
	cd $(PACKAGEBASEDIR) && zip -r $(PACKAGE_BASENAME).zip $(PACKAGENAME)/*
	mv $(PACKAGEBASEDIR)/$(PACKAGE_BASENAME).zip .

# This target archives distribution into a tar.gz file.
.PHONY: dist-tar-gz
dist-tar-gz:
	$(MAKE) dist-common
	cd $(PACKAGEBASEDIR) && tar czvf $(PACKAGE_BASENAME).tar.gz $(PACKAGENAME)/
	mv $(PACKAGEBASEDIR)/$(PACKAGE_BASENAME).tar.gz .

# Targets specific to particular platform (OS/CPU)

.PHONY: dist-win32
dist-win32:
	$(MAKE) dist-zip \
		FPC_OS=win32 FPC_CPU=i386 \
		EXE=.exe PACKAGE_BASENAME_SUFFIX=win32 \
		ADD_PASDOC_GUI=t LAZBUILD_OPTIONS='--operating-system=win32 --cpu=i386'

.PHONY: dist-win64
dist-win64:
	$(MAKE) dist-zip \
		FPC_OS=win64 FPC_CPU=x86_64 \
		EXE=.exe PACKAGE_BASENAME_SUFFIX=win64 \
		ADD_PASDOC_GUI=t LAZBUILD_OPTIONS='--operating-system=win64 --cpu=x86_64'

.PHONY: dist-os2
dist-os2:
	$(MAKE) dist-zip \
	  FPC_OS=os2 FPC_CPU=i386 \
		EXE=.exe PACKAGE_BASENAME_SUFFIX=os2

.PHONY: dist-beos
dist-beos:
	$(MAKE) dist-zip \
	  FPC_OS=beos FPC_CPU=i386 \
		PACKAGE_BASENAME_SUFFIX=be-x86

.PHONY: dist-linux-m68k
dist-linux-m68k:
	$(MAKE) dist-tar-gz \
	  FPC_OS=linux FPC_CPU=m68k \
		PACKAGE_BASENAME_SUFFIX=linux-m68k

.PHONY: dist-linux-x86
dist-linux-x86:
	$(MAKE) dist-tar-gz \
	  FPC_OS=linux FPC_CPU=i386 \
	  PACKAGE_BASENAME_SUFFIX=linux-x86 \
		ADD_PASDOC_GUI=t LAZBUILD_OPTIONS='--operating-system=linux --cpu=i386'

.PHONY: dist-linux-x86_64
dist-linux-x86_64:
	$(MAKE) dist-tar-gz \
	  FPC_OS=linux FPC_CPU=x86_64 \
	  PACKAGE_BASENAME_SUFFIX=linux-x86_64 \
		ADD_PASDOC_GUI=t LAZBUILD_OPTIONS='--operating-system=linux --cpu=x86_64'

.PHONY: dist-linux-arm
dist-linux-arm:
	$(MAKE) dist-tar-gz \
	  FPC_OS=linux FPC_CPU=arm \
		PACKAGE_BASENAME_SUFFIX=linux-arm \
		ADD_PASDOC_GUI=t LAZBUILD_OPTIONS='--operating-system=linux --cpu=arm'

.PHONY: dist-linux-aarch64
dist-linux-aarch64:
	$(MAKE) dist-tar-gz \
	  FPC_OS=linux FPC_CPU=aarch64 \
		PACKAGE_BASENAME_SUFFIX=linux-aarch64 \
		ADD_PASDOC_GUI=t LAZBUILD_OPTIONS='--operating-system=linux --cpu=aarch64'

.PHONY: dist-linux-ppc
dist-linux-ppc:
	$(MAKE) dist-tar-gz \
	  FPC_OS=linux FPC_CPU=powerpc \
	  PACKAGE_BASENAME_SUFFIX=linux-ppc

.PHONY: dist-darwin-x86_64
dist-darwin-x86_64:
	$(MAKE) dist-zip \
	  FPC_OS=darwin FPC_CPU=x86_64 \
		PACKAGE_BASENAME_SUFFIX=darwin-x86_64 \
		ADD_PASDOC_GUI=t PASDOC_GUI_BUNDLE=t LAZBUILD_OPTIONS='--operating-system=darwin --cpu=x86_64 --widgetset=cocoa'

.PHONY: dist-darwin-aarch64
dist-darwin-aarch64:
	$(MAKE) dist-zip \
	  FPC_OS=darwin FPC_CPU=aarch64 \
	  PACKAGE_BASENAME_SUFFIX=darwin-aarch64 \
		ADD_PASDOC_GUI=t PASDOC_GUI_BUNDLE=t LAZBUILD_OPTIONS='--operating-system=darwin --cpu=aarch64 --widgetset=cocoa'

SOURCE_PACKAGE_BASENAME := $(PACKAGENAME)-$(VERSION)-src

.PHONY: dist-src
dist-src:
	rm -Rf /tmp/pasdoc-src-temp
	mkdir -p /tmp/pasdoc-src-temp
	cp -R . /tmp/pasdoc-src-temp/pasdoc
	$(MAKE) clean -C /tmp/pasdoc-src-temp/pasdoc
# Exclude .cge-jenkins-lazarus and .cache that may happen because Jenkins
# runs this with Docker container with $HOME set to pasdoc dir.
	cd /tmp/pasdoc-src-temp/ && \
		zip -r $(SOURCE_PACKAGE_BASENAME).zip \
		--exclude='*/.git/*' \
		--exclude='*/.cge-jenkins-lazarus/*' \
		--exclude='*/.cache/*' \
		--exclude='*.tar.gz' \
		--exclude='*.zip' \
		--exclude='*~' \
		pasdoc
	mv /tmp/pasdoc-src-temp/$(SOURCE_PACKAGE_BASENAME).zip .
