
ifeq ($(strip $(PVSNESLIB_HOME)),)
  $(error "PVSNESLIB_HOME is not defined.  Please create a PVSNESLIB_HOME environment variable using this guide: https://github.com/alekmaul/pvsneslib/wiki/Installation")
endif

SRC	:= ./src
ROMNAME := pv-sound-test

# The Terrific Audio Driver project to use
# (This makefile assumes all audio resources are in the directory of the .terrificaudio file)
TAD_PROJECT ?= ../../../examples/example-project.terrificaudio

# Memory map
# (set to 1 to use HIROM)
# This variable MUST MATCH hdr.asm
HIROM ?= 0


include ${PVSNESLIB_HOME}/devkitsnes/snes_rules


#---------------------------------------------------------------------------------


.PHONY: all
all: $(ROMNAME).sfc

clean: cleanBuildRes cleanRom cleanTadAudio cleanResources


#---------------------------------------------------------------------------------


RESOURCES := tiles/font-2bpp.pic tiles/font-2bpp.pal

tiles/%-2bpp.pic tiles/%-2bpp.pal &: tiles/%-2bpp.png
	$(GFXCONV) -R -s 8 -u 4 -p -t png -i '$<'


# dependencies
data.obj: $(RESOURCES)

.PHONY: cleanResources
cleanResources:
	$(RM) $(RESOURCES)


#---------------------------------------------------------------------------------


# Location of the tad-compiler binary
ifndef TAD_COMPILER
  TAD_COMPILER := $(firstword $(wildcard ../../../tad-compiler.exe ../../../target/release/tad-compiler.exe ../../../target/release/tad-compiler))

  ifeq ($(TAD_COMPILER), )
    $(error "Cannot find tad-compiler binary.  Please compile a release build of tad-compiler or set the TAD_COMPILER variable.")
  endif
endif


TAD_PROJECT_DIR := $(dir $(TAD_PROJECT))
TAD_PROJECT_FILES := $(wildcard $(TAD_PROJECT)/* $(TAD_PROJECT)/*/* $(TAD_PROJECT)/*/*/*)


ifeq ($(HIROM),1)
  TAD_MAPPING_ARG := --hirom
else
  TAD_MAPPING_ARG := --lorom
endif


gen/audio-data.asm gen/audio-data.bin &: $(TAD_PROJECT) $(TAD_PROJECT_FILES)
	$(TAD_COMPILER) pv-export $(TAD_MAPPING_ARG) --output-asm gen/audio-data.asm --output-bin gen/audio-data.bin $(TAD_PROJECT)

gen/audio.h : $(TAD_PROJECT)
	$(TAD_COMPILER) pv-enums -o $@ $(TAD_PROJECT)


.PHONY: cleanTadAudio
cleanTadAudio:
	$(RM) gen/audio-data.asm gen/audio-data.bin gen/audio.h


# Dependency graph
data.obj: gen/audio-data.asm gen/audio-data.bin
tad-audio.obj: ../tad-audio.asm
$(CFILES:.c=.obj): ../tad-audio.h gen/audio.h


