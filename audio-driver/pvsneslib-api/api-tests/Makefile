
ifeq ($(strip $(PVSNESLIB_HOME)),)
  $(error "PVSNESLIB_HOME is not defined.  Please create a PVSNESLIB_HOME environment variable using this guide: https://github.com/alekmaul/pvsneslib/wiki/Installation")
endif

SRC := ./src
ROMNAME := pv-unit-tests

include ${PVSNESLIB_HOME}/devkitsnes/snes_rules


#---------------------------------------------------------------------------------


.PHONY: all
all: $(ROMNAME).sfc

clean: cleanBuildRes cleanRom cleanGfx


# Dependency graph
$(CFILES:.c=.obj): ../tad-audio.h
tad-audio.obj: ../tad-audio.asm


