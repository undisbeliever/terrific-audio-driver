
MAKEFLAGS += --no-builtin-rules --warn-undefined-variables
.SUFFIXES:


ROM_PREFIX := ca65-unit-tests
MODULES    := reset break-isr main tad-tests tad-audio


DEPS       := $(wildcard src/*inc ../_common/*.inc) \
	      ../../tad-audio.inc ../../tad-audio.s \
	      ../../../loader.bin ../../../audio-driver.bin ../../../blank-song.bin

LOROM_CFG  := ../_common/lorom256k.cfg
HIROM_CFG  := ../_common/hirom256k.cfg


AS65	?= ca65
LD65	?= ld65
AFLAGS	?= -g


ROMS_      := $(ROM_PREFIX)-lorom $(ROM_PREFIX)-hirom
ROM_FILES  := $(addsuffix .sfc, $(ROMS_)) $(addsuffix .dbg, $(ROMS_)) $(addsuffix -map.txt, $(ROMS_))

LOROM_OBJECTS := $(patsubst %,obj/%-lorom.o, $(MODULES))
HIROM_OBJECTS := $(patsubst %,obj/%-hirom.o, $(MODULES))

PYTHON3      ?= python3 -bb
FIX_CHECKSUM := $(PYTHON3) ../_tools/write-sfc-checksum.py


.PHONY: all clean

all: $(ROM_FILES)

clean:
	$(RM) $(ROM_FILES)
	$(RM) $(LOROM_OBJECTS) $(HIROM_OBJECTS)


$(ROM_PREFIX)-lorom.sfc $(ROM_PREFIX)-lorom.dbg $(ROM_PREFIX)-lorom-map.txt &: $(LOROM_CFG) $(LOROM_OBJECTS)
	$(LD65) -o $(ROM_PREFIX)-lorom.sfc --dbgfile $(ROM_PREFIX)-lorom.dbg -m $(ROM_PREFIX)-lorom-map.txt -C $(LOROM_CFG) $(LOROM_OBJECTS)
	$(FIX_CHECKSUM) --lorom $(ROM_PREFIX)-lorom.sfc


$(ROM_PREFIX)-hirom.sfc $(ROM_PREFIX)-hirom.dbg $(ROM_PREFIX)-hirom-map.txt &: $(HIROM_CFG) $(HIROM_OBJECTS)
	$(LD65) -o $(ROM_PREFIX)-hirom.sfc --dbgfile $(ROM_PREFIX)-hirom.dbg -m $(ROM_PREFIX)-hirom-map.txt -C $(HIROM_CFG) $(HIROM_OBJECTS)
	$(FIX_CHECKSUM) --hirom $(ROM_PREFIX)-hirom.sfc


obj/%-lorom.o: src/%.s $(DEPS)
	$(AS65) $(AFLAGS) -DLOROM $< -o $@

obj/%-hirom.o: src/%.s $(DEPS)
	$(AS65) $(AFLAGS) -DHIROM $< -o $@

obj/%-hirom.o: ../_common/%.s $(DEPS)
	$(AS65) $(AFLAGS) -DHIROM $< -o $@

obj/%-lorom.o: ../_common/%.s $(DEPS)
	$(AS65) $(AFLAGS) -DLOROM $< -o $@

