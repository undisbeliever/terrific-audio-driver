
MAKEFLAGS += --no-builtin-rules --warn-undefined-variables
.SUFFIXES:


ROM_PREFIX  := ca65-sound-test

TAD_PROJECT ?= ../../../../examples/example-project.terrificaudio

# Must be HIROM or LOROM
MAPPING	    ?= LOROM

# Location of the tad-compiler binary
TAD_COMPILER ?= ../../../../target/release/tad-compiler

# First segment to store audio data in (must end in a decimal number)
TAD_DATA_SEGMENT ?= RODATA2

MODULES    := reset break-isr main dma-forceblank text-buffer tad-audio audio-data

GEN_FILES  := gen/audio.inc gen/audio-data.s gen/audio-data.bin

DEPS       := $(wildcard src/*.inc) ../../tad-audio.inc gen/audio.inc


AS65	?= ca65
LD65	?= ld65
AFLAGS	?= -g

PYTHON3      ?= python3 -bb
FIX_CHECKSUM := $(PYTHON3) ../_tools/write-sfc-checksum.py


ifeq ($(MAPPING), LOROM)
  MAPPING_CFG := ../_common/lorom256k.cfg
  MAPPING_ARG := --lorom
else ifeq ($(MAPPING), HIROM)
  MAPPING_CFG := ../_common/hirom256k.cfg
  MAPPING_ARG := --hirom
else
  $(error Unknown MAPPING value)
endif

ROM_FILES  := $(addsuffix .sfc, $(ROM_PREFIX)) $(addsuffix .dbg, $(ROM_PREFIX)) $(addsuffix -map.txt, $(ROM_PREFIX))

OBJECTS := $(patsubst %,obj/%.o, $(MODULES))

TAD_PROJECT_DIR := $(dir $(TAD_PROJECT))
TAD_PROJECT_FILES := $(wildcard $(TAD_PROJECT)/* $(TAD_PROJECT)/*/* $(TAD_PROJECT)/*/*/*)


.PHONY: all clean

all: $(ROM_FILES)


clean:
	$(RM) $(ROM_FILES)
	$(RM) $(OBJECTS)
	$(RM) $(GEN_FILES)


$(ROM_PREFIX).sfc $(ROM_PREFIX).dbg $(ROM_PREFIX)-map.txt &: $(MAPPING_CFG) $(OBJECTS)
	$(LD65) -o $(ROM_PREFIX).sfc --dbgfile $(ROM_PREFIX).dbg -m $(ROM_PREFIX)-map.txt -C $(MAPPING_CFG) $(OBJECTS)
	$(FIX_CHECKSUM) $(MAPPING_ARG) $(ROM_PREFIX).sfc


obj/%.o: src/%.s $(DEPS)
	$(AS65) $(AFLAGS) -D$(MAPPING) -o $@ $<

obj/%.o: ../_common/%.s $(DEPS)
	$(AS65) $(AFLAGS) -D$(MAPPING) -o $@ $<

obj/tad-audio.o: ../../tad-audio.s
	$(AS65) $(AFLAGS) -D$(MAPPING) -o $@ $<

obj/audio-data.o: gen/audio-data.s gen/audio-data.bin
	$(AS65) $(AFLAGS) -D$(MAPPING) -o $@ $<


gen/audio-data.s gen/audio-data.bin &: $(TAD_PROJECT) $(TAD_PROJECT_FILES)
	$(TAD_COMPILER) ca65-export $(MAPPING_ARG) --segment '$(TAD_DATA_SEGMENT)' --output-asm gen/audio-data.s --output-bin gen/audio-data.bin $(TAD_PROJECT)

gen/audio.inc : $(TAD_PROJECT)
	$(TAD_COMPILER) ca65-enums -o $@ $(TAD_PROJECT)


