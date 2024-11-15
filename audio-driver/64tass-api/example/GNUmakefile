
MAKEFLAGS += --no-builtin-rules --warn-undefined-variables
.SUFFIXES:

64TASS    ?= 64tass

TAD_PROJECT ?= ../../../examples/example-project.terrificaudio


BIN_FILES  := 64tass-example-hirom.sfc 64tass-example-lorom.sfc
LABEL_FILES  := $(patsubst %.sfc,%.label, $(BIN_FILES))

GEN_FILES  := gen/audio.inc gen/audio-data-lorom.inc gen/audio-data-lorom.bin gen/audio-data-hirom.inc gen/audio-data-hirom.bin

SRC_FILES := $(wildcard src/*.asm src/*.inc)


# First segment to store audio data in (must end in a decimal number)
TAD_DATA_SECTION = AudioData0

ifneq (1,$(words [$(TAD_PROJECT)]))
  $(error TAD_PROJECT variable cannot contain whitespace)
endif
TAD_PROJECT_DIR := $(dir $(TAD_PROJECT))
TAD_PROJECT_FILES := $(wildcard $(TAD_PROJECT)/* $(TAD_PROJECT)/*/* $(TAD_PROJECT)/*/*/*)

# Location of the tad-compiler binary
ifndef TAD_COMPILER
  TAD_COMPILER := $(firstword $(wildcard ../../../tad-compiler.exe ../../../target/release/tad-compiler.exe ../../../target/release/tad-compiler))

  ifeq ($(TAD_COMPILER), )
    $(error "Cannot find tad-compiler binary.  Please compile a release build of tad-compiler.")
  endif
endif

ifneq (1,$(words [$(TAD_COMPILER)]))
  $(error TAD_COMPILER variable cannot contain whitespace)
endif


.PHONY: all clean

all: $(BIN_FILES)


64tass-example-lorom.sfc: $(SRC_FILES) gen/audio.inc gen/audio-data-lorom.inc gen/audio-data-lorom.bin
	$(64TASS) -a -x -X --nostart -Wall -C -DLOROM=1 -o $@ -l $(basename $@).label src/_example.asm

64tass-example-hirom.sfc: $(SRC_FILES) gen/audio.inc gen/audio-data-hirom.inc gen/audio-data-hirom.bin
	$(64TASS) -a -x -X --nostart -Wall -C -DHIROM=1 -o $@ -l $(basename $@).label src/_example.asm



gen/audio-data-lorom.inc gen/audio-data-lorom.bin &: $(TAD_PROJECT) $(TAD_PROJECT_FILES)
	$(TAD_COMPILER) 64tass-export --lorom --section '$(TAD_DATA_SECTION)' --output-asm gen/audio-data-lorom.inc --output-bin gen/audio-data-lorom.bin $(TAD_PROJECT)

gen/audio-data-hirom.inc gen/audio-data-hirom.bin &: $(TAD_PROJECT) $(TAD_PROJECT_FILES)
	$(TAD_COMPILER) 64tass-export --hirom --section '$(TAD_DATA_SECTION)' --output-asm gen/audio-data-hirom.inc --output-bin gen/audio-data-hirom.bin $(TAD_PROJECT)

gen/audio.inc : $(TAD_PROJECT)
	$(TAD_COMPILER) 64tass-enums -o $@ $(TAD_PROJECT)


clean:
	$(RM) $(BIN_FILES)
	$(RM) $(LABEL_FILES)
	$(RM) $(GEN_FILES)

