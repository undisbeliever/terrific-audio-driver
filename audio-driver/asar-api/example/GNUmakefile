
MAKEFLAGS += --no-builtin-rules --warn-undefined-variables
.DELETE_ON_ERROR:
.SUFFIXES:

ASAR      ?= asar
TAD_PROJECT ?= ../../../examples/example-project.terrificaudio


INC_FILES := $(wildcard src/*.inc)


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

all: asar-example-hirom.sfc asar-example-lorom.sfc


asar-example-lorom.sfc: $(INC_FILES) gen/audio.inc gen/audio-data-lorom.inc gen/audio-data-lorom.bin
	$(ASAR) -werror --no-title-check --symbols=wla src/_example-lorom.asm asar-example-lorom.sfc 

asar-example-hirom.sfc: $(INC_FILES) gen/audio.inc gen/audio-data-hirom.inc gen/audio-data-hirom.bin
	$(ASAR) -werror --no-title-check --symbols=wla src/_example-hirom.asm asar-example-hirom.sfc 



gen/audio-data-lorom.inc gen/audio-data-lorom.bin &: $(TAD_PROJECT) $(TAD_PROJECT_FILES)
	$(TAD_COMPILER) asar-export --lorom --output-asm gen/audio-data-lorom.inc --output-bin gen/audio-data-lorom.bin $(TAD_PROJECT)

gen/audio-data-hirom.inc gen/audio-data-hirom.bin &: $(TAD_PROJECT) $(TAD_PROJECT_FILES)
	$(TAD_COMPILER) asar-export --hirom --output-asm gen/audio-data-hirom.inc --output-bin gen/audio-data-hirom.bin $(TAD_PROJECT)

gen/audio.inc : $(TAD_PROJECT)
	$(TAD_COMPILER) asar-enums -o $@ $(TAD_PROJECT)


clean:
	$(RM) asar-example-lorom.sfc  asar-example-lorom.sym
	$(RM) asar-example-hirom.sfc  asar-example-hirom.sym
	$(RM) gen/audio.inc gen/audio-data-lorom.inc gen/audio-data-lorom.bin gen/audio-data-hirom.inc gen/audio-data-hirom.bin

