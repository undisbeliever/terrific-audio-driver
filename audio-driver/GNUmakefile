
MAKEFLAGS += --no-builtin-rules --warn-undefined-variables
.SUFFIXES:


WIZ_MAIN_FILES    := src/loader.wiz src/audio-driver.wiz
WIZ_INCLUDE_FILES := $(filter-out $(WIZ_MAIN_FILES), $(wildcard src/*.wiz))

WIZ_BIN_OUT       := $(patsubst src/%.wiz,%.wiz.bin,$(WIZ_MAIN_FILES))
WIZ_SYM_OUT       := $(patsubst src/%.wiz,%.sym,$(WIZ_MAIN_FILES))


ifndef WIZ
  WIZ := $(firstword $(wildcard ../wiz/bin/wiz ../bin/wiz/wiz.exe))

  ifeq ($(WIZ), )
    $(error "Cannot find wiz binary.  Please import the wiz git submodule and compile wiz.")
  endif
endif


.PHONY: all clean


all: loader.bin loader.mlb audio-driver.bin audio-driver.mlb $(WIZ_BIN_OUT)
	diff --report-identical-files loader.bin loader.wiz.bin
	diff --report-identical-files audio-driver.bin audio-driver.wiz.bin

loader.bin loader.mlb &: src/loader.asm src/common-memmap.inc src/registers.inc src/io-commands.inc
	cargo run --release --bin spc700asm -- --mlb loader.mlb -o loader.bin src/loader.asm

audio-driver.bin audio-driver.mlb &: src/audio-driver.asm src/bytecode.inc src/common-memmap.inc src/data-formats.inc src/io-commands.inc src/registers.inc
	cargo run --release --bin spc700asm -- --mlb audio-driver.mlb -o audio-driver.bin src/audio-driver.asm

clean:
	$(RM) loader.bin loader.mlb audio-driver.bin audio-driver.mlb
	$(RM) $(WIZ_BIN_OUT) $(WIZ_SYM_OUT)

%.wiz.bin %.wiz.sym &: src/%.wiz $(WIZ_INCLUDE_FILES)
	$(WIZ) --system=spc700 -s wla -o $@ $<

