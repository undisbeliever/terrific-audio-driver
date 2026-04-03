
MAKEFLAGS += --no-builtin-rules --warn-undefined-variables
.SUFFIXES:

.PHONY: all clean


all: loader.bin loader.mlb audio-driver.bin audio-driver.mlb


loader.bin loader.mlb &: src/loader.asm src/common-memmap.inc src/registers.inc src/io-commands.inc
	cargo run --release --bin spc700asm -- --mlb loader.mlb -o loader.bin src/loader.asm

audio-driver.bin audio-driver.mlb &: src/audio-driver.asm src/bytecode.inc src/common-memmap.inc src/data-formats.inc src/io-commands.inc src/registers.inc
	cargo run --release --bin spc700asm -- --mlb audio-driver.mlb -o audio-driver.bin src/audio-driver.asm


clean:
	$(RM) loader.bin loader.mlb
	$(RM) audio-driver.bin audio-driver.mlb


