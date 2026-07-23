
MAKEFLAGS += --no-builtin-rules --warn-undefined-variables
.SUFFIXES:

.PHONY: all clean


all: audio-driver.bin audio-driver.mlb


audio-driver.bin audio-driver.mlb &: src/audio-driver.asm src/bytecode.inc src/data-formats.inc src/io-commands.inc src/loader.inc src/memmap.inc src/registers.inc src/variables.inc
	cargo run --release --bin spc700asm -- --mlb audio-driver.mlb -o audio-driver.bin src/audio-driver.asm


clean:
	$(RM) audio-driver.bin audio-driver.mlb

