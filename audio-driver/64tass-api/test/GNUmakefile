
MAKEFLAGS += --no-builtin-rules --warn-undefined-variables
.SUFFIXES:


AS65    ?= ca65
LD65    ?= ld65
64TASS    ?= 64tass


.PHONY: test 64tass ca65 clean

test: 64tass ca65
	diff --report-identical-files out/64tass-lorom.bin out/ca65-lorom.bin
	diff --report-identical-files out/64tass-hirom.bin out/ca65-hirom.bin



64tass: out/64tass-lorom.bin out/64tass-hirom.bin

64TASS_API := ../tad-code.inc ../tad-process.inc ../tad-zeropage.inc ../tad-lowram.inc

out/64tass-lorom.bin: 64tass/main.asm $(64TASS_API)
	$(64TASS) -a -x -X --nostart -Wall -C -DLOROM=1 --verbose-list -L $(basename $@).txt -o $@ 64tass/main.asm

out/64tass-hirom.bin: 64tass/main.asm $(64TASS_API)
	$(64TASS) -a -x -X --nostart -Wall -C -DHIROM=1 --verbose-list -L $(basename $@).txt -o $@ 64tass/main.asm



CA65_BINARIES := out/ca65-lorom.bin out/ca65-hirom.bin
CA65_OBJ := out/ca65-lorom.o out/ca65-hirom.o out/ca65-main.o

ca65: $(CA65_BINARIES)

out/ca65-%.bin: out/ca65-%.o out/ca65-main.o ca65/minimal.cfg
	$(LD65) -o $@ -C ca65/minimal.cfg $< out/ca65-main.o

out/ca65-lorom.o: ../../ca65-api/tad-audio.s
	$(AS65) -g -DLOROM -o $@ $<

out/ca65-hirom.o: ../../ca65-api/tad-audio.s
	$(AS65) -g -DHIROM -o $@ $<

out/ca65-main.o: ca65/main.s
	$(AS65) -g -o $@ $<



clean:
	$(RM) out/64tass-lorom.bin out/64tass-hirom.bin out/64tass-lorom.txt out/64tass-hirom.txt
	$(RM) $(CA65_BINARIES) $(CA65_OBJ)

