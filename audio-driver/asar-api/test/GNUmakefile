
MAKEFLAGS += --no-builtin-rules --warn-undefined-variables
.DELETE_ON_ERROR:
.SUFFIXES:


ASAR    ?= asar
64TASS  ?= 64tass


.PHONY: test asar 64tass clean

test: asar 64tass
	diff --report-identical-files out/asar-lorom.bin out/64tass-lorom.bin
	diff --report-identical-files out/asar-hirom.bin out/64tass-hirom.bin



asar: out/asar-lorom.bin out/asar-hirom.bin

ASAR_INCLUDES := ../tad-variables.inc ../tad-constants.inc ../tad-process.inc ../tad-control.inc

out/asar-lorom.bin: main.asm $(ASAR_INCLUDES)
	$(ASAR) -werror --no-title-check --symbols=wla -DLOROM '$<' '$@'

out/asar-hirom.bin: main.asm $(ASAR_INCLUDES)
	$(ASAR) -werror --no-title-check --symbols=wla -DHIROM '$<' '$@'



64tass: out/64tass-lorom.bin out/64tass-hirom.bin

64TASS_INCLUDES := ../../64tass-api/tad-code.inc ../../64tass-api/tad-process.inc ../../64tass-api/tad-lowram.inc ../../64tass-api/tad-zeropage.inc

out/64tass-lorom.bin: ../../64tass-api/test/64tass/main.asm $(64TASS_INCLUDES)
	$(64TASS) -a -x -X --nostart -Wall -C -DLOROM=1 -o '$@' '$<'

out/64tass-hirom.bin: ../../64tass-api/test/64tass/main.asm $(64TASS_INCLUDES)
	$(64TASS) -a -x -X --nostart -Wall -C -DHIROM=1 -o '$@' '$<'



clean:
	$(RM) out/asar-lorom.bin out/asar-lorom.sym out/asar-hirom.bin out/asar-hirom.sym
	$(RM) out/64tass-lorom.bin out/64tass-hirom.bin

