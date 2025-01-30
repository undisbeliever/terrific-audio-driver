
MAKEFLAGS += --no-builtin-rules --warn-undefined-variables
.SUFFIXES:


MAIN_FILES    := src/loader.wiz src/audio-driver.wiz
INCLUDE_FILES := $(filter-out $(MAIN_FILES), $(wildcard src/*.wiz))

BIN_OUT       := $(patsubst src/%.wiz,%.bin,$(MAIN_FILES))
SYM_OUT       := $(patsubst src/%.wiz,%.sym,$(MAIN_FILES))


ifndef WIZ
  WIZ := $(firstword $(wildcard ../wiz/bin/wiz ../bin/wiz/wiz.exe))

  ifeq ($(WIZ), )
    $(error "Cannot find wiz binary.  Please import the wiz git submodule and compile wiz.")
  endif
endif


.PHONY: all clean


all: $(BIN_OUT) $(SYM_OUT)


clean:
	$(RM) $(BIN_OUT) $(SYM_OUT)


%.bin %.sym &: src/%.wiz $(INCLUDE_FILES)
	$(WIZ) --system=spc700 -s wla -o $@ $<

