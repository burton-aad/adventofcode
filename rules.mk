
SRC_C := $(wildcard */*.c)
BIN_C := $(basename $(SRC_C))
SRC_EL := $(wildcard */*.el)
BIN_EL := $(SRC_EL:.el=.elc)
SRC_CPP := $(wildcard *.cpp)
BIN_CPP := $(basename $(SRC_CPP))
SRC_RS := $(wildcard *.rs)
BIN_RS := $(basename $(SRC_RS))
SRC_CARGO := $(wildcard */Cargo.toml)
CARGO_FOLDER := $(SRC_CARGO:/Cargo.toml=)

CXXFLAGS := -std=c++11
EMACS := emacs

.DEFAULT_GOAL := all

define create_target_cpp
$(1): $(1).o
	g++ -o $$@ $$< $$(LDFLAGS)
endef

define create_target_cargo
BIN_CARGO += $(addprefix $(1),$(addprefix /target/debug/,$(1)))
$$(lastword $$(BIN_CARGO)): $(wildcard $(1)/src/*.rs) $(1)/Cargo.toml
	cargo build --manifest-path $$?
	touch $$@
endef

define create_target_rs
$(1): $(1).rs
	rustc $$<
endef

$(foreach jour, $(BIN_CPP), $(eval $(call create_target_cpp, $(jour))))
$(foreach jour, $(CARGO_FOLDER), $(eval $(call create_target_cargo, $(jour))))
$(foreach jour, $(BIN_RS), $(eval $(call create_target_rs, $(jour))))

.PHONY: all
all: $(BIN_C) $(BIN_EL) $(BIN_CPP) $(BIN_CARGO) $(BIN_RS)

%.o: %.cpp
	g++ $(CXXFLAGS) -o $@ -c $<

%.elc: %.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<

.PHONY: clean
clean:
	rm -f $(BIN_C) $(BIN_EL) $(BIN_CPP) $(SRC_CPP:.cpp=.o) $(BIN_RS) $(SRC_RS:.rs=.pdb)
	rm -rf $(CARGO_FOLDER:%=%/target) $(SRC_CARGO:.toml=.lock)

FORCE:
