
SRC_CPP := $(wildcard *.cpp)
BIN_CPP := $(basename $(SRC_CPP))
SRC_RS := $(wildcard */Cargo.toml)
BIN_RS := $(SRC_RS:Cargo.toml=target)
SRC_RSF := $(wildcard *.rs)
BIN_RSF := $(basename $(SRC_RSF))

CXXFLAGS := -std=c++11

.PHONY: all
all: $(BIN_CPP) $(BIN_RS) $(BIN_RSF)

define create_target_cpp
$(1): $(1).o
	g++ -o $$@ $$< $$(LDFLAGS)
endef

define create_target_rs
$(1): FORCE
	cargo build --manifest-path $$@/../Cargo.toml
endef

define create_target_rsf
$(1): $(1).rs
	rustc $$<
endef

$(foreach jour, $(BIN_CPP), $(eval $(call create_target_cpp, $(jour))))
$(foreach jour, $(BIN_RS), $(eval $(call create_target_rs, $(jour))))
$(foreach jour, $(BIN_RSF), $(eval $(call create_target_rsf, $(jour))))

%.o: %.cpp
	g++ $(CXXFLAGS) -o $@ -c $<

.PHONY: clean
clean:
	rm -rf $(BIN_CPP) $(SRC_CPP:.cpp=.o) $(BIN_RS) $(SRC_RS:.toml=.lock) $(BIN_RSF) $(SRC_RSF:.rs=.pdb)

FORCE:
