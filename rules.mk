
SRC_CPP := $(wildcard *.cpp)
BIN_CPP := $(basename $(SRC_CPP))
SRC_RS := $(wildcard */Cargo.toml)
BIN_RS := $(dir $(SRC_RS))

CXXFLAGS := -std=c++11

.PHONY: all
all: $(BIN_CPP) $(BIN_RS)

define create_target_cpp
$(1): $(1).o
	g++ -o $$@ $$< $$(LDFLAGS)
endef

define create_target_rs
$(1): FORCE
	cargo build --manifest-path $$@/Cargo.toml
endef

$(foreach jour, $(BIN_CPP), $(eval $(call create_target_cpp, $(jour))))
$(foreach jour, $(BIN_RS), $(eval $(call create_target_rs, $(jour))))

%.o: %.cpp
	g++ $(CXXFLAGS) -o $@ -c $<

.PHONY: clean
clean:
	rm -f $(BIN_CPP) $(BIN_RS) $(SRC_CPP:.cpp=.o)

FORCE:
