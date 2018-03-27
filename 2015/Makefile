

OBJ := $(patsubst %.cpp,%.o,$(wildcard *.cpp))
BIN := $(basename $(OBJ))

$(warning $(BIN))

CXXFLAGS := -std=c++11

.PHONY: all
all: $(BIN)

define create_target
$(1): $(1).o
	g++ -o $$@ $$<
endef

$(foreach jour, $(BIN), $(eval $(call create_target, $(jour))))

%.o: %.cpp
	g++ $(CXXFLAGS) -o $@ -c $<

.PHONY: clean
clean:
	rm -f $(BIN) $(OBJ)
