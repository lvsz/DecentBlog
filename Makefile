.PHONY: all test benchmark clean

export ERLC ERLC_FLAGS HEADERS BUILD VPATH RUN_SH

BUILD		= ./build
SRC			= ./src
INCLUDE		= ./include
VPATH		= $(SRC):$(BUILD):$(INCLUDE)

ERL_FILES	= $(wildcard $(SRC)/*.erl)
OBJECTS		= $(ERL_FILES:$(SRC)/%.erl=%.beam)
HEADERS		= $(wildcard $(INCLUDE)/*.hrl)
ERLC_FLAGS	= -Wall -I $(INCLUDE)
ERLC		= erlc +debug_info
RUN_SH		= ./run.sh

$(shell mkdir -p $(BUILD))

all: $(OBJECTS)

%.beam: %.erl $(HEADERS)
	$(ERLC) $(ERLC_FLAGS) -o $(BUILD) $<

test: all
	@$(MAKE) -f test/Makefile run_tests


benchmark: *.beam
	exit 1 # TODO
	./run_benchmarks.sh

clean:
	$(RM) -r $(BUILD) erl_crash.dump
	@$(MAKE) -f test/Makefile clean_tests

