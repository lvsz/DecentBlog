.PHONY: all test benchmark clean

BUILD		:= ./build
$(shell mkdir -p $(BUILD))

ERL_SRC		:= $(wildcard ./src/*.erl)
ERL_HRL		:= $(wildcard ./include/*.hrl)
ERL_BEAM	:= $(ERL_SRC:./src/%.erl=$(BUILD)/%.beam)
ERLC_FLAGS	:= -o $(BUILD) -I ./include
ERLC		:= erlc +debug_info -Wall

all: $(ERL_BEAM)

$(BUILD)/%.beam: ./src/%.erl $(ERL_HRL)
	$(ERLC) $(ERLC_FLAGS) $<

test: $(BEAM)
	exit 1 # TODO
	./run.sh supervisor test

benchmark: $(BEAM)
	exit 1 # TODO
	./run_benchmarks.sh

clean:
	$(RM) $(ERL_BEAM) erl_crash.dump
