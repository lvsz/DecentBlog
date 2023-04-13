.PHONY: all test benchmark clean

BUILD		= ./build
INCLUDE		= ./include
HEADERS		= $(wildcard $(INCLUDE)/*.hrl)
ERLC_FLAGS	= -Wall -I $(INCLUDE)
ERLC		= erlc +debug_info

APP_ERL		= $(wildcard src/*.erl)
APP_OBJ		= $(SRC_ERL:src/%.erl=$(BUILD)/%.beam)

$(shell mkdir -p $(BUILD))

all: $(APP_OBJ)

$(BUILD)/%.beam: src/%.erl $(HEADERS)
	@echo "$< => $@"
	@$(ERLC) $(ERLC_FLAGS) -o $(BUILD) $<


TEST_ERL	= $(wildcard test/*_tests.erl)
TEST_OBJ	= $(TEST_ERL:test/%.erl=%.beam)

test: $(TEST_OBJ)

%_tests.beam: test/%_tests.erl $(HEADERS)
	$(ERLC) $(ERLC_FLAGS) -o $(BUILD) $<
	@echo testing: $@
	@erl -noshell -pa $(BUILD) -s $* test -s init stop


benchmark: *.beam
	exit 1 # TODO
	./run_benchmarks.sh

clean:
	$(RM) -r $(BUILD) erl_crash.dump

