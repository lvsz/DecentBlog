.PHONY: all test benchmark clean

BUILD		:= ./build
INCLUDE		:= ./include
HEADERS		:= $(wildcard $(INCLUDE)/*.hrl) $(wildcard src/*.hrl)
ERLC_FLAGS	:= -Wall -I $(INCLUDE)
ERLC		:= erlc +debug_info

APP_SRC		:= $(wildcard src/*.erl)
APP_OBJ		:= $(APP_SRC:src/%.erl=$(BUILD)/%.beam)

TEST_SRC	:= $(wildcard test/*_tests.erl)
TEST_OBJ	:= $(TEST_SRC:test/%.erl=$(BUILD)/%.beam)
TESTS		:= $(TEST_OBJ:$(BUILD)/%.beam=%)

all: $(shell mkdir -p $(BUILD)) $(APP_OBJ)


test: all $(TEST_OBJ) $(TESTS)

$(BUILD)/%.beam: src/%.erl $(HEADERS)
	@echo "$< => $@"
	@$(ERLC) $(ERLC_FLAGS) -o $(BUILD) $<

$(BUILD)/%_tests.beam: test/%_tests.erl $(HEADERS)
	@echo "$< => $@"
	@$(ERLC) $(ERLC_FLAGS) -o $(BUILD) $<

%_tests:
	@echo testing: $*
	@erl -noshell -pa $(BUILD) -s $* test -s init stop


benchmark: *.beam
	exit 1 # TODO
	./run_benchmarks.sh

clean:
	$(RM) -r $(BUILD) erl_crash.dump

