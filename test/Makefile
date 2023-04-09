.PHONY: run_tests clean_tests

ifeq ($(notdir $(CURDIR)),test)

%: run_tests clean_tests
	@$(MAKE) -C .. $(MAKECMDGOALS)

else

TESTDIR		:= ./test
VPATH		:= $(VPATH):$(TESTDIR)

SUITE		:= _SUITE
ERL_FILES	:= $(wildcard $(TESTDIR)/*$(TEST_SUITE).erl)
OBJECTS		:= $(ERL_FILES:$(TESTDIR)/%.erl=%.beam)

CONFIG		:= -config ./config/test
RUN_ARGS	:= $(CONFIG) -pa $(BUILD)

$(shell mkdir -p $(BUILD))

run_tests: $(OBJECTS)

%$(SUITE).beam: %$(SUITE).erl %.erl $(HEADERS)
	$(ERLC) $(ERLC_FLAGS) -o $(BUILD) $<
	$(RUN_SH) $*$(SUITE) test $(RUN_ARGS)

clean_tests:
	$(RM) -r $(BUILD) ./logs

endif

