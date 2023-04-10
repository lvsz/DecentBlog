.PHONY: run_tests clean_tests

ifeq ($(notdir $(CURDIR)),test)

%: run_tests clean_tests
	@$(MAKE) -C .. $(MAKECMDGOALS)

else

TESTDIR		:= ./test
VPATH		:= $(VPATH):$(TESTDIR)

SUITE		:= _SUITE
tests		:= _tests
ERL_FILES	:= $(wildcard $(TESTDIR)/*.erl)
OBJECTS		:= $(ERL_FILES:$(TESTDIR)/%.erl=%.beam)
SUITES		:= $(notdir $(basename $(wildcard $(TESTDIR)/*$(SUITE).erl)))

CONFIG		:= -config ./config/test
RUN_ARGS	:= $(CONFIG) -pa $(BUILD)

$(shell mkdir -p $(BUILD))

run_tests: $(info running $(SUITES))
run_tests: $(OBJECTS) $(SUITES)

%$(SUITE):
	$(RUN_SH) $@ test $(RUN_ARGS)

%$(SUITE).beam: %$(SUITE).erl %.erl $(HEADERS)
	$(ERLC) $(ERLC_FLAGS) -o $(BUILD) $<

%$(tests).beam: %$(tests).erl %.erl $(HEADERS)
	$(ERLC) $(ERLC_FLAGS) -o $(BUILD) $<

clean_tests:
	$(RM) -r $(BUILD) ./logs

endif

