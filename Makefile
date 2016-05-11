.PHONY: all cleanall distclean eunit ct ci

REBAR=./rebar3 $(REBAR_OPTS)

all: compile

cleanall:
	$(REBAR) clean --all

distclean: clean
	rm -rf _build

# This compiles and runs one test suite. For quick feedback/TDD.
# Example:
# $ make eunit SUITE=my_eunit_tests
eunit:
	@if [ "$(SUITE)" ]; then $(REBAR) eunit --suite $(SUITE);\
	else $(REBAR) eunit; fi

# This compiles and runs one test suite. For quick feedback/TDD.
# Example:
# $ make ct SUITE=my_ct_SUITE
ct:
	@if [ "$(SUITE)" ]; then $(REBAR) ct --suite $(SUITE);\
	else $(REBAR) ct; fi

ci: cleanall compile lint dialyzer eunit ct
	@echo "Build complete."

%:
	$(REBAR) $@
