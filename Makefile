.PHONY: all cleanall distclean eunit ct

REBAR=./rebar3 $(REBAR_OPTS)

all: compile

cleanall:
	$(REBAR) clean --all

distclean: clean
	rm -rf _build

eunit:
	$(REBAR) eunit

# This compiles and runs one test suite. For quick feedback/TDD.
# Example:
# $ make ct SUITE=amp_resolver
ct:
	@if [ "$(SUITE)" ]; then $(TEST_REBAR) ct --suite $(SUITE);\
	else $(TEST_REBAR) ct; fi

%:
	$(REBAR) $@
