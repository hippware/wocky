.PHONY: all cleanall distclean eunit ct ci

REBAR=./rebar3 $(REBAR_OPTS)

all: compile

cleanall:
	@$(REBAR) clean --all
	@rm -rf _build/default/lib/ejabberd
	@rm -rf _build/default/rel
	@rm -rf _build/test
	@rm -rf log

distclean: clean
	@rm -rf _build

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

ci: cleanall compile lint xref dialyzer eunit ct cover
	@echo "Build complete."

tar: release
	@cd _build/default/rel/wocky; tar zcf ../wocky.tar.gz *

%:
	@$(REBAR) $@
