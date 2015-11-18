.PHONY: all compile clean cleanall rel prodrel test

REBAR=./rebar3 $(REBAR_OPTS)


all: compile

compile:
	$(REBAR) compile

clean: docclean
	rm -rf apps/*/logs
	$(REBAR) clean

cleanall:
	rm -rf _build

rel:
	$(REBAR) release

prodrel:
	$(REBAR) as prod release

doc:
	$(REBAR) edoc

docclean: cleandoc

cleandoc:
	rm -f apps/wocky/doc/*html apps/wocky/doc/stylesheet.css apps/wocky/doc/edoc-info apps/wocky/doc/erlang.png

xref:
	$(REBAR) xref

dialyzer:
	$(REBAR) dialyzer

eunit:
	$(REBAR) eunit

# This compiles and runs one test suite. For quick feedback/TDD.
# Example:
# $ make ct SUITE=amp_resolver
ct:
	@if [ "$(SUITE)" ]; then $(REBAR) ct --suite $(SUITE);\
	else $(REBAR) ct; fi


##
## Integration Testing
##

TESTNODES = node1 node2
INT_TEST_DIR = ext/MongooseIM/test/ejabberd_tests

testrel: $(TESTNODES)

$(TESTNODES):
	@echo "building $@"
	$(REBAR) as test_$@ release

test_deps:
	(cd $(INT_TEST_DIR); make get-deps)

test: test_deps
	(cd $(INT_TEST_DIR); make test)

test_preset: test_deps
	(cd $(INT_TEST_DIR); make test_preset)

cover_test: test_deps
	(cd $(INT_TEST_DIR); make cover_test)

cover_test_preset: test_deps
	(cd $(INT_TEST_DIR); make cover_test_preset)

quicktest: test_deps
	(cd $(INT_TEST_DIR); make quicktest)
