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


##
## Documentation generation
##

doc:
	$(REBAR) edoc

docclean: cleandoc

cleandoc:
	rm -f apps/wocky/doc/*html apps/wocky/doc/stylesheet.css apps/wocky/doc/edoc-info apps/wocky/doc/erlang.png


##
## Unit/Functional Testing
##

eunit:
	$(REBAR) eunit --cover --application wocky

ct:
	@if [ "$(SUITE)" ]; then $(REBAR) ct --cover --suite $(SUITE);\
	else $(REBAR) ct --cover; fi

# This compiles and runs one test suite. For quick feedback/TDD.
# Example:
# $ make qct SUITE=amp_resolver
qct:
	mkdir -p /tmp/ct_log
	ct_run -pa apps/*/ebin -pa deps/*/ebin -dir apps/*/test\
        -I apps/*/include -logdir /tmp/ct_log -suite $(SUITE)_SUITE -noshell


##
## Integration Testing
##

TESTNODES = node1 node2

testrel: $(TESTNODES)

$(TESTNODES):
	@echo "building $@"
	$(REBAR) as test_$@ release

test_deps:
	cd test/ejabberd; make get-deps

test: test_deps
	cd test/ejabberd; make test

test_preset: test_deps
	cd test/ejabberd; make test_preset

cover_test: test_deps
	cd test/ejabberd; make cover_test

cover_test_preset: test_deps
	cd test/ejabberd; make cover_test_preset

quicktest: test_deps
	cd test/ejabberd; make quicktest


##
## Dialyzer
##

COMBO_PLT = .wocky_combo_dialyzer.plt
# We skip some deps, because they're Dialyzer-broken
BANNED_DEPS = meck edown
BANNED_PATHS = $(addsuffix /ebin, $(addprefix deps/, $(BANNED_DEPS)))
DEPS_LIBS = $(filter-out $(BANNED_PATHS), $(wildcard deps/*/ebin))
MONGOOSE_LIBS = $(wildcard apps/ejabberd/ebin/*.beam)

OTP_APPS = compiler crypto erts kernel stdlib mnesia ssl ssh xmerl public_key tools sasl hipe edoc syntax_tools runtime_tools inets webtool asn1
DIALYZER_APPS = ejabberd mysql pgsql
DIALYZER_APPS_PATHS = $(addsuffix /ebin, $(addprefix apps/, $(DIALYZER_APPS)))

check_plt:
	dialyzer --check_plt --plt $(COMBO_PLT)

build_plt:
	dialyzer --build_plt --apps $(OTP_APPS) --output_plt $(COMBO_PLT) $(DEPS_LIBS)

dialyzer: check_plt dialyzer_quick

dialyzer_quick:
	dialyzer -n -Wno_return -Wno_unused -Wno_undefined_callbacks --fullpath --plt $(COMBO_PLT) $(DIALYZER_APPS_PATHS)
#	    fgrep -v -f ./dialyzer.ignore-warnings | tee dialyzer.log

cleanplt:
	rm $(COMBO_PLT)


%:
	@:
