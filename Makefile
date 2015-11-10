.PHONY: compile rel test

REBAR=./rebar $(REBAR_OPTS)
RELX=./relx


# Compile the application, but do not touch the dependencies
compile: deps
	$(REBAR) compile skip_deps=true

compileall:
	$(REBAR) compile

# Download and compile all dependencies, but not the application itself
deps:
	$(REBAR) get-deps
	$(REBAR) compile skip_apps=ejabberd,wocky

all: deps compile

clean:
	rm -rf apps/*/logs
	$(REBAR) clean

depsclean:
	rm -rf deps

cleanall: clean depsclean relclean testclean docclean
	rm -f fake_*.pem

certs: fake_cert.pem fake_server.pem

fake_cert.pem:
	openssl req \
	-x509 -nodes -days 365 \
	-subj '/C=PL/ST=ML/L=Krakow/CN=mongoose-im' \
	-newkey rsa:2048 -keyout fake_key.pem -out fake_cert.pem

fake_server.pem:
	cat fake_cert.pem fake_key.pem > fake_server.pem

rel: certs compile
	$(RELX) --config rel/relx.config --output rel

relclean:
	rm -rf rel/wocky


##
## Documentation generation
##

doc:
	$(REBAR) doc skip_deps=true

depsdoc:
	$(REBAR) doc

docclean: cleandoc

cleandoc:
	rm -f apps/wocky/doc/*html apps/wocky/doc/stylesheet.css apps/wocky/doc/edoc-info apps/wocky/doc/erlang.png


##
## Unit/Functional Testing
##

eunit: compile
	$(REBAR) skip_deps=true eunit

ct: compile
	@if [ "$(SUITE)" ]; then $(REBAR) ct suite=$(SUITE) skip_deps=true;\
	else $(REBAR) ct skip_deps=true; fi

# This compiles and runs one test suite. For quick feedback/TDD.
# Example:
# $ make qct SUITE=amp_resolver_SUITE
qct:
	mkdir -p /tmp/ct_log
	ct_run -pa apps/*/ebin -pa deps/*/ebin -dir apps/*/test\
        -I apps/*/include -logdir /tmp/ct_log -suite $(SUITE)_SUITE -noshell


##
## Integration Testing
##

TESTNODES = node1 node2

testrel: certs $(TESTNODES)

test_node_dir:
	mkdir -p test/nodes

$(TESTNODES): compile test_node_dir
	@echo "building $@"
	$(RELX) --config rel/relx.config --output-dir test/nodes --overlay_vars rel/reltool_vars/$@_vars.config
	mv test/nodes/wocky test/nodes/wocky_$@
	cp -R `dirname $(shell ./readlink.sh $(shell which erl))`/../lib/tools-* test/nodes/wocky_$@/lib/

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

testclean:
	rm -rf test/nodes


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
