.PHONY: compile

REBAR=./rebar $(REBAR_OPTS)

# ToDo: Setup header file dependencies correctly so the default make target can be "compile"
all: deps compile

compile: deps
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

rel: compile
	$(REBAR) generate -f skip_deps=true

# Compile only the project, exclude dependencies
nodeps:
	$(REBAR) compile skip_deps=true

doc:
	$(REBAR) doc skip_deps=true

