REBAR_CLEAN=-r clean
REBAR_COMPILE=compile
REBAR_GENERATE=generate
REBAR=./rebar

compile:
	@$(REBAR) $(REBAR_COMPILE)

release:
	@$(REBAR) $(REBAR_GENERATE)

clean:
	@$(REBAR) $(REBAR_CLEAN)

build: compile test

test: compile
	@$(REBAR) skip_deps=true -r eunit

all: clean build release
