ERL=erl
BEAMDIR=./deps/*/ebin ./ebin
REBAR=rebar3

all: clean compile

compile:
	@$(REBAR) compile

xref:
	@$(REBAR) xref skip_deps=true

clean:
	@ $(REBAR) clean

test: eunit

edoc:
	@$(REBAR) skip_deps=true doc
