REBAR ?= rebar

all: src

src:
	$(REBAR) get-deps
	$(REBAR) compile

clean:
	$(REBAR) clean

test: all
	$(REBAR) eunit

.PHONY: clean src all test
