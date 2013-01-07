.PHONY: deps docs

all: check-deps
	@./rebar compile

deps:
	@./rebar get-deps

check-deps:
	@./rebar check-deps

test: test-eunit 

test-eunit:
	@./rebar eunit skip_deps=true

test-ct:
	@./rebar ct skip_deps=true verbose=1 suites=hub_ct
