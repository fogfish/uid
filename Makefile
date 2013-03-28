.PHONY: deps test rel

ifeq ($(id),)
export id=uid
endif

all: rebar deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean
	rm -rf test.*-temp-data

distclean: clean 
	./rebar delete-deps

test: all
	./rebar skip_deps=true eunit

docs:
	./rebar skip_deps=true doc

dialyzer: compile
	@dialyzer -Wno_return -c apps/riak_kv/ebin

rel:
	./rebar generate

run:
	erl -name ${id}@127.0.0.1 -setcookie uid \
	    -pa ./deps/*/ebin \
	    -pa ./*/ebin \
	    -pa ./ebin \
	    -pa ./priv

rebar:
	curl -O https://raw.github.com/wiki/basho/rebar/rebar
	chmod ugo+x rebar

