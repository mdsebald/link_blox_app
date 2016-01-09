
SDCARD_LOCATION=/dev/sdc

DIALYZER_OPTS = -Wrace_conditions -Wunderspecs

all:
	rebar get-deps compile
	if [ -n "$(NERVES_ROOT)" ]; then $(NERVES_ROOT)/scripts/rel2fw.sh _rel/BlockPoint; fi

relsync:
	rebar compile
	../relsync/relsync --destnode demo@nerves --hooks relsync_hooks.erl --cookie democookie --sname relsync

burn:
	sudo env PATH=$(PATH) fwtool -t complete -d $(SDCARD_LOCATION) run _images/bbb.fw

DEPSOLVER_PLT=$(CURDIR)/.depsolver_plt
ERLANG_APPS=erts kernel stdlib crypto public_key mnesia ssl

$(DEPSOLVER_PLT):
		dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
					--apps $(ERLANG_APPS) -r deps

dialyzer: $(DEPSOLVER_PLT)
		dialyzer --plt $(DEPSOLVER_PLT) $(DIALYZER_OPTS) --src src

typer: $(DEPSOLVER_PLT)
		typer --plt $(DEPSOLVER_PLT) -r ./src

clean:
	rebar clean
	-rm -fr _rel _images

distclean:
	-rm -fr _rel _images ebin deps $(DEPSOLVER_PLT)

.PHONY: dialyzer typer clean distclean burn
