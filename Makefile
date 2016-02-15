
SDCARD_LOCATION=/dev/sdc

DIALYZER_OPTS = -Wrace_conditions -Wunderspecs

all:
	rebar get-deps compile
	if [ -n "$(NERVES_ROOT)" ]; then $(NERVES_ROOT)/scripts/rel2fw.sh _rel/LinkBlox; fi

relsync:
	rebar compile
	../relsync/relsync --destnode demo@nerves --hooks relsync_hooks.erl --cookie democookie --sname relsync

burn-complete: burn
burn:
	sudo ../nerves-system-br/buildroot/output/host/usr/bin/fwup -a -i $(firstword $(wildcard *.fw)) -t complete

# Upgrade the image on the SDCard (app data won't be removed)
# This is usually the fastest way to update an SDCard that's already
# been programmed. It won't update bootloaders, so if something is
# really messed up, burn-complete may be better.
burn-upgrade:
	sudo ../nerves-system-br/buildroot/output/host/usr/bin/fwup -a -i $(firstword $(wildcard *.fw)) -t upgrade
	sudo ../nerves-system-br/buildroot/output/host/usr/bin/fwup -y -a -i /tmp/finalize.fw -t on-reboot
	sudo rm /tmp/finalize.fw


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
