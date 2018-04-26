.PHONY: all compile test run lint

LIBS=_build/default/lib
PLT=.dialyzer_plt

compile:
	rebar3 compile

test:
	rebar3 ct

lint: compile $(PLT)
	dialyzer -r $(LIBS)/reminder_bot/ebin --plt $(PLT)

$(PLT):
	dialyzer --build_plt --apps erts kernel stdlib inets ssl --output_plt $(PLT)
	dialyzer --add_to_plt --plt $(PLT) -r $(LIBS)/lager
	dialyzer --add_to_plt --plt $(PLT) -r $(LIBS)/jsx
	dialyzer --add_to_plt --plt $(PLT) -r $(LIBS)/legendary_goggles

run: compile
	erl \
		-env ERL_LIBS $(LIBS) \
		+K true \
		-boot start_sasl \
		-config config/app \
		-name reminder_bot \
		-s reminder_bot
