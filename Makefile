.PHONY: all compile test run lint observer provision deploy

LIBS=_build/default/lib
PLT=.dialyzer_plt
INVENTORY=ansible/inventory
VAULT_ID=ansible/.vault-password

compile:
	rebar3 compile

test:
	rebar3 ct

lint: compile $(PLT)
	dialyzer -r $(LIBS)/reminder_bot/ebin --plt $(PLT)

$(PLT):
	dialyzer --build_plt --apps erts kernel stdlib inets ssl --output_plt $(PLT)
	dialyzer --add_to_plt --plt $(PLT) -r $(LIBS)/cowboy
	dialyzer --add_to_plt --plt $(PLT) -r $(LIBS)/lager
	dialyzer --add_to_plt --plt $(PLT) -r $(LIBS)/jsx
	dialyzer --add_to_plt --plt $(PLT) -r $(LIBS)/legendary_goggles

observer:
	erl -name observer -hidden -run observer

run: compile
	erl \
		-env ERL_LIBS $(LIBS) \
		+K true \
		-boot start_sasl \
		-config config/app \
		-reminder_bot token '"$(TOKEN)"' \
		-name reminder_bot \
		-s reminder_bot

provision: $(INVENTORY) $(VAULT_ID)
	ansible-playbook -i $(INVENTORY) --vault-id $(VAULT_ID) ansible/provision.yml

deploy: $(INVENTORY) $(VAULT_ID)
	ansible-playbook -i $(INVENTORY) --vault-id $(VAULT_ID) ansible/deploy.yml
