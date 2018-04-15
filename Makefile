.PHONY: all compile test run

all: compile

compile:
	rebar3 compile

test:
	rebar3 ct

run: compile
	erl \
		-env ERL_LIBS _build/default/lib \
		+K true \
		-boot start_sasl \
		-config config/app \
		-name reminder_bot \
		-s reminder_bot
