###-----------------------------------------------------------------------------
### TARGETS
###-----------------------------------------------------------------------------
.PHONY: all compile clean check

all: compile

compile:
	rebar3 compile

clean:
	rebar3 clean

check:
	rebar3 check

###-----------------------------------------------------------------------------
### TEST TARGETS
###-----------------------------------------------------------------------------
.PHONY: test

test:
	@$(MAKE) postgres_up
	rebar3 test
	@$(MAKE) postgres_down

###-----------------------------------------------------------------------------
### LOCAL DEPLOYMENT TARGETS
###-----------------------------------------------------------------------------
.PHONY: postgres_up postgres_up

postgres_up:
	docker-compose -f docker-compose.yml -p erltity_example_postgres up -d

postgres_down:
	docker compose -p erltity_example_postgres down
