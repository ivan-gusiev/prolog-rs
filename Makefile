## Prolog-rsw uses `just` instead of `Make` for  build scripts
## This makefile just forwards everything to the equivalent `just` command

# Use everything as arguments for wildcard
RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
# ...and turn them into do-nothing targets
$(eval $(RUN_ARGS):;@:)

.PHONY: makedefault
makedefault: default;

%:
	just $@ $(RUN_ARGS)
