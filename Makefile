## We don't use Make, this machinery just forwards everything to just
## See justfile for more information

# Use everything as arguments for wildcard
RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
# ...and turn them into do-nothing targets
$(eval $(RUN_ARGS):;@:)

.PHONY: makedefault
makedefault: default;

%:
	just $@ $(RUN_ARGS)
