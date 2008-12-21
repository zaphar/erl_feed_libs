include ../include/globals.mk

all: $(ERL_OBJECTS)

clean:
	rm -f $(CLEAN_TARGETS)

test: $(MODULES)

./$(MODULES): $(ERL_OBJECTS) clean_db
	@rm -rf $(ERL_DB)
	@echo "#Running tests for $@"
	@$(ERL_RUN) $@ $(ERL_STOP)
