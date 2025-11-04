.PHONY: all compile clean run shell demo leak stress

ERLC=erlc
ERLC_FLAGS=-o ebin
ERL=erl
ERL_FLAGS=-pa ebin

SRC_DIR=src
EBIN_DIR=ebin

SOURCES=$(wildcard $(SRC_DIR)/*.erl)
TARGETS=$(patsubst $(SRC_DIR)/%.erl,$(EBIN_DIR)/%.beam,$(SOURCES))

# Also compile demo.erl
DEMO_BEAM=$(EBIN_DIR)/demo.beam

all: compile

$(EBIN_DIR):
	mkdir -p $(EBIN_DIR)

$(EBIN_DIR)/%.beam: $(SRC_DIR)/%.erl | $(EBIN_DIR)
	$(ERLC) $(ERLC_FLAGS) $<

$(EBIN_DIR)/demo.beam: demo.erl | $(EBIN_DIR)
	$(ERLC) $(ERLC_FLAGS) demo.erl

compile: $(EBIN_DIR) $(TARGETS) $(DEMO_BEAM)
	@echo "Compiled chat_server application"

clean:
	rm -rf $(EBIN_DIR)

shell: compile
	$(ERL) $(ERL_FLAGS)

# Run basic demo
run: compile
	$(ERL) $(ERL_FLAGS) -s demo run -s init stop

# Run leak demonstration
demo: compile
	$(ERL) $(ERL_FLAGS) -s demo demonstrate_leak -s init stop

# Run stress test with 50 images
stress: compile
	$(ERL) $(ERL_FLAGS) -eval 'demo:stress_test(50)' -s init stop

# Interactive with observer
observe: compile
	$(ERL) $(ERL_FLAGS) -eval 'observer:start()'
