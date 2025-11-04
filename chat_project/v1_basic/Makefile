.PHONY: all clean run test

ERLC = erlc
ERL = erl
ERLC_FLAGS = -Wall

SRC_DIR = src
EBIN_DIR = ebin

SOURCES = $(wildcard $(SRC_DIR)/*.erl)
BEAMS = $(patsubst $(SRC_DIR)/%.erl,$(EBIN_DIR)/%.beam,$(SOURCES))

all: $(EBIN_DIR) $(BEAMS)

$(EBIN_DIR):
	mkdir -p $(EBIN_DIR)

$(EBIN_DIR)/%.beam: $(SRC_DIR)/%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

clean:
	rm -rf $(EBIN_DIR)
	find . -name "*.beam" -delete
	find . -name "erl_crash.dump" -delete

run: all
	@echo "Starting chat server on port 5555..."
	@echo "Connect with: telnet localhost 5555"
	$(ERL) -pa $(EBIN_DIR) -eval 'chat_server:start().'

test: all
	@echo "Running basic tests..."
	$(ERL) -pa $(EBIN_DIR) -noshell -eval 'io:format("Modules compiled successfully~n"), init:stop().'

help:
	@echo "Chat Server v1 - Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  make          - Compile all modules"
	@echo "  make run      - Start chat server on port 5555"
	@echo "  make clean    - Remove compiled files"
	@echo "  make test     - Verify compilation"
	@echo ""
	@echo "Manual usage:"
	@echo "  erlc -o ebin src/*.erl"
	@echo "  erl -pa ebin"
	@echo "  1> chat_server:start()."
