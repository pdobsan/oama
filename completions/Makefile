PROG = oama
DEPS = ../lib/OAMa/CommandLine.hs
LOCAL = ~/.local/share/zsh/site-functions
BIN_DIR = ~/.cabal/bin

help:
	@echo Posible targets:
	@echo '  generate  -' generate completions for bash, zsh and fish shells
	@echo '  local-zsh -' install $(LOCAL)/_$(PROG)
	@echo '  uninstall -' remove $(LOCAL)/_$(PROG)
	@echo '  clean     -' delete all completions
	@echo '  clobber   -' clean and uninstall

generate: $(PROG).bash $(PROG).zsh $(PROG).fish

$(PROG).bash: $(DEPS)
	$(PROG) --bash-completion-script /usr/bin/$(PROG) >$@

$(PROG).zsh: $(DEPS)
	$(PROG) --zsh-completion-script /usr/bin/$(PROG) >$@

$(PROG).fish: $(DEPS)
	$(PROG) --fish-completion-script /usr/bin/$(PROG) >$@

local-zsh:
	$(BIN_DIR)/$(PROG) --zsh-completion-script $(BIN_DIR)/$(PROG) > $(LOCAL)/_$(PROG)

uninstall:
	rm -fr $(LOCAL)/_$(PROG)

clean:
	rm -f $(PROG).bash $(PROG).zsh $(PROG).fish

clobber: clean uninstall
