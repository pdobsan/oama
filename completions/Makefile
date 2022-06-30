PROG = mailctl
DEPS = ../lib/MailCtl/CommandLine.hs

generate: $(PROG).bash $(PROG).zsh $(PROG).fish

$(PROG).bash: $(DEPS)
	$(PROG) --bash-completion-script /usr/bin/$(PROG) >$@

$(PROG).zsh: $(DEPS)
	$(PROG) --zsh-completion-script /usr/bin/$(PROG) >$@

$(PROG).fish: $(DEPS)
	$(PROG) --fish-completion-script /usr/bin/$(PROG) >$@

local-zsh:
	~/.cabal/bin/$(PROG) --zsh-completion-script ~/.cabal/bin/$(PROG) > ~/.local/share/zsh/site-functions/_$(PROG)

clean:
	rm -f $(PROG).bash $(PROG).zsh $(PROG).fish

