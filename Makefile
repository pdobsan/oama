
LOCALDIR  = $(HOME)/.local
LIBDIR    = $(LOCALDIR)/lib/email-oauth2
VARDIR    = $(LOCALDIR)/var/email-oauth2
CABALDIR  = $(HOME)/.cabal
CABALBIN  = $(CABALDIR)/bin
CONFIGDIR = $(HOME)/.config/mailctl

help:
	@echo
	@echo Available targets:
	@echo
	@echo "uninstall  - remove all components from the system"
	@echo "             except $(VARDIR)"
	@echo

install: cabal-build $(CABALBIN)/mailctl $(CONFIGDIR)/config.json

$(CABALBIN)/mailctl: dist-newstyle/build/x86_64-linux/ghc-9.2.2/mailctl-0.1.0.0/x/mailctl/build/mailctl/mailctl
	install -m 700 $< $@
	strip $(CABALBIN)/mailctl

cabal-build:
	cabal build

$(CONFIGDIR):
	mkdir -p $(CONFIGDIR)

$(VARDIR):
	mkdir -p $(VARDIR)

$(CONFIGDIR)/config.json: config.json | $(CONFIGDIR)
	install -m 600 $< $@

clean:
	cabal clean

uninstall:
	rm -f $(CABALBIN)/mailctl
	rm -fr $(CONFIGDIR)
