APP       = mailctl
GHC_VER   = ghc-$(shell ghc --version | choose -1)
APP_VER   = $(APP)-$(shell rg '^version:' $(APP).cabal | choose 1)
CABALDIR  = $(HOME)/.cabal
CABALBIN  = $(CABALDIR)/bin
CONFIGDIR = $(HOME)/.config/$(APP)

help:
	@echo
	@echo Available targets:
	@echo
	@echo "install   - install $(APP_VER) using $(GHC_VER)"
	@echo "uninstall - remove $(APP_VER) and its configs from the system"
	@echo

install: cabal-build $(CABALBIN)/$(APP) $(CONFIGDIR)/config.json

$(CABALBIN)/$(APP): dist-newstyle/build/x86_64-linux/$(GHC_VER)/$(APP_VER)/x/$(APP)/build/$(APP)/$(APP)
	install -m 700 $< $@
	strip $(CABALBIN)/$(APP)

cabal-build:
	cabal build

$(CONFIGDIR):
	mkdir -p $(CONFIGDIR)

$(CONFIGDIR)/config.json: configs/mailctl-config.json | $(CONFIGDIR)
	install -m 600 $< $@

clean:
	cabal clean

uninstall: clean
	rm -f $(CABALBIN)/$(APP)
	rm -fr $(CONFIGDIR)
