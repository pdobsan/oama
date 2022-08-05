PROG = mailctl
# note $$2 for make eats the first $ !
VERSION = $(shell grep '^version:' $(PROG).cabal | awk '{print $$2}')
PROGX = $(PROG)-$(VERSION)-$(shell uname -s)-$(shell uname -m)
GHC = 9.2.4

help:
	@echo
	@echo Available targets:
	@echo
	@echo "freeze  - set ghc $(GHC) and generate cabal.project.freeze"
	@echo "build   - build $(PROGX)"
	@echo "release - create a release of the current version"
	@echo

release: build
	git push
	gh release create $(VERSION) --generate-notes
	gh release upload $(VERSION) $(PROGX) $(PROGX).sha256 cabal.project.freeze
	git fetch --tags upstream

cabal.project.freeze: freeze
freeze:
	ghcup set ghc $(GHC)
	cabal freeze

build: $(PROGX)
$(PROGX): cabal.project.freeze
	cabal build
	cabal install
	cp -a ~/.cabal/bin/$(PROG) $(PROGX)
	sha256sum $(PROGX) > $(PROGX).sha256
	ls -l $(PROGX) $(PROGX).sha256 cabal.project.freeze

aur: release
	gh workflow run aur.yaml

clean:
	rm -f $(PROGX) $(PROGX).sha256
	rm -f *.freeze

clobber: clean
	cabal clean
