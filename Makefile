SHELL = /bin/bash
PROG = mailctl
# note $$2 for make eats the first $ !
VERSION = $(shell grep '^version:' $(PROG).cabal | awk '{print $$2}')
PROGX = $(PROG)-$(VERSION)-$(shell uname -s)-$(shell uname -m)
PROG_G = $(PROG)-*-$(shell uname -s)-$(shell uname -m)
GHC = 9.4.2

help:
	@echo
	@echo Available targets:
	@echo
	@echo "freeze  - set ghc $(GHC) and generate cabal.project.freeze"
	@echo "build   - build $(PROGX)"
	@echo "release - create a release of version $(VERSION)"
	@echo "aur     - publish $(PROG)-bin $(VERSION) on AUR"
	@echo

git-check:
	git status -s
	git diff --quiet

release: build aur/PKGBUILD
	git push
	gh release create $(VERSION) --generate-notes
	gh release upload $(VERSION) $(PROGX) $(PROGX).sha256 cabal.project.freeze
	git fetch --tags origin

cabal.project.freeze: freeze
freeze:
	ghcup set ghc $(GHC)
	cabal freeze

build: $(PROGX)
$(PROGX): cabal.project.freeze git-check
	cabal build
	cabal install
	cp -a ~/.cabal/bin/$(PROG) $(PROGX)
	sha256sum $(PROGX) > $(PROGX).sha256
	ls -l $(PROGX) $(PROGX).sha256 cabal.project.freeze

aur/PKGBUILD: $(PROG).cabal
	sed -i -e "s/^pkgver=.*$$/pkgver=$(VERSION)/" aur/PKGBUILD
	git diff --quiet aur/PKGBUILD

.PHONY: aur
aur: aur/PKGBUILD git-check
	gh workflow run aur.yaml

clean:
	rm -f $(PROG_G) $(PROG_G).sha256
	rm -f *.freeze

uninstall:
	rm -f ~/.cabal/bin/$(PROG)

clobber: clean uninstall
	cabal clean
