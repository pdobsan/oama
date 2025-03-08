PROG = oama
BIN_DIR = ~/.cabal/bin

help:
	@echo Possible targets:
	@echo '  build-with-libs    -' configure and build using secret-libs package
	@echo '  build-with-tools   -' configure and build using secret-tools package
	@echo '  install-with-libs  -' install using secret-libs package
	@echo '  install-with-tools -' install using secret-tools package
	@echo '  trim               -' run strip and upx on the installed $(PROG)
	@echo '  uninstall          -' remove $(PROG) from $(BIN_DIR)
	@echo '  clean              -' cabal clean
	@echo '  clobber            -' clean and uninstall
	@echo
	@echo 'Cabal flags can be passed with CABAL_FLAGS, for example'
	@echo
	@echo '  make install CABAL_FLAGS="-fsecret-libs"'
	@echo

build-with-tools: clean-local
	nice cabal build --project-file secret-tools.project

build-with-libs: clean-local
	nice cabal build --project-file secret-libs.project

install-with-tools: build-with-tools
	cabal install --project-file secret-tools.project --install-method=copy

install-with-libs: build-with-libs
	cabal install --project-file secret-libs.project --install-method=copy

trim:
	strip $(BIN_DIR)/$(PROG)
	upx $(BIN_DIR)/$(PROG)

uninstall:
	rm -f $(BIN_DIR)/$(PROG)

clean-local:
	rm -f *.project.local
	rm -f *.project.local~

clean: clean-local
	cabal clean

clobber: uninstall clean

