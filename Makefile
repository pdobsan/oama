PROG = oama
BIN_DIR = ~/.local/bin

help:
	@echo Possible targets:
	@echo '  build-with-libs    -' build using FFI to library API-s
	@echo '  build-with-tools   -' build to spawn external tools
	@echo '  install-with-libs  -' install $(PROG) built with library API-s
	@echo '  install-with-tools -' install $(PROG) built tp spawn external tools
	@echo '  trim               -' run strip and upx on the installed $(PROG)
	@echo '  uninstall          -' remove $(PROG) from $(BIN_DIR)
	@echo '  clean              -' cabal clean
	@echo '  clobber            -' clean and uninstall
	@echo

build-with-tools: clean-local
	nice cabal build

build-with-libs: clean-local
	nice cabal build -fsecret-libs

install-with-tools: build-with-tools
	cabal install --install-method=copy --overwrite-policy=always

install-with-libs: build-with-libs
	cabal install -fsecret-libs --install-method=copy --overwrite-policy=always

trim:
	strip $(BIN_DIR)/$(PROG)
	upx $(BIN_DIR)/$(PROG)

uninstall:
	rm -f $(BIN_DIR)/$(PROG)

clean-local:
	rm -f *.project.local *.project.local~
	rm -f *.project.freeze
	

clean: clean-local
	cabal clean

clobber: uninstall clean

