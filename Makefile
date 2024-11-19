PROG = oama
BIN_DIR = ~/.cabal/bin

help:
	@echo Possible targets:
	@echo '  build     -' cabal build
	@echo '  install   -' install $(PROG) into $(BIN_DIR)
	@echo '  trim      -' run strip and upx on the installed $(PROG)
	@echo '  uninstall -' remove $(PROG) from $(BIN_DIR)
	@echo '  clean     -' cabal clean
	@echo '  clobber   -' clean and uninstall
	@echo
	@echo 'Cabal flags can be passed with CABAL_FLAGS, for example'
	@echo
	@echo '  make install CABAL_FLAGS="-flibsecret"'
	@echo

build:
	cabal build $(CABAL_FLAGS)

install:
	cabal install --install-method=copy $(CABAL_FLAGS)

trim:
	strip $(BIN_DIR)/$(PROG)
	upx $(BIN_DIR)/$(PROG)

uninstall:
	rm -f $(BIN_DIR)/$(PROG)

clean:
	cabal clean

clobber: uninstall clean

