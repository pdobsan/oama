PROG = oama
BIN_DIR = ~/.cabal/bin

help:
	@echo Possible targets:
	@echo '  build     -' cabal build
	@echo '              cabal flags can be passed with CABAL_FLAGS:'
	@echo '                 make build CABAL_FLAGS="--allow-newer ..."'
	@echo '  install   -' install $(PROG) into $(BIN_DIR)
	@echo '  trim      -' run strip and upx on the installed $(PROG)
	@echo '  uninstall -' remove $(PROG) from $(BIN_DIR)
	@echo '  clean     -' cabal clean
	@echo '  clobber   -' clean and uninstall

build:
	cabal build $(CABAL_FLAGS)

install: build
	cabal install $(CABAL_FLAGS)

trim:
	strip $(BIN_DIR)/$(PROG)
	upx $(BIN_DIR)/$(PROG)

uninstall:
	rm -f $(BIN_DIR)/$(PROG)

clean:
	cabal clean

clobber: uninstall clean

