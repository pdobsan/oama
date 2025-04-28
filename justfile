# List available recipes
_default:
    @just --list

# Clean
clean: _clean
    cabal clean

# Clean ghc/cabal configs
_clean:
    rm -f *.project.local *.project.local~
    rm -f *.project.freeze

# Configure to build with secret-tools
secret-tools: _clean
    cabal configure 

# Configure to build with secret-libs
secret-libs: _clean
    cabal configure -fsecret-libs

# Build oama according to last configuration
build:
    #!/usr/bin/env bash
    if [ -f cabal.project.local ]; then
        cabal build
    else
        echo Run one of secret_libs or secret-tools recipe first!
        exit 1
    fi

# Run oama without installing it
run args='--help': build
    cabal run oama -- {{args}} 
    
install-flags := '--install-method=copy --overwrite-policy=always'
# Install oama
install: build
    cabal install {{install-flags}}

bin-dir := "~/.local/bin"
program := "oama"

# Remove installed oama
uninstall:
    rm -f {{bin-dir}}/{{program}}

# Run strip and upx on the installed oama
trim:
    strip {{bin-dir}}/{{program}}
    upx {{bin-dir}}/{{program}}
