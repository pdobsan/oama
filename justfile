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
    rm -fr package

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
run +args='--help': build
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

# Create a precompiled binary package
package: build
    #!/bin/bash
    KERNEL=$(uname -s)
    MACHINE=$(uname -m)
    OAMA_VERSION=$(awk '/^version:/ {print $2}' oama.cabal)
    OAMA_BIN=$(cabal list-bin -v0 oama)
    PKGDIR=oama-$OAMA_VERSION-$KERNEL-$MACHINE
    rm -fr package
    mkdir -p package/$PKGDIR
    install -s "$OAMA_BIN" package/$PKGDIR
    cp LICENSE README.md package/$PKGDIR
    cp -r configs package/$PKGDIR
    cp -r completions package/$PKGDIR
    cd package
    tar czf $PKGDIR.tar.gz $PKGDIR
    if  [[ -n "$CI" && -n "$GITHUB_RUN_ID" ]] ; then
        mv $PKGDIR.tar.gz ..
        echo "artifactPath=$PKGDIR.tar.gz" >> $GITHUB_ENV
        echo "artifactName=$PKGDIR" >> $GITHUB_ENV
    fi
