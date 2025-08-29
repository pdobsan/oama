# List available recipes
_default:
    @just --list -u

# Run cabal clean then remove .local and .freeze files
clean:
    cabal clean
    rm -f *.project.local *.project.local~
    rm -f *.project.freeze
    rm -fr package

# Show current project configs
show-project-configs:
    @echo === cabal.project ===
    -@cat cabal.project
    @echo === cabal.project.local ===
    -@cat cabal.project.local

# Configure to build a fully static executable (in alpine only)
config-static:
    cabal configure --enable-append --enable-executable-static
    just show-project-configs

# Configure to build with gi-secret lib API
config-gisecret:
    cabal configure --enable-append -flib-gisecret
    just show-project-configs

# Configure to build with gpgme lib API
config-gpgme:
    cabal configure --enable-append -flib-gpgme
    just show-project-configs

# Build oama according to current configuration
build:
    cabal build

# Run oama without installing it
run +args='--help': build
    cabal run oama -- {{args}}

install-flags := '--install-method=copy --overwrite-policy=always'

# Install oama into ~/.local/bin
install: build
    #!/bin/bash -ex
    # cabal install {{install-flags}}
    OAMA_BIN=$(cabal list-bin -v0 oama)
    install "$OAMA_BIN" ~/.local/bin
    OAMA=$(which oama)
    just _trim $OAMA

# Remove installed oama
uninstall:
    rm -f $(which oama)

# Run strip and upx on prog
_trim prog:
    -strip {{prog}}
    -upx {{prog}}

# Create a precompiled binary package
package: build
    #!/bin/bash -xe
    KERNEL=$(uname -s)
    MACHINE=$(uname -m)
    OAMA_VERSION=$(awk '/^version:/ {print $2}' oama.cabal)
    OAMA_BIN=$(cabal list-bin -v0 oama)
    PKGDIR=oama-$OAMA_VERSION-$KERNEL-$MACHINE
    rm -fr package
    mkdir -p package/$PKGDIR
    install "$OAMA_BIN" package/$PKGDIR
    just _trim package/$PKGDIR/oama
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
