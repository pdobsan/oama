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

# Configure to build a fully static executable (in alpine only)
config-static: clean
    cabal configure --enable-executable-static

# Configure to build with gpgme lib API
config-gpgme: clean
    cabal configure -flib-gpgme

# Configure to build with gi-secret lib API
config-gisecret: clean
    cabal configure -flib-gisecret

# replace _GIT_STATUS_INFO string in built executable
_replace-git-hash oama_bin:
    #!/bin/bash -x
    # GIT_HASH=$(jj show -r @ | awk '/^Commit ID:/ {print $3}')
    GIT_HASH=$(git rev-parse HEAD)
    if [[ -n "$(git status --porcelain)" ]]
    then
        # repo is dirty
        STATUS="dirty"
    else
        # repo is clean
        STATUS="clean"
    fi
    perl -pi -e s/git-hash--123456789012345678901234567890-xxxxx/$GIT_HASH\ $STATUS/g {{oama_bin}}

# Build oama according to last configuration
build: _build
    #!/bin/bash -x
    OAMA_BIN=$(cabal list-bin -v0 oama)
    just _replace-git-hash $OAMA_BIN

_build:
    cabal build

# Run oama without installing it
run +args='--help': build
    cabal run oama -- {{args}}

install-flags := '--install-method=copy --overwrite-policy=always'

# Install oama
install: _build
    #!/bin/bash -x
    cabal install {{install-flags}}
    OAMA=$(which oama)
    just _replace-git-hash $OAMA
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
    just _replace-git-hash package/$PKGDIR/oama
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
