# **oama** - OAuth credential Manager

> [NOTE!]
> `oama` is the successor of `mailctl`; for rationale and details, see
> discussion #26

The Oauth2 credentials are kept in the
[**Gnome keyring**](https://wiki.gnome.org/Projects/GnomeKeyring/) or
in [GNU PG](https://www.gnupg.org/) **encrypted files**.

Many IMAP/SMTP clients, like [msmtp](https://marlam.de/msmtp/),
[fdm](https://github.com/nicm/fdm),
[isync](http://isync.sourceforge.net/),
[neomutt](https://github.com/neomutt/neomutt) or
[mutt](http://www.mutt.org/) can use OAuth2 access tokens but lack the
ability to renew and/or authorize OAuth2 credentials. The purpose of
`oama` is to provide these missing capabilities by acting as a kind of
smart password manager. In particular, access token renewal happens
automatically in the background transparent to the user.

## Usage

Before `oama` is fully operational you need to create the necessary
configuration files. See details in [Configuration](#configuration). Make
sure that all the directories referenced in the configuration exist.

After configuration the very first command you must run is `authorize`. That
is an interactive process involving a browser since during it you need to
login and authorize access to your email account. `oama` will lead you
through this process.

Invoking `oama` without any arguments print this message:

OAMa - OAuth credential manager with store/lookup, renewal, authorization.

Usage: oama [--version] [-c|--config-file <config>] [--debug] COMMAND

  OAMa is an OAuth credential manager providing store/lookup, automatic renewal
  and authorization operations. The credentials are stored either in the Gnome
  keyring or in files encrypted by GnuPG. OAMa is useful for IMAP/SMTP or other
  network clients which cannot authorize and renew OAuth tokens on their own.

Available options:
  -h,--help                Show this help text
  --version                Show version
  -c,--config-file <config>
                           Configuration file
  --debug                  Print HTTP traffic to stdout

Available commands:
  access                   Get the access token for email
  renew                    Renew the access token of email
  authorize                Authorize OAuth2 for service/email
  password                 Get the password for email (only from keyring)
  printenv                 Print the current Environment

If no configuration file is specified the default is
`$XDG_CONFIG_HOME/oama/config.yaml`.

More detailed help for individual commands can also be generated, for
example:

    % oama authorize -h

Shell completion for `bash`, `zsh` and `fish` shells are provided.

### Authorization for Corporate/Organizational/Institutional accounts

Corporation/Organization/Institution accounts might be treated differently
by the service provider. In such cases, not passing a login hint might be
useful, see below.

#### Google Organizational Account

Invoke `oama` with no login hint:

    oama authorize google <you@company.email> --nohint

#### Microsoft Organizational Account

Invoke `oama` using your proper organizational email:

    oama authorize microsoft <you@company.email>

Then visit the `http://localhost:8080/start` page to perform the steps
below:

 - Click "Sign in with another account"
 - Click "Sign-in options"
 - Click "Sign in to an organization"
 - Put in the correct domain name which matches your organization address above
 - Log in with your credentials at the organization.

## Configuration

The configuration files for `oama` are in YAML. The files in the `configs/`
directory are templates for `oama`, `msmtp` and `fdm`. The
`configs/services-template.yaml` file contains details for `google`,
`microsoft` and `yahoo`.

You need to provide your own `client_id` and `client_secret` of your
application or of a suitable FOSS registered application.

Service providers are varying how they communicate at their authorization and
token endpoints. The accepted HTTP methods and how the request's parameters
delivered must be configured in the `services.yaml` file for both endpoints.
Options for `*_http_method` are `POST` and `GET`. Options for `*_params_mode`
are `query-string`, `request-body` (JSON encoded), `request-body-form`
(x-www-form-urlencoded) or `both`. When `both` is configured `oama` uses the
`request-body` method since it is considered safer.

If you encounter difficulties during *authorization* or *renewal* try to use
the `--debug` switch which causes `oama` to mirror print HTTP traffic to
`stdout`.

## Installation

### Compiled static binaries

Each release contains compiled static executables of `oama` which should
work on most Linux distributions.
Currently Linux/x86_64 and Linux/aarch64(arm64) binaries are provided.
Select the version you want to download from
[releases](https://github.com/pdobsan/oama/releases).

### Archlinux

For Archlinux users there is also a package on AUR:
[oama-bin](https://aur.archlinux.org/packages/oama-bin)

### Building from sources

To build `oama` from source you need a Haskell development environment,
with `ghc 9.2.4` or higher. Either your platform's package system can provide
this or you can use [ghcup](https://www.haskell.org/ghcup/). Once you have
the `ghc` Haskell compiler and `cabal` etc. installed, follow the steps
below:

Clone this repository and invoke `cabal`:

    git clone https://gitgub.com/pdobsan/oama
    cd oama
    cabal update
    cabal install --install-method=copy

That installs `oama` into the `~/.cabal/bin/` directory.


## Logging

All transactions and exceptions are logged to `syslog`. If your OS using
`systemd` you can inspect the log with a command like below:

    journalctl --identifier fdm --identifier oama --identifier msmtp -e

## Issues

Please, report any problems, questions, suggestions regarding `oama` by
opening an issue or by starting a discussion here.


## License

`oama` is released under the 3-Clause BSD License, see the file
[LICENSE](LICENSE).

