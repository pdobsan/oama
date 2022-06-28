# mailctl

[![Build](https://github.com/pdobsan/mailctl/actions/workflows/build.yaml/badge.svg)](https://github.com/pdobsan/mailctl/actions/workflows/build.yaml)

`mailctl` provides IMAP/SMTP clients with the capabilities of renewal and
authorization of OAuth2 credentials.

Many IMAP/SMTP clients, like [msmpt](https://marlam.de/msmtp/),
[fdm](https://github.com/nicm/fdm),
[neomutt](https://github.com/neomutt/neomutt) or
[mutt](http://www.mutt.org/) can use OAuth2 access tokens but lack the
ability to renew and/or authorize OAuth2 credentials. The purpose of
`mailctl` is to provide these missing capabilities by acting as a kind of
smart password manager. In particular, access token renewal happens
automatically in the background transparent to the user.

If an IMAP/SMTP client cannot even use an OAuth2 access token itself it
still may be "wrapped" with `fdm` and `msmtp` with the help of `mailctl`.

`mailctl` also has some functionalities to manage the orchestration of
a mailing system comprised of `msmtp`, `fdm`, and `(neo)mutt` or similar
ones.

`mailctl` has been successfully used with Google/Gmail, operation with other
Oauth2 service providers are planned.

## Usage

Before `mailctl` is fully operational you need to create the necessary
configuration files. See details in [Configuration](#configuration). Make
sure that all the directories referenced in the configuration exist.

After configuration the very first command you must run is `authorize`. That
is an interactive process involving a browser since during it you need to
login and authorize access to your email account. `mailctl` will lead you
through this process.

`mailctl -h` generates this help message:

    mailctl - Provide OAuth2 renewal and authorization capabilities.

    Usage: mailctl [--version] [-c|--config-file <config>] [--run-by-cron] COMMAND

      Mailctl provides IMAP/SMTP clients with the capabilities of renewal and
      authorization of OAuth2 credentials.

    Available options:
      -h,--help                Show this help text
      --version                Show version
      -c,--config-file <config>
                               Configuration file
      --run-by-cron            mailctl invoked by cron

    Available commands:
      password                 Get the password for email
      access                   Get the access token for email
      renew                    Renew the access token of email
      authorize                Authorize OAuth2 for service/email
      fetch                    Get fdm to fetch all or the given accounts
      cron                     Manage running by cron
      list                     List all accounts in fdm's config
      printenv                 Print the current Environment

The various functionalities of `mailctl` are quite loosely coupled. In
particular, using a password manager (like `pass`) and the related
configuration details are optional.

More detailed help for individual commands can also be generated:

    % mailctl authorize -h

    Usage: mailctl authorize <service> <email>

      Authorize OAuth2 for service/email

    Available options:
      <service>                Service name
      <email>                  Email address
      -h,--help                Show this help text


### Shell completion
 
Shell completion for `bash`, `zsh` and `fish` shells can be automatically
generated. Here is how to do it for `zsh`:

      mailctl --zsh-completion-script $(which mailctl) > ~/.local/share/zsh/site-functions/_mailctl


## Configuration

The configuration files for `mailctl` are in YAML. The files in the
`configs/` directory are templates for `mailctl`, `msmtp` and `fdm`. The
`configs/services-template.yaml` file contains details for `google`,
`microsoft`. You need to provide your own `client_id` and `client_secret` of
your application or of a suitable FOSS registered application.

Edit them to adjust to your environment. When you are done run
`mailctl authorize <service> <email>`.

The base password manager used here is
[pass](https://www.passwordstore.org/)
The Oauth2 credentials are kept encrypted using
[GNU PG](https://www.gnupg.org/).

Since both `pass` and `mailctl` are using `gpg` it is assumed that an
authorized `gpg-agent` is running.

`fdm` is run by `cron` with a `crontab` like this:

    PATH  = /YOUR_HOME_DIRECTORY/.cabal/bin:/usr/bin:/bin
    # fetch mail in every 10 minutes
    */10 * * * *  mailctl --run-by-cron fetch


## Installation

### Compiled static binaries

Each release contains a compiled static executable of `mailctl` built on the
most recent Ubuntu LTS platform. That should work on most Linux
distributions. You can download the latest one from
[here](https://github.com/pdobsan/mailctl/releases/latest). Check the sha256
check sum of the downloaded file. Rename it to `mailctl`, made it executable
and put somewhere in your search path.

#### Archlinux

For Archlinux users there is also a package on AUR:
[mailctl-bin](https://aur.archlinux.org/packages/mailctl-bin)

### Building from sources

To build `mailctl` from source you need a Haskell development environment.
Either your platform package system can provide this or you can use
[ghcup](https://www.haskell.org/ghcup/). Once you have the `ghc` Haskell
compiler and `cabal` etc. installed, follow the steps below:

Clone this repository and invoke `cabal`:

    git clone https://github.com/pdobsan/mailctl.git
    cd mailctl
    cabal install

`mailctl` is known to build with `ghc 8.10.7` and `ghc 9.2.3`

### Logging

All transactions and exceptions are logged to `syslog`. If your OS using
`systemd` you can inspect the log with the command below:

    journalctl --identifier fdm --identifier mailctl --identifier msmtp -e


## Acknowledgment 

The method of dealing with OAuth2 services was inspired by the
[mutt_oauth2.py](https://gitlab.com/muttmua/mutt/-/blob/master/contrib/mutt_oauth2.py)
Python script written by Alexander Perlis.


## License

`mailctl` is released under the 3-Clause BSD License, see the file
[LICENSE](LICENSE).

