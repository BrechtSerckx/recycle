Recycle
===

Generate ICS files and links for your waste collections. 
---

Based on recycleapp.be and the Recycle! app.

You can manually import the generated ICS files into your calendar, but let a tool like ICSx5/ICSDroid or a calendar app like Google Calendar or Outlook.com automatically import them for you. That way, your calendar always stays up to date with the waste collections. Note: ICSx5 works most reliably, Google Calendar for example does not support all features.

## How to install

Prerequisites:
- Nix

1. Clone this repo.
2. Run `nix-env -i -f default.nix`

## Usage

The application can be used in 2 ways: an CLI ICS file generator for waste collection, and a server for generating the ICS files.

### CLI app

See `recycle generate-ics --help`

### Server

See `recycle serve-ics --help`

## How to develop

Prerequisites:
- Nix
- direnv (optional)

1. Clone this repo.
2. Run `cd recycle`
3. Run `nix-shell` or `direnv allow` to install dev dependencies.
