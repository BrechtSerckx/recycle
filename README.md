Recycle
===

Generate ICS files and links for your waste collections. 
---

Based on recycleapp.be and the Recycle! app.

You can manually import the generated ICS files into your calendar, but let a tool like ICSx5/ICSDroid or a calendar app like Google Calendar or Outlook.com automatically import them for you. That way, your calendar always stays up to date with the waste collections. Note: ICSx5 works most reliably, Google Calendar for example does not support all features.

## Privacy

No data is stored. The service is completely stateless: your address information is encoded directly in the webcal link and converted to collection data on the fly on each request. Nothing is kept on the server.

## How to install

Prerequisites:
- Nix

1. Clone this repo.
2. Run `nix-env -i -f default.nix`

## Usage

The application can be used in 2 ways: a CLI ICS file generator for waste collection, and a server for generating the ICS files.

### CLI app

See `recycle generate-ics --help`

### Server

See `recycle serve-ics --help`

## How to develop

Prerequisites:
- Nix
- direnv

1. Clone this repo and `cd` into it.
2. Run `direnv allow` to enter the Nix dev shell.
3. Use `just` to run common tasks — run `just` with no arguments to list them.

```sh
just build              # build frontend then backend
just serve              # start server at http://localhost:3332
just dev                # frontend dev server with hot reload
just test               # run all tests
just fmt                # format all sources (ormolu, cabal-fmt, prettier)
just lint               # lint all sources (hlint, tsc)
```
