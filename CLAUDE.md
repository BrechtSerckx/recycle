# CLAUDE.md — recycle

## Project overview

**recycle-ics** generates ICS calendar files for waste collection schedules from the recycleapp.be API. It has two components:

- **Backend** (`recycle-ics/`): Haskell library + executable. Serves a REST API and static files via Warp/Servant.
- **Frontend** (`recycle-ics-ui/`): React + TypeScript SPA (Create React App). Renders a form that calls the backend API and generates a download link.

The server serves the built frontend as static files from the path in `RECYCLE_ICS_WWW_DIR`.

## Environment

All commands must run inside the Nix shell. Use `direnv exec . just <recipe>` (preferred) or `direnv exec . <command>` directly.

```
direnv exec . just build-backend      # cabal build all
direnv exec . just test-backend       # cabal test
direnv exec . just serve              # cabal run recycle-ics -- serve-ics

direnv exec . just build-frontend     # npm run build  (in recycle-ics-ui/)
direnv exec . just dev                # npm start — hot-reload dev server

direnv exec . just build              # frontend then backend (order matters: path is baked in at compile time)
direnv exec . just fmt                # ormolu + cabal-fmt + prettier
direnv exec . just lint               # hlint + tsc --noEmit
```

Run `direnv exec . just` with no arguments to list all available recipes.

The `.env` file (loaded by direnv) sets required env vars. Override secrets in `local/env` (gitignored).

Key env vars:
| Var | Purpose |
|-----|---------|
| `RECYCLE_ICS_PORT` | Port for the server (default 3332) |
| `RECYCLE_ICS_CONSUMER` | `X-Consumer` header for recycleapp.be API |
| `RECYCLE_ICS_SECRET` | `X-Authorization` header for recycleapp.be API |
| `RECYCLE_ICS_WWW_DIR` | Path to built frontend (default `recycle-ics-ui/build`) |
| `RECYCLE_ICS_VERBOSITY` | Log level (`Debug`/`Info`/`Warning`/`Error`) |
| `REACT_APP_RECYCLE_ICS_SERVER_URL` | Backend URL used by the frontend |

`RECYCLE_ICS_WWW_DIR` is baked in at **compile time** via `th-env` / Template Haskell (`$$(TH.Env.envQ' "RECYCLE_ICS_WWW_DIR")`), so the frontend must be built before the backend if you change that path.

## Repository layout

```
recycle-ics/            # Haskell package
  src/
    Recycle/
      API.hs            # Servant client for recycleapp.be
      AppM.hs           # RecycleM monad / Env
      Class.hs          # HasRecycleClient, HasTime typeclasses
      Types.hs          # Domain types (Fraction, CollectionEvent, …)
      Types/
        Geo.hs          # ZipcodeId, StreetId, HouseNumber
        LangCode.hs     # NL/FR/EN/DE
        Error.hs
      Ics/
        API.hs          # Servant server API definition (RecycleIcsAPI)
        Server.hs       # Request handlers
        ICalendar.hs    # VCalendar/VEvent/VTodo construction
        Types.hs        # FractionEncoding, Filter, CollectionQuery
  app/
    Main.hs             # Entry point: CLI parse → GenerateIcs | ServeIcs
    Opts.hs             # optparse-applicative CLI definitions
    Parsers.hs          # Shared option parsers
  test/
    Spec.hs             # HSpec test suite
    responses/          # Fixture JSON files from recycleapp.be API

recycle-ics-ui/         # React/TypeScript frontend
  src/
    api.ts              # fetch() wrappers for the backend API
    types.ts            # FormInputs type + defaults
    App.tsx             # Root component
    section/            # Form sections (address, filter, daterange, …)
    Autocompleter.tsx   # Shared debounce helper

cabal.project           # Single-package Cabal project
shell.nix               # Nix dev shell (HLS, cabal, hlint, ormolu, node, npm, prettier)
default.nix             # Nix build
docker.nix              # Docker image build
```

## Backend architecture

- **Monad**: `RecycleM` = `ReaderT Env IO`. Uses the `capability` library for structured effects (`HasReader`, `HasState`, `HasThrow`) derived via `DerivingVia`.
- **Server**: Servant API defined in `Recycle.Ics.API`. Handlers in `Recycle.Ics.Server`. Static file serving for the frontend via `wai-app-static`.
- **Auth**: Token fetched from recycleapp.be and cached in `IORef (Maybe AuthResult)`. Refreshed automatically on expiry.
- **ICS generation**: `Recycle.Ics.ICalendar` builds `VCalendar` using the `iCalendar` package. Fractions can be encoded as `VEVENT` or `VTODO`.

## Frontend architecture

- React 18 + TypeScript, Create React App (react-scripts 5).
- `react-hook-form` with `FormProvider` for the multi-section form.
- Sections: Language → Address (zipcode autocomplete → street autocomplete → house number) → Filter (fractions) → Date range → Encoding → Download.
- The download URL is constructed from form values as query params and passed to `<a href=...>`.
- Backend URL comes from `REACT_APP_RECYCLE_ICS_SERVER_URL` (baked in at frontend build time).

## Haskell style conventions

- **Formatter**: `ormolu` (use `direnv exec . just fmt-backend` or `direnv exec . ormolu --mode inplace <file>`)
- **Linter**: `hlint` (config in `.hlint.yaml`)
- **GHC**: 9.4.5, `-Wall` enforced
- Default extensions listed in `recycle-ics.cabal` under `common-default-extensions` — most notably `OverloadedRecordDot`, `NoFieldSelectors`, `RecordWildCards`, `DerivingVia`, `DuplicateRecordFields`.
- 2-space indentation (brittany config in `brittany.yaml`, though ormolu is the active formatter).
- Explicit export lists required (hlint: `Use explicit module export list`).
- Prefer `DerivingVia` / `DeriveAnyClass` / `DeriveGeneric` over manual instances.

## Frontend style conventions

- **Formatter**: `prettier` (available in nix shell)
- TypeScript strict mode via CRA defaults.
- Functional components + hooks only.

## Running the full stack locally

```sh
direnv exec . just install-frontend   # once, or after package.json changes
direnv exec . just build              # build frontend then backend
direnv exec . just serve              # start server at http://localhost:3332
```

For frontend development with hot reload:
```sh
direnv exec . just dev                # proxies API calls to backend via REACT_APP_RECYCLE_ICS_SERVER_URL
```

## Tests

```sh
direnv exec . just test-backend
```

Tests use HSpec and fixture JSON files in `test/responses/` to test JSON deserialization of recycleapp.be API responses.

## CI

GitHub Actions (`nix.yaml`): installs Nix, runs `nix-shell --run 'npm install && npm run build'` in `recycle-ics-ui/`, then `cabal build all`, then `nix-build -A recycle-ics`.
