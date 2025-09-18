# Repository Guidelines

IMPORTANT: Test and format your changes after every set of changes

```
cabal test
fourmolu --mode inplace <myfiles>
```

## Project Structure & Module Organization
- `src/App` hosts Hyperbole web UI handlers; `src/NSO/Data|Image|Files` cover NSO integrations and file orchestration.
- `types/` is the shared library; move cross-project types and anything that might belong on hackage here before reusing them in `src/`.
- `app/` contains the Cabal entrypoint (`Main.hs`) and static assets served at runtime.
- `migrations/` stores SQLx migrations; use sqlx to generate them
- Tests live in `test/`, with suites under `test/Test/`
- Helper scripts reside in `bin/`; treat them as the canonical local workflow.

## Build, Test, and Development Commands
- `cabal test` - checks compilation once and runs tests
- `sqlx migrate run` applies pending migrations against the configured database. Don't run this

## Coding Style & Naming Conventions
- Use two-space indentation
- Format Haskell with Fourmolu

## Testing Guidelines
- Tests use Skeletest; add suites in `test/Test/...`
- Mirror module names in test files (`Test/App/UploadSpec.hs`)
- Do not test live databases
- Avoid using fixtures

## Commit & Pull Request Guidelines
- Write concise, sub-80 character summaries
- Group related changes per commit; rerun `cabal test` beforehand and note coverage or data migrations in the PR body.
- PRs should describe scope
- Call out new environment variables and update `DEVELOPMENT.md` when behavior or setup changes.

## Environment & Security Notes
- ENV belongs in `.envrc` for local development. Do not alter without permission. Never commit secrets or workstation-specific paths.
- Avoid checking binaries into git.
