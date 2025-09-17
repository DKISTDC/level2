Level 2 Inversion
=================

*Admin tool to manage the creation of Level 2 data for the DKIST solar telescope.*

* See [DEVELOPMENT.md](./DEVELOPMENT.md)
* See [AGENTS.md](./AGENTS.md)


Project Structure
-----------------

Other Folders
* `app/` - Main.hs, and static web files 
* `bin/` - scripts to aid development
* `deps/` - static files used by tests, etc
* `image/` - used by docker
* `migrations/` - sqlx postgres migrations
* `test/` - tests

Types
* `types/` - dependent package containing general haskell code and NSO types

Application
* `src/App` - Hyperbole application
* `src/NSO/Data` - NSO-specific data interfaces
* `src/NSO/Image` - FITS and ASDF Image generation
* `src/NSO/Files` - File paths for L2 app
