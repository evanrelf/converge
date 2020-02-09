# Todo

## Library

- Write the simplest possible functionality that overcomes all technical hurdles
  - [ ] Receive events via webhook API
  - [x] Write a pure function to handle the event (use `fused-effects`)
  - [x] Make an autheticated request to the GitHub API
  - Example: Leave a "Hello" comment on all new pull requests
- Add more API endpoints to Servant
- See if basic GitHub stuff like auth and errors can be generalized across
  multiple effects
- Add timestamps to `Log` IO carrier (or wrap a third-party library like
  `co-log`)

## Executable

- Write basic command-line options parser using `optparse-applicative`

## Tests

- Write basic test of core (pure) functions using `hspec`

## Other

- Write function to automatically add Haskell packages to the Nix overlay
  - [ ] Read the contents of the `nix/haskell-packages/` directory
  - [ ] Apply patches if they exist and their file name matches
  - [ ] Allow easier configuration of stuff like `dontCheck`
