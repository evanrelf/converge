# Todo

- Write the simplest possible functionality that overcomes all technical hurdles
  - Receive events via webhook API
  - Write a pure function to handle the event (use `fused-effects`)
  - Make an autheticated request to the GitHub API
  - Example: Leave a "Hello" comment on all new pull requests
- Add more API endpoints to Servant
- Handle general GitHub needs in umbrella `GitHub` effect
  - Authentication
  - Error handling
- Write basic command-line options parser using `optparse-applicative`
- Write basic test of core (pure) functions using `hspec`
