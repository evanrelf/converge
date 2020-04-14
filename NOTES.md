# Notes

## Ideas

- Add `/config` endpoint for tweaking behavior at runtime
  - Log verbosity
  - Which components are active
  - Constants (polling rate, etc.)
- Event sourcing design, with replayable event log to fuzz test pure core
- See if basic GitHub stuff like auth and errors can be generalized across
  multiple effects
- When receiving edit events for unknown items, a fetch for that item should be
  triggered

## Tasks

- Disambiguate issue and pull request comments
- Finish `Bread` effect over at `evanrelf/fused-effects-garlic-bread` so I can
  augment my `Log` effect with context or something
- Add timestamps to `Log` IO carrier (or wrap a third-party library like
  `co-log`)
- Write basic command-line options parser using `optparse-applicative`
- Write basic test of core (pure) functions using `hspec`
