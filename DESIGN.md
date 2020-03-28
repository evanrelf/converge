# Design

## Separation of concerns

Functional core, imperative shell.

The repository, and all events both to and from GitHub, should be modeled as
pure data structures. The core will receive events from the shell, and will
return actions to the shell. The shell will request an initial world state from
GitHub, and then listen for webhooks to update its world model. All statefulness
should be kept in the shell.

This separation of concerns is working perfectly if the shell can be replaced
with a pure mock.

The term "shell" also means it should only be a thin layer. As much logic as
possible should be purified and kept in the core.

## Logging

Complex programs make lots of decisions -- that's why they're useful! -- but
sometimes they don't make the right decision. Logging when and why decisions are
made helps when tracking down problems.

Logs aren't very useful if they don't have context. A **timestamp**,
**breadcrumbs** to where the log came from, and a human-readable **description**
should be included in every log entry.
