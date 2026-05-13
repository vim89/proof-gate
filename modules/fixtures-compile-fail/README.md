# Compile-fail fixtures

This module stores bad drafts that should fail before AI or human review starts.

The files under `cases/` are reference snippets for demos and future scripted compile-fail checks.
They are intentionally kept outside `src/` so the normal build stays green.

Active compile-fail coverage currently lives in `modules/proof/src/test`.
