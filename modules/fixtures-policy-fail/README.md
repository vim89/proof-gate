# Policy-fail fixtures

This module stores drafts that compile but must fail ProofGate policy review before AI and human review starts.

The module is intentionally not aggregated into the normal root build.
Run the expected-failure check instead:

```bash
scripts/check-policy-fixtures.sh
```

Current coverage:

- direct `sys.env` reads
- direct `System.getenv` reads
- direct `ConfigFactory` usage
- raw `try` / `catch`
- raw `throw`
- raw `require`
