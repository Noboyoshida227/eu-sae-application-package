# Security, privacy, and optional AI

AI features are off by default. Enabling the checkbox alone is insufficient:
the user must also acknowledge the external transfer before entering a key.

The comparison prompts send aggregate estimates, uncertainty measures,
diagnostics, counts, and analysis context. Geographic identifiers are removed
from the top-ranked and extreme-change prompt tables. Raw microdata are not
sent by the provided AI code. Aggregate unpublished estimates may nevertheless
be confidential or personal data in context, so authorization is still needed.

Approved gateways can be configured without code changes:

- `SAE_OPENAI_BASE_URL`
- `SAE_ANTHROPIC_BASE_URL`

Provider model IDs can also be overridden without editing the application:

- `SAE_OPENAI_MODEL` (default: `gpt-4.1`)
- `SAE_ANTHROPIC_MODEL` (default: `claude-sonnet-4-6`)

Do not place API keys in configuration files, saved setups, reports, logs, or
the repository. Review provider retention, residency, contractual terms, and
incident-response requirements before enabling external calls.

AI commentary is advisory text, not a statistical test or release decision.
When requested, it appears in distinctly labelled blocks beside the relevant
statistical sections in both `final_report.html` and `final_report.docx`. Provider text is escaped before HTML insertion, and every
expected section is marked generated, failed, or disabled. A warning-only
numeric-provenance check does not establish factual correctness. The structured
status and provider/model metadata are saved in
`outputs/data/ai_interpretations.rds`; API keys are never saved there.

Only deterministic statistics and approved human review may determine official
publication status. An AI-annotated report must not be disseminated until its
AI blocks have been reviewed under the applicable sign-off process.

Editing the Word report does not remove the requirement for documented human
review. Retain AI labels and provenance in the archived original; keep an edited
working copy separate. Apply the same confidentiality controls to both formats.
