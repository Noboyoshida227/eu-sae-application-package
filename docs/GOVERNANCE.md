# Governance and release authority

This repository does not currently establish an institutional product owner,
method owner, release approver, security owner, support service level, or
World Bank Group publication authorization. Existing author names and
affiliations in documents are not substitutes for those approvals.

## Interim change control

1. Every candidate change is linked to an issue-register item or release note.
2. Statistical-method changes require a named method owner, validation evidence
   and recorded approval before production use.
3. Data, documentation, image and third-party rights require an approved asset
   inventory before publication.
4. Security/privacy changes require review of data flows, providers, retention,
   residency, incident response and user notices.
5. A release manager records test, checksum, platform and document-QA evidence
   in `docs/RELEASE_CHECKLIST.md` and signs the exact commit/tag.
6. A production release must name the responsible organization, approvers,
   support channel, maintenance policy and vulnerability contact.

Until these roles and approvals are recorded, the package remains an
unsupported, unofficial release candidate and must not be described as an
official statistical production system.
