# Required Git-history and release-asset remediation

Removing files from the working tree does not remove them from Git history.
The eight literature PDFs remain retrievable from existing objects/tags and the
published v5.1.0 source archive. This is a public-release blocker.

The repository owner should coordinate the following with institutional IP,
records-management and repository administrators. These steps rewrite public
history and therefore are intentionally **not** executed by the candidate
builder.

1. Preserve an access-controlled evidentiary mirror and record the approved
   removal scope.
2. Notify collaborators that all commit IDs and affected tags will change and
   that old clones must not be pushed back.
3. Rewrite every branch and tag, for example with `git filter-repo --path
   'docs/guidance/literature' --invert-paths --force`, also removing any other
   specifically approved superseded binary paths.
4. Verify both `git log --all -- 'docs/guidance/literature'` and an all-object
   path/blob audit return no affected objects.
5. Force-push the rewritten branches and tags only after approval. Retire or
   replace the v5.1.0 GitHub release/tag and any separately uploaded archives.
6. Ask GitHub Support about cache/object purge where required, and review forks
   and mirrors; a force-push alone cannot recall existing downloads.
7. Re-clone into an empty directory, repeat the object audit, build the release
   there, and verify checksums before publication.
8. Establish one canonical tag convention and Git LFS or an approved document
   repository for future large binaries, with rights review before addition.

The folder built by `scripts/build_clean_release.R` contains no `.git` history
and is suitable for controlled review, but it is not evidence that the public
GitHub repository has been remediated.
