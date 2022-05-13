## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
---
I've updated `oro.nifti`, which had the underlying bug.  There were a few conditions that were also shored up on the length 1 condition logical.  The `oro.nifti` bug crept up here because that code/condition wasn't executed in `oro.nifti` checks, but it is here.  Once `oro.nifti` version passes, then this can be updated.