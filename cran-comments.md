## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
---
I've updated `oro.nifti` to 0.14.3, which had the underlying bug.  There were a few conditions that were also shored up on the length 1 condition logical.  The `oro.nifti` bug crept up here because that code/condition wasn't executed in `oro.nifti` checks, but it is here.  Once `oro.nifti` version passed, this was updated.  I had to coordinate with that author for the pushes as I did not want to remove specific examples from vignettes.