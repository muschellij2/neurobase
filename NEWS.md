# neurobase 1.27.11

* Now `ortho_diff` should be able to handle `RNifti` objects quickly.

# neurobase 1.27.10

* Added ability to do `ortho2` when third dimension is 1.

# neurobase 1.27.4

* Added a `NEWS.md` file to track changes to the package.
* Fixed `rescale_img`.
* Added arguments to `robust_window` for only non-zero values for quantile.
* Function `finite_img` for `array` works.
* Only `useRaster` if `is.null(y)` in `ortho2`.