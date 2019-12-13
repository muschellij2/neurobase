#' Create Random `nifti` object
#'
#' @param dim dimensions for the `nifti` object
#' @param ... arguments to send to \code{\link{nifti}}
#'
#' @return A `nifti` object
#' @export
#'
#' @examples
#' random_nifti(c(10, 10, 2))
#' random_nifti(c(10, 10))
random_nifti = function(dim, ...) {
  n = prod(dim)
  img = array(rnorm(n), dim = dim)
  img = nifti(img, dim = dim, ...)
  img
}
