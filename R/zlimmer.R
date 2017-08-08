#' @title Find Image z-limits
#' @description Helper function for plotting - returns zlim for \code{\link[graphics]{image}} 
#' plot function
#' @param x Object of class \code{nifti}
#' @param zlim A user-specified \code{zlim}.  If \code{NULL}, will calculate how 
#' \code{\link{ortho2}} would calculate \code{zlim}
#' @param computed_range If the range of the data was already computed,
#' this can be passed in and will be used if relevant.
#' @return If \code{zlim = NULL}, then vector of length 2, otherwise returns \code{zlim}
#' @export
zlimmer = function(x, zlim = NULL, computed_range = NULL){
  if (is.null(x)) {
    return(NULL)
  }
  x = check_nifti(x, allow.array = TRUE)
  x_is_nifti = inherits(x, "nifti")
  if (is.null(zlim)) {
    if (x_is_nifti) {
      zlim <- c(cal.min(x), cal.max(x))
      if (any(!is.finite(zlim)) || diff(zlim) == 0) {
        zlim <- c(glmin(x), glmax(x))
      }
    } else {
      zlim = c(0, 0)
    }
    if (any(!is.finite(zlim)) || diff(zlim) == 0) {
      if (!is.null(computed_range)) {
        stopifnot(length(computed_range) == 2)
        stopifnot(!any(is.na(computed_range)))
        zlim = computed_range
      } else {
        zlim <- range(x, na.rm = TRUE)
      }
    }
  }
  return(zlim)
}