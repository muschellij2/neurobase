#' @title Double Orthographic Display
#' @description Orthographic function, but side-by-side
#' @return NULL
#' @seealso \link[oro.nifti]{orthographic}
#' @param x is an object of class nifti or similar.
#' @param y is an object of class nifti or similar to be set aside x.
#' @param col is grayscale (by default).
#' @param col.y is grayscale (by default).
#' @param NA.x Set any values of 0 in \code{x} to \code{NA}
#' @param mfrow (numeric) layout of the 3 slices
#' @param add Should the y-plot be added or its own plot?  Used
#' in \code{double_ortho}
#' @param ... other arguments to \code{\link{ortho2}}
#' @export
#' @examples
#' set.seed(10)
#' x = oro.nifti::nifti(array(rnorm(10000), dim = rep(10, 4)))
#' y = x > 2
#' mask = x > 2.5
#' double_ortho(x, y)
#'
double_ortho = function (x,
                         y = NULL,
                         col = gray(0:64 / 64),
                         col.y = gray(0:64 / 64),
                         NA.x = TRUE,
                         mfrow = c(2, 4),
                         add = FALSE,
                         ...)
{
  ortho2(
    x = x,
    y = y,
    col = col,
    col.y = col.y,
    NA.x = NA.x,
    add = add,
    mfrow = mfrow,
    ...
  )
}
