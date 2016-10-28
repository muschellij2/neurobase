#' @title Density of Values in an Image
#' @description Computes the density of values
#' of an image with the option for a mask.
#'  
#' @param x Object of class \code{nifti}
#' @param ... Arguments passed to \code{\link{density.default}}
#' @param mask object to subset the image.  If missing, then all 
#' values of the image are plotted.
#'
#' @return Output of \code{\link{density}}
#' @export
#' @examples 
#' img = nifti(array(rnorm(10^3), dim = rep(10, 3)))
#' mask = img > 0
#' density(img, mask = mask)
density.nifti = function(x, ..., mask) {
  if (missing(mask)) {
    x = img_data(x)
    x = c(x)
  } else {
    x = mask_vals(object = x, mask)
  }
  density(x, ...)
}

#' @export
#' @rdname density.nifti
#' @aliases density.anlz
density.anlz = function(x, ..., mask) {
  density.nifti(x = x, ..., mask = mask)
}
