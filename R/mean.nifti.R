#' @title Mean of Values in an Image
#' @description Computes the mean of values
#' of an image with the option for a mask.
#'  
#' @param x Object of class \code{nifti}
#' @param ... Arguments passed to \code{\link{mean.default}}
#' @param mask object to subset the image.  If missing, then all 
#' values of the image are plotted.
#'
#' @return Output of \code{\link{mean}}
#' @export
#' @examples 
#' img = nifti(array(rnorm(10^3), dim = rep(10, 3)))
#' mask = img > 0
#' mean(img, mask = mask)
mean.nifti = function(x, ..., mask) {
  if (missing(mask)) {
    x = img_data(x)
    x = c(x)
  } else {
    x = mask_vals(object = x, mask)
  }
  mean(x, ...)
}

#' @export
#' @rdname mean.nifti
#' @aliases mean.anlz
mean.anlz = function(x, ..., mask) {
  mean.nifti(x = x, ..., mask = mask)
}
