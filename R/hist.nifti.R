#' @title Histogram of Values in an Image
#' @description Computes and displays a histogram of the values
#' of an image with the option for a mask.
#' 
#' @param x Object of class \code{nifti}
#' @param ... Arguments passed to \code{\link{hist.default}}
#' @param mask object to subset the image.  If missing, then all 
#' values of the image are plotted.
#'
#' @return Output of \code{\link{hist}}
#' @export
#' @examples 
#' img = nifti(array(rnorm(10^3), dim = rep(10, 3)))
#' mask = img > 0
#' hist(img, mask = mask)
hist.nifti = function(x, ..., mask) {
  if (missing(mask)) {
    x = img_data(x)
    x = c(x)
  } else {
    x = mask_vals(object = x, mask)
  }
  hist(x, ...)
}

#' @export
#' @rdname hist.nifti
#' @aliases hist.anlz
hist.anlz = function(x, ..., mask) {
  hist.nifti(x = x, ..., mask = mask)
}