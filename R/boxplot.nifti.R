#' @title Boxplot of Values in an Image
#' @description Computes the boxplot of values
#' of an image with the option for a mask.
#'  
#' @param x Object of class \code{nifti}
#' @param ... Arguments passed to \code{\link{boxplot.default}}
#' @param mask object to subset the image.  If missing, then all 
#' values of the image are plotted.
#'
#' @return Output of \code{\link{boxplot}}
#' @export
#' @examples 
#' img = nifti(array(rnorm(10^3), dim = rep(10, 3)))
#' mask = img > 0
#' boxplot(img, mask = mask)
boxplot.nifti = function(x, ..., mask) {
  if (missing(mask)) {
    x = img_data(x)
    x = c(x)
  } else {
    x = mask_vals(object = x, mask)
  }
  boxplot(x, ...)
}

#' @export
#' @rdname boxplot.nifti
#' @aliases boxplot.anlz
boxplot.anlz = function(x, ..., mask) {
  boxplot.nifti(x = x, ..., mask = mask)
}
