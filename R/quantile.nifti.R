#' @title Sample Quantiles
#' @description Computes sample 
#' quantiles for an image, with the option of a mask.
#' 
#' @param x Object of class \code{nifti}
#' @param ... Arguments passed to \code{\link{quantile}}
#' @param mask object to subset the image.  If missing, then all 
#' values of the image are plotted.
#'
#' @return Output of \code{\link{quantile}}
#' @export
#' @examples 
#' img = nifti(array(rnorm(10^3), dim = rep(10, 3)))
#' mask = img > 0
#' quantile(img, mask = mask)
quantile.nifti = function(x, ..., mask) {
  if (missing(mask)) {
    x = img_data(x)
    x = c(x)
  } else {
    x = mask_vals(object = x, mask)
  }
  quantile(x, ...)
}


#' @export
#' @rdname quantile.nifti
#' @aliases quantile.anlz
quantile.anlz = function(x, ..., mask) {
  quantile.nifti(x = x, ..., mask = mask)
}