#' @title %Func% of Values in an Image
#' @description Computes the %func% of values
#' of an image with the option for a mask.
#'  
#' @param x Object of class \code{nifti}
#' @param ... Arguments passed to \code{\link{%func%.default}}
#' @param mask object to subset the image.  If missing, then all 
#' values of the image are plotted.
#'
#' @return Output of \code{\link{%func%}}
#' @export
#' @examples 
#' img = nifti(array(rnorm(10^3), dim = rep(10, 3)))
#' mask = img > 0
#' %func%(img, mask = mask)
%func%.nifti = function(x, ..., mask) {
  if (missing(mask)) {
    x = img_data(x)
    x = c(x)
  } else {
    x = mask_vals(object = x, mask)
  }
  %func%(x, ...)
}

#' @export
#' @rdname %func%.nifti
#' @aliases %func%.anlz
%func%.anlz = function(x, ..., mask) {
  %func%.nifti(x = x, ..., mask = mask)
}
