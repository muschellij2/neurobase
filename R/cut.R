#' @title Perform Cut on an image
#' @description Cuts a numeric image into an integer factor,
#'  with the option of a mask.
#' 
#' @param x Object of class \code{nifti}
#' @param ... Arguments passed to \code{\link{cut}}
#' @param mask object to subset the image.  If missing, then all 
#' values of the image are used
#'
#' @return Object of \class{nifti} with an \code{attribute} of levels
#' @export
#' @examples 
#' img = nifti(array(rnorm(10^3), dim = rep(10, 3)))
#' mask = img > 0
#' cut(img, mask = mask)
cut.nifti = function(x, 
                     ...,
                     mask){
  if (missing(mask)) {
    x = img_data(x)
    x = c(x)
    mask = array(1, dim = dim(x))
  } else {
    x = mask_vals(object = x, mask)
  }
  cuts = cut(x, ...)
  levs = levels(cuts)
  cuts = as.numeric(cuts)
  
  x = remake_img(vec = cuts, img = x, mask = mask)
  x = mask_img(x, mask = mask)
  
  attr(x, "levels") = levs
  return(x)
} 



#' @export
#' @rdname cut.nifti
#' @aliases cut.anlz
cut.anlz = function(x, ..., mask) {
  cut.nifti(x = x, ..., mask = mask)
}