#' @name minmax_img-methods
#' @docType methods 
#' @aliases minmax_img 
#' @title Normalize Image using Range
#' @description Calculates the range of values in an image, then
#' scales the image minimum to 0 and maximum to 1
#' @return A \code{nifti} object (or list of them) or class of 
#' object passed in if not specified
#' @param img character path of image or 
#' an object of class \code{nifti}, or list of images
#' @export 
setGeneric("minmax_img", function(img) {
  standardGeneric("minmax_img")
})

#' @rdname minmax_img-methods
#' @aliases minmax_img,nifti-method
#' @export
setMethod("minmax_img", "nifti", function(img) { 
  r = range(c(img), na.rm = TRUE)
  if (diff(r) == 0) {
    warning("Difference in max/min is zero, NaNs will result!")
  }
  img = (img - r[1])/(r[2] - r[1])
  img = cal_img(img)
  return(img)
})

#' @rdname minmax_img-methods
#' @aliases minmax_img,array-method
#' @export
setMethod("minmax_img", "array", function(img) { 
  r = range(c(img), na.rm = TRUE)
  if (diff(r) == 0) {
    warning("Difference in max/min is zero, NaNs will result!")
  }
  img = (img - r[1])/(r[2] - r[1])
  return(img)
})


#' @rdname minmax_img-methods
#' @aliases minmax_img,ANY-method
#' @export
setMethod("minmax_img", "ANY", function(img) { 
  # workaround because can't get class
  if (inherits(img, "niftiImage")) {
    r = range(img, na.rm = TRUE)
    if (diff(r) == 0) {
      warning("Difference in max/min is zero, NaNs will result!")
    }    
    img = (img - r[1])/(r[2] - r[1])    
  } else {
    stop("Not implemented for this type!")
  }
  return(img)
})


#' @rdname minmax_img-methods
#' @aliases minmax_img,character-method
#'  
#' @export
setMethod("minmax_img", "character", function(img) { 
  img = check_nifti(img)
  img = minmax_img(img)
  return(img)
})


#' @rdname minmax_img-methods
#' @aliases minmax_img,list-method
#' @export
setMethod("minmax_img", "list", function(img) { 
  ### add vector capability
  img = lapply(img, minmax_img)
  return(img)
})



