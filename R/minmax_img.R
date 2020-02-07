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
#' @examples 
#' set.seed(5)
#' dims = rep(10, 4)
#' arr = array(rpois(prod(dims), lambda = 2), dim = dims)
#' nim = oro.nifti::nifti(arr)
#' mimg = minmax_img(nim)
#' marr = minmax_img(arr)
#' testthat::expect_equal(array(mimg, dim = dim(mimg)), marr)
#' 
#' set.seed(5)
#' dims = rep(10, 3)
#' arr = array(rnorm(prod(dims)), dim = dims)
#' arr[,,10] = 0
#' nim = oro.nifti::nifti(arr)
#' rnifti = RNifti::asNifti(nim)
#' timg = tempimg(nim)
#' limg = list(factor(timg), factor(timg))
#' func = minmax_img
#' func(arr)
#' func(nim)
#' func(rnifti)
#' func(timg)
#' func(limg)
setGeneric("minmax_img", function(img) standardGeneric("minmax_img"))

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
#' @aliases minmax_img,factor-method
#'  
#' @export
setMethod("minmax_img", "factor", function(img) { 
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



