#' @name finite_img-methods
#' @docType methods 
#' @aliases finite_img 
#' @title Finite Image
#' @description Simple wrapper for setting non-finite values to zero 
#' @return nifti object 
#' @param img character path of image or 
#' an object of class \code{nifti}, or list of images
#' @param replace Value to replace non-finite values to
#' @export 
#' @author John Muschelli \email{muschellij2@@gmail.com} 
#' @examples 
#' set.seed(5)
#' dims = rep(10, 3)
#' arr = array(rpois(prod(dims), lambda = 2), dim = dims)
#' arr[c(5, 6, 7, 8)] = c(NA, NaN, Inf, -Inf)
#' nim = nifti(arr)
#' finite_img(nim)
#' finite_img(arr)
#' tfile = tempimg(nim)
#' checkimg(c(tfile, tfile))
#' checkimg(list(tfile, tfile))
#' finite_img(list(tfile, tfile))
#' finite_img(c(tfile, tfile))
#' img = RNifti::readNifti(tfile)
#' checkimg(img)
#' img[c(5, 6, 7, 8)] = c(NA, NaN, Inf, -Inf)
#' stopifnot(!any(c(is.na(c(finite_img(img))))))
setGeneric("finite_img", function(img, replace = 0) standardGeneric("finite_img"))

#' @rdname finite_img-methods
#' @aliases finite_img,nifti-method
#' @export
setMethod("finite_img", "nifti", function(img, replace = 0) { 
  img[ !is.finite(img) ] = replace
  img = cal_img(img)
  return(img)
})

#' @rdname finite_img-methods
#' @aliases finite_img,array-method
#' @export
setMethod("finite_img", "array", function(img, replace = 0) { 
  img[ !is.finite(img) ] = replace
  return(img)
})

#' @rdname finite_img-methods
#' @aliases finite_img,ANY-method
#' @export
setMethod("finite_img", "ANY", function(img, replace = 0) { 
  # workaround because can't get class
  if (inherits(img, "niftiImage")) {
    img[ !is.finite(img) ] = replace
  } else {
    stop("Not implemented for this type!")
  }
  return(img)
})


#' @rdname finite_img-methods
#' @aliases finite_img,character-method
#'  
#' @export
setMethod("finite_img", "character", function(img, replace = 0) { 
  img = check_nifti(img)
  img = finite_img(img, replace = replace)
  return(img)
})


#' @rdname finite_img-methods
#' @aliases finite_img,list-method
#' @export
setMethod("finite_img", "list", function(img, replace = 0) { 
  ### add vector capability
  img = lapply(img, finite_img, replace = replace)
  return(img)
})

