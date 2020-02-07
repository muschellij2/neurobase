
.separate_img = function(img, 
                         levels = NULL,
                         drop_zero = TRUE){
  if (is.null(levels)) {
    levels = unique(c(img))
  } else {
    levels = unique(levels)
  }
  if (drop_zero) {
    levels = setdiff(levels, 0)
  }
  if (length(levels) == 0) {
    stop("No non-zero values in the levels this image!")
  }
  levels = sort(levels)
  res = lapply(levels, function(x) {
    img == x
  })
  names(res) = levels
  return(res)
}


#' @name separate_img-methods
#' @docType methods 
#' @aliases separate_img 
#' @title Separate Labeled Image into Multiple Binary Images
#' @description Takes in an image, gets the unique values, then 
#' creates a list of binary images for each one of those values.
#' @note Exact equalling is using \code{==} 
#' @return A \code{nifti} object (or list of them) or class of 
#' object passed in if not specified
#' @param img character path of image or 
#' an object of class \code{nifti}, or list of images
#' @param levels if \code{levels} is given, then the separation is only
#' done for those levels and not unique values of the image.
#' @param drop_zero Should zeroes be dropped from the labels?  Zero 
#' usually denotes background or non-interesting voxels
#' @export 
#' @examples 
#' set.seed(5)
#' dims = rep(10, 3)
#' arr = array(rpois(prod(dims), lambda = 2), dim = dims)
#' nim = oro.nifti::nifti(arr)
#' simg = separate_img(nim)
#' simg_arr = separate_img(arr)
#' slist = lapply(simg, function(x) array(x, dim(x)))
#' testthat::expect_equal(slist, simg_arr)
#' 
#' rnifti = RNifti::asNifti(nim)
#' timg = tempimg(nim)
#' limg = list(factor(timg), factor(timg))
#' func = separate_img
#' func(arr)
#' func(nim)
#' func(rnifti)
#' func(timg)
#' func(limg)
setGeneric("separate_img", function(img, 
                                    levels = NULL,
                                    drop_zero = TRUE) standardGeneric("separate_img"))

#' @rdname separate_img-methods
#' @aliases separate_img,nifti-method
#' @export
setMethod("separate_img", "nifti", function(img, levels = NULL,
                                            drop_zero = TRUE) { 
  res = .separate_img(img = img,
                      levels = levels,                      
                      drop_zero = drop_zero)
  return(res)
})

#' @rdname separate_img-methods
#' @aliases separate_img,array-method
#' @export
setMethod("separate_img", "array", function(img, levels = NULL,
                                            drop_zero = TRUE) { 
  res = .separate_img(img = img, 
                      levels = levels,                      
                      drop_zero = drop_zero)
  return(res)
})


#' @rdname separate_img-methods
#' @aliases separate_img,ANY-method
#' @export
#' @importFrom RNifti updateNifti
setMethod("separate_img", "ANY", function(img, levels = NULL,
                                          drop_zero = TRUE) { 
  # workaround because can't get class
  if (inherits(img, "niftiImage")) {
    res = .separate_img(img = img, 
                        levels = levels,                      
                        drop_zero = drop_zero)
    res = lapply(res, function(x) {
      RNifti::updateNifti(x, template = img)
    })    
    return(res)
  } else {
    stop("Not implemented for this type!")
  }
  return(img)
})


#' @rdname separate_img-methods
#' @aliases separate_img,factor-method
#'  
#' @export
setMethod("separate_img", "factor", function(img, 
                                                levels = NULL,
                                                drop_zero = TRUE) { 
  img = as.character(img)
  img = separate_img(img,
                     levels = levels,
                     drop_zero = drop_zero)
  return(img)
})

#' @rdname separate_img-methods
#' @aliases separate_img,character-method
#'  
#' @export
setMethod("separate_img", "character", function(img, 
                                                levels = NULL,
                                                drop_zero = TRUE) { 
  img = check_nifti(img)
  img = separate_img(img,
                     levels = levels,
                     drop_zero = drop_zero)
  return(img)
})


#' @rdname separate_img-methods
#' @aliases separate_img,list-method
#' @export
setMethod("separate_img", "list", function(img, levels = NULL,
                                           drop_zero = TRUE) { 
  ### add vector capability
  img = lapply(img, separate_img,
               levels = levels,
               drop_zero = drop_zero
               )
  return(img)
})



