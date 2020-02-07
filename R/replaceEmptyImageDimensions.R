#' @name replaceEmptyImageDimensions-methods
#' @docType methods 
#' @aliases replaceEmptyImageDimensions 
#' @title Replace Subsetting from Empty Image Dimensions
#' @description Simple wrapper for subsetting an image with indices, 
#' dropping empty dimensions.  
#' @param img image, nifti object, or array
#' @param inds indices of subset from \code{\link{getEmptyImageDimensions}} or
#' \code{\link{dropEmptyImageDimensions}}.
#' @param target_dim Original dimension from which the data was subset,
#' the final dimension of the output
#' @param value value to replace in the image where outside the indices
#' @param reorient Should image be reoriented if a filename?
#' @param ... not used
#' @return Object of class \code{nifti} or \code{array} if \code{nifti}
#' is not supplied
#' @note \code{replace_empty_dim} is a shorthand for 
#' \code{replaceEmptyImageDimensions} with all the same arguments.
#' @seealso \code{\link{getEmptyImageDimensions}}, 
#' \code{\link{dropEmptyImageDimensions}} 
#' @export
#' @examples 
#' dims = rep(10, 3)
#' arr = array(rnorm(prod(dims)), dim = dims)
#' arr[,,10] = 0
#' nim = oro.nifti::nifti(arr)
#' rnifti = RNifti::asNifti(nim)
#' timg = tempimg(nim)
#' limg = list(factor(timg, timg))
#' inds = getEmptyImageDimensions(nim)
#' inds_arr = getEmptyImageDimensions(arr)
#' testthat::expect_equal(inds, inds_arr)
#' 
#' out = applyEmptyImageDimensions(nim, inds = inds)
#' result = replaceEmptyImageDimensions(out, inds = inds,
#' target_dim = dim(nim))
#' testthat::expect_equal(array(result, dim = dim(result)), 
#' array(nim, dim = dim(nim)))
#' replace_empty_dim(out, inds = inds,
#' target_dim = dim(nim))
#' 
#' target_dim = dim(nim)
#' 
#' arr = array(out, dim = dim(out))
#' nim = oro.nifti::nifti(arr)
#' rnifti = RNifti::asNifti(nim)
#' timg = tempimg(nim)
#' limg = list(factor(timg), factor(timg))
#' func = function(...) replaceEmptyImageDimensions(..., 
#' target_dim = target_dim, inds = inds)
#' func(arr)
#' func(nim)
#' func(rnifti)
#' func(timg)
#' func(limg)
setGeneric("replaceEmptyImageDimensions", 
           function(img, 
                    inds,
                    target_dim,
                    value = 0,
                    reorient = FALSE,
                    ...) standardGeneric("replaceEmptyImageDimensions"))

.replaceEmptyImageDimensions <- function(img, 
                                         inds,
                                         target_dim,
                                         value = 0,
                                         reorient = FALSE,
                                         ...) {
  dimg = dim(img)
  ndim = length(dimg)
  if (ndim > 3) {
    stop(paste0("Only images with 3 dimensions supported, ", 
                "as checked by length(dim(img))"))
  }
  i2 = array(value, dim = target_dim)
  arr_img = array(img, dim = dimg)
  if (ndim == 2) {
    i2[inds[[1]], inds[[2]]] = arr_img
  } else {
    i2[inds[[1]], inds[[2]], inds[[3]]]  = arr_img
  }
  return(i2)
}

#' @rdname replaceEmptyImageDimensions-methods
#' @aliases replaceEmptyImageDimensions,nifti-method
#' @export
setMethod("replaceEmptyImageDimensions", "nifti", 
          function(img,  
                   inds,
                   target_dim,
                   value = 0,
                   ...) {
            res = .replaceEmptyImageDimensions(
              img = img, inds = inds, target_dim = target_dim,
              value = value,
              ...)
            res = copyNIfTIHeader(img = img, 
                                  arr = res, 
                                  drop = TRUE)  
            return(res)
          })

#' @rdname replaceEmptyImageDimensions-methods
#' @aliases replaceEmptyImageDimensions,nifti-method
#' @export
setMethod("replaceEmptyImageDimensions", "character", 
          function(img,  
                   inds,
                   target_dim, 
                   value = 0,
                   reorient = FALSE,
                   ...) {
            img = check_nifti(img, reorient = reorient)
            res = replaceEmptyImageDimensions(img, inds = inds,
                                              target_dim = target_dim,
                                              value = value,
                                              ...)
            return(res)
          })


#' @rdname replaceEmptyImageDimensions-methods
#' @aliases replaceEmptyImageDimensions,factor-method
#'  
#' @export
setMethod("replaceEmptyImageDimensions", "factor", 
          function(img,  
                   inds,
                   target_dim, 
                   value = 0,
                   reorient = FALSE,
                   ...) { 
            img = as.character(img)
            res = replaceEmptyImageDimensions(img = img,  
                                              inds = inds,
                                              target_dim = target_dim,
                                              value = value,
                                              ...)
            return(res)
          })


#' @rdname replaceEmptyImageDimensions-methods
#' @aliases replaceEmptyImageDimensions,list-method
#' @export
setMethod("replaceEmptyImageDimensions", "list", 
          function(img,  
                   inds,
                   target_dim,
                   value = 0,
                   ...) { 
            ### add vector capability
            res = lapply(img, replaceEmptyImageDimensions, 
                         inds = inds, 
                         target_dim = target_dim,       
                         value = value,
                         ...)
            return(res)
          })


#' @rdname replaceEmptyImageDimensions-methods
#' @aliases replaceEmptyImageDimensions,array-method
#' @export
setMethod("replaceEmptyImageDimensions", "array", 
          function(img,  
                   inds,
                   target_dim,  
                   value = 0,
                   ...) { 
            res = .replaceEmptyImageDimensions(
              img = img,  inds = inds, 
              target_dim = target_dim,
              value = value,
              ...)            
            return(res)
          })


#' @rdname replaceEmptyImageDimensions-methods
#' @aliases replaceEmptyImageDimensions,anlz-method
#' @export
setMethod("replaceEmptyImageDimensions", "anlz", 
          function(img,  
                   inds,
                   target_dim,
                   value = 0,
                   ...) { 
            img = as.nifti(img)
            res = replaceEmptyImageDimensions(img = img, inds = inds, 
                                              target_dim = target_dim,
                                              value = value,
                                              ...)
            return(res)
          })

#' @rdname replaceEmptyImageDimensions-methods
#' @aliases replaceEmptyImageDimensions,ANY-method
#' @export
setMethod("replaceEmptyImageDimensions", "ANY", 
          function(img,  
                   inds,
                   target_dim, 
                   value = 0,
                   reorient = FALSE,
                   ...) {
            # workaround because can't get class
            if (inherits(img, "niftiImage")) {
              img = check_nifti(img, reorient = reorient)
              res = replaceEmptyImageDimensions(img = img, inds = inds, 
                                                target_dim = target_dim,
                                                value = value,
                                                ...)
              return(res)              
            } else {
              stop("Not implemented for this type!")
            }
            return(img)
          })




#' @rdname replaceEmptyImageDimensions-methods
#' @export
replace_empty_dim <- function(img, 
                            ...) {
  replaceEmptyImageDimensions(img = img, ...)
}
