#' @name maskEmptyImageDimensions-methods
#' @docType methods 
#' @aliases maskEmptyImageDimensions 
#' @title Apply Masking from Empty Image Dimensions
#' @description Simple wrapper for replacing indices with a value
#' 
#' @param img image, nifti object, or array
#' @param inds indices of subset from \code{\link{getEmptyImageDimensions}} or
#' \code{\link{dropEmptyImageDimensions}}.
#' @param reorient Should image be reoriented if a filename?
#' @param mask.value Value to replace voxels outside the mask.
#' @param ... not used
#' @return Object of class \code{nifti} or \code{array} if \code{nifti}
#' is not supplied
#' @note \code{mask_empty_dim} is a shorthand for 
#' \code{maskEmptyImageDimensions} with all the same arguments.
#' @seealso \code{\link{getEmptyImageDimensions}}, 
#' \code{\link{dropEmptyImageDimensions}} 
#' @export
#' @examples 
#' set.seed(5)
#' dims = rep(10, 3)
#' arr = array(rnorm(prod(dims)), dim = dims)
#' arr[,,10] = 0
#' nim = oro.nifti::nifti(arr)
#' inds = getEmptyImageDimensions(nim)
#' inds_arr = getEmptyImageDimensions(arr)
#' res = maskEmptyImageDimensions(nim, inds = inds, mask.value = NA)
#' res2 = maskEmptyImageDimensions(arr, inds = inds_arr, mask.value = NA)
#' testthat::expect_equal(array(res, dim = dim(res)), 
#' array(res2, dim = dim(res2)))
#' 
#' set.seed(5)
#' dims = rep(10, 3)
#' arr = array(rnorm(prod(dims)), dim = dims)
#' arr[,,10] = 0
#' nim = oro.nifti::nifti(arr)
#' inds = getEmptyImageDimensions(nim)
#' rnifti = RNifti::asNifti(nim)
#' timg = tempimg(nim)
#' limg = list(factor(timg), factor(timg))
#' mask_empty_dim(nim, inds = inds)
#' func = function(...) maskEmptyImageDimensions(..., inds = inds)
#' func(arr)
#' func(nim)
#' func(rnifti)
#' func(timg)
#' func(limg)
#' 
setGeneric("maskEmptyImageDimensions", 
           function(img, 
                    inds,
                    reorient = FALSE,
                    mask.value = 0,
                    ...) standardGeneric("maskEmptyImageDimensions"))

.maskEmptyImageDimensions <- function(img, 
                                      inds,
                                      reorient = FALSE,
                                      mask.value = 0,
                                      ...) {
  dimg = dim(img)
  if (length(dimg) > 3) {
    stop(paste0("Only images with 3 dimensions supported, ", 
                "as checked by length(dim(img))"))
  }
  mask = array(mask.value, dim = dim(img))
  mask[inds[[1]], inds[[2]], inds[[3]]] = 
    img[inds[[1]], inds[[2]], inds[[3]]]
  return(mask)
}

#' @rdname maskEmptyImageDimensions-methods
#' @aliases maskEmptyImageDimensions,nifti-method
#' @export
setMethod("maskEmptyImageDimensions", "nifti", 
          function(img,  
                   inds,
                   mask.value = 0,
                   ...) {
            res = .maskEmptyImageDimensions(
              img = img, inds = inds,
              mask.value = mask.value, ...)
            res = copyNIfTIHeader(
              img = img, 
              arr = res, 
              drop = TRUE)  
            return(res)
          })

#' @rdname maskEmptyImageDimensions-methods
#' @aliases maskEmptyImageDimensions,nifti-method
#' @export
setMethod("maskEmptyImageDimensions", "character", 
          function(img,  
                   inds,
                   reorient = FALSE,
                   mask.value = 0,
                   ...) {
            img = check_nifti(img, reorient = reorient)
            res = maskEmptyImageDimensions(
              img, inds = inds,
              mask.value = mask.value, ...)
            return(res)
          })


#' @rdname maskEmptyImageDimensions-methods
#' @aliases maskEmptyImageDimensions,factor-method
#'  
#' @export
setMethod("maskEmptyImageDimensions", "factor", 
          function(img,  
                   inds,
                   reorient = FALSE,
                   mask.value = 0,
                   ...) { 
            img = as.character(img)
            res = maskEmptyImageDimensions(
              img = img,  
              inds = inds,
              mask.value = mask.value, ...)
            return(res)
          })


#' @rdname maskEmptyImageDimensions-methods
#' @aliases maskEmptyImageDimensions,list-method
#' @export
setMethod("maskEmptyImageDimensions", "list", 
          function(img,  
                   inds,
                   mask.value = 0,
                   ...) { 
            ### add vector capability
            res = lapply(
              img, maskEmptyImageDimensions, 
              inds = inds,
              mask.value = mask.value,
              ...)
            return(res)
          })


#' @rdname maskEmptyImageDimensions-methods
#' @aliases maskEmptyImageDimensions,array-method
#' @export
setMethod("maskEmptyImageDimensions", "array", 
          function(img,  
                   inds,
                   mask.value = 0,
                   ...) { 
            res = .maskEmptyImageDimensions(
              img = img,  inds = inds,
              mask.value = mask.value, ...)            
            return(res)
          })


#' @rdname maskEmptyImageDimensions-methods
#' @aliases maskEmptyImageDimensions,anlz-method
#' @export
setMethod("maskEmptyImageDimensions", "anlz", 
          function(img,  
                   inds,
                   mask.value = 0,
                   ...) { 
            img = as.nifti(img)
            res = maskEmptyImageDimensions(
              img = img, inds = inds,
              mask.value = mask.value, ...)
            return(res)
          })

#' @rdname maskEmptyImageDimensions-methods
#' @aliases maskEmptyImageDimensions,ANY-method
#' @export
setMethod("maskEmptyImageDimensions", "ANY", 
          function(img,  
                   inds,
                   reorient = FALSE,
                   mask.value = 0,
                   ...) {
            # workaround because can't get class
            if (inherits(img, "niftiImage")) {
              img = check_nifti(img, reorient = reorient)
              res = maskEmptyImageDimensions(
                img = img, inds = inds,
                mask.value = mask.value,  
                ...)
              return(res)              
            } else {
              stop("Not implemented for this type!")
            }
            return(img)
          })




#' @rdname maskEmptyImageDimensions-methods
#' @export
mask_empty_dim <- function(img, 
                            ...) {
  maskEmptyImageDimensions(img = img, ...)
}
