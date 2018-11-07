#' @name applyEmptyImageDimensions-methods
#' @docType methods 
#' @aliases applyEmptyImageDimensions 
#' @title Apply Subsetting from Empty Image Dimensions
#' @description Simple wrapper for subsetting an image with indices, 
#' dropping empty dimensions.  
#' @param img image, nifti object, or array
#' @param inds indices of subset from \code{\link{getEmptyImageDimensions}} or
#' \code{\link{dropEmptyImageDimensions}}.
#' @param reorient Should image be reoriented if a filename?
#' @param ... not used
#' @return Object of class \code{nifti} or \code{array} if \code{nifti}
#' is not supplied
#' @note \code{apply_empty_dim} is a shorthand for 
#' \code{applyEmptyImageDimensions} with all the same arguments.
#' @seealso \code{\link{getEmptyImageDimensions}}, 
#' \code{\link{dropEmptyImageDimensions}} 
#' @export
setGeneric("applyEmptyImageDimensions", 
           function(img, 
                    inds,
                    reorient = FALSE,
                    ...) standardGeneric("applyEmptyImageDimensions"))

.applyEmptyImageDimensions <- function(img, 
                                       inds,
                                       reorient = FALSE,
                                       ...) {
  dimg = dim(img)
  if (length(dimg) > 3) {
    stop(paste0("Only images with 3 dimensions supported, ", 
                "as checked by length(dim(img))"))
  }
  i2 = img[inds[[1]], inds[[2]], inds[[3]]] 
  return(i2)
}

#' @rdname applyEmptyImageDimensions-methods
#' @aliases applyEmptyImageDimensions,nifti-method
#' @export
setMethod("applyEmptyImageDimensions", "nifti", 
          function(img,  
                   inds,
                   ...) {
            res = .applyEmptyImageDimensions(
              img = img, inds = inds, ...)
            res = copyNIfTIHeader(img = img, 
                                  arr = res, 
                                  drop = TRUE)  
            return(res)
          })

#' @rdname applyEmptyImageDimensions-methods
#' @aliases applyEmptyImageDimensions,nifti-method
#' @export
setMethod("applyEmptyImageDimensions", "character", 
          function(img,  
                   inds,
                   reorient = FALSE,
                   ...) {
            img = check_nifti(img, reorient = reorient)
            res = applyEmptyImageDimensions(img, inds = inds, ...)
            return(res)
          })


#' @rdname applyEmptyImageDimensions-methods
#' @aliases applyEmptyImageDimensions,factor-method
#'  
#' @export
setMethod("applyEmptyImageDimensions", "factor", 
          function(img,  
                   inds,
                   reorient = FALSE,
                   ...) { 
            img = as.character(img)
            res = applyEmptyImageDimensions(img = img,  inds = inds, ...)
            return(res)
          })


#' @rdname applyEmptyImageDimensions-methods
#' @aliases applyEmptyImageDimensions,list-method
#' @export
setMethod("applyEmptyImageDimensions", "list", 
          function(img,  
                   inds,
                   ...) { 
            ### add vector capability
            res = lapply(img, applyEmptyImageDimensions, 
                         inds = inds, 
                          ...)
            return(res)
          })


#' @rdname applyEmptyImageDimensions-methods
#' @aliases applyEmptyImageDimensions,array-method
#' @export
setMethod("applyEmptyImageDimensions", "array", 
          function(img,  
                   inds,
                   ...) { 
            res = .applyEmptyImageDimensions(
              img = img,  inds = inds, ...)            
            return(res)
          })


#' @rdname applyEmptyImageDimensions-methods
#' @aliases applyEmptyImageDimensions,anlz-method
#' @export
setMethod("applyEmptyImageDimensions", "anlz", 
          function(img,  
                   inds,
                   ...) { 
            img = as.nifti(img)
            res = applyEmptyImageDimensions(img = img, inds = inds, ...)
            return(res)
          })

#' @rdname applyEmptyImageDimensions-methods
#' @aliases applyEmptyImageDimensions,ANY-method
#' @export
setMethod("applyEmptyImageDimensions", "ANY", 
          function(img,  
                   inds,
                   reorient = FALSE,
                   ...) {
            # workaround because can't get class
            if (inherits(img, "niftiImage")) {
              img = check_nifti(img, reorient = reorient)
              res = applyEmptyImageDimensions(img = img, inds = inds,  ...)
              return(res)              
            } else {
              stop("Not implemented for this type!")
            }
            return(img)
          })




#' @rdname applyEmptyImageDimensions-methods
#' @export
apply_empty_dim <- function(img, 
                            ...) {
  applyEmptyImageDimensions(img = img, ...)
}
