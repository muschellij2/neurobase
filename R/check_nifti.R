#' @name check_nifti-methods
#' @docType methods 
#' @aliases check_nifti 
#' @title Check if nifti image or read in a nifti image
#' @description Simple check to see if input is character or of 
#' class nifti
#' @return nifti object or array if allow.array=TRUE and x is an array
#' @seealso \link{readnii}
#' @param x character path of image or 
#' an object of class nifti, or array
#' @param reorient (logical) passed to \code{\link{readnii}} 
#' if the image
#' is to be re-oriented
#' @param allow.array (logical) Are array types allowed (TRUE) or
#' should there be an error if the object is not character or class
#' nifti.
#' @param fast if \code{TRUE}, then \code{\link{fast_readnii}} will be used 
#' versus \code{\link{readnii}} if the files need to be read in.
#' @param need_header if \code{TRUE}, then an image type with header information
#' will be returned.  If not, then an array is fine.  Used really only in 
#' conjunction with \code{allow.array}
#' @param ... additional arguments to pass to \code{\link{readnii}} if relevant
#' @export 
#' @author John Muschelli \email{muschellij2@@gmail.com} 
#' 
#' @examples 
#' x = nifti()
#' check_nifti(x)
#' set.seed(5)
#' dims = rep(10, 4)
#' arr = array(rpois(prod(dims), lambda = 2), dim = dims)
#' nim = oro.nifti::nifti(arr)
#' check_nifti(nim)
#' check_nifti(as.anlz(nim))
#' testthat::expect_error(check_nifti(arr, allow.array = FALSE))
#' tfile = tempimg(nim)
#' check_nifti(c(tfile, tfile))
#' check_nifti(list(tfile, tfile))
#' check_nifti(factor(c(tfile, tfile)))
#' check_nifti(RNifti::readNifti(tfile))
setGeneric("check_nifti", 
           function(x, reorient=FALSE, 
                    allow.array=FALSE,
                    fast = FALSE,
                    need_header = TRUE, 
                    ...) standardGeneric("check_nifti"))

#' @rdname check_nifti-methods
#' @aliases check_nifti,nifti-method
#' @export
setMethod("check_nifti", "nifti", 
          function(x, 
                   reorient=FALSE, 
                   allow.array=FALSE,
                   fast = FALSE,
                   need_header = TRUE, 
                   ...) { 
            return(x)
          })



#' @rdname check_nifti-methods
#' @aliases check_nifti,character-method
#'  
#' @export
setMethod("check_nifti", "character", 
          function(x, 
                   reorient=FALSE, 
                   allow.array=FALSE,
                   fast = FALSE,
                   need_header = TRUE, 
                   ...) { 
            ### add vector capability
            if (length(x) > 1) {
              file = lapply(x, check_nifti,  
                            reorient = reorient, 
                            allow.array = allow.array,
                            fast = fast,
                            need_header = need_header,
                            ... = ...)
              return(file)
            } else {
              if (fast) {
                file = fast_readnii(x, ...)
              } else {
                file = readnii(x, reorient = reorient, ...)
              }
              return(file)
            }
          })

#' @rdname check_nifti-methods
#' @aliases check_nifti,factor-method
#'  
#' @export
setMethod("check_nifti", "factor", function(
  x, 
  reorient=FALSE, 
  allow.array=FALSE,
  fast = FALSE,
  need_header = TRUE,
  ...) { 
  x = as.character(x)
  return(
    check_nifti(
      x,
      reorient = reorient,
      allow.array = allow.array,
      fast = fast,
      need_header = need_header,
      ...
    )
  )
})


#' @rdname check_nifti-methods
#' @aliases check_nifti,list-method
#' @export
setMethod("check_nifti", "list", 
          function(x,  
                   reorient=FALSE, 
                   allow.array=FALSE,
                   fast = FALSE,
                   need_header = TRUE,
                   ...) { 
            ### add vector capability
            file = lapply(x, check_nifti, 
                          reorient = reorient, 
                          allow.array = allow.array,
                          fast = fast,
                          need_header = need_header,
                          ... = ...)
            return(file)
          })


#' @rdname check_nifti-methods
#' @aliases check_nifti,array-method
#' @export
setMethod("check_nifti", "array", 
          function(x,  
                   reorient=FALSE, 
                   allow.array=FALSE,
                   fast = FALSE,
                   need_header = FALSE,
                   ...) { 
            if (!allow.array) {
              stop("x is array but allow.array = FALSE")
            }
            if (need_header) {
              stop("x is array but header is needed")
            }
            return(x)
          })


#' @rdname check_nifti-methods
#' @aliases check_nifti,anlz-method
#' @export
setMethod("check_nifti", "anlz", 
          function(x,  
                   reorient=FALSE, 
                   allow.array=FALSE,
                   fast = FALSE,
                   need_header = TRUE,
                   ...) { 
            
            x = as.nifti(x)
            return(x)
          })

#' @rdname check_nifti-methods
#' @aliases check_nifti,ANY-method
#' @export
setMethod("check_nifti", "ANY", 
          function(x, 
                   reorient=FALSE, 
                   allow.array=FALSE,
                   fast = FALSE,
                   need_header = TRUE,
                   ...) {
            # workaround because can't get class
            if (inherits(x, "niftiImage")) {
              if (!allow.array) {
                x = oro.nifti::nii2oro(x)
              }
            } else if (inherits(x, "NiftiArray")) {
              if (!allow.array) {
                x = methods::as(x, "niftiImage")
                x = oro.nifti::nii2oro(x)
              }
            } else {
              stop("Not implemented for this type!")
            }
            return(x)
          })
