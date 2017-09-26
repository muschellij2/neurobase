#' @name check_nifti_header-methods
#' @docType methods 
#' 
#' @aliases check_nifti_header 
#' @title Check if nifti image or read in a nifti header
#' @description Simple check to see if input is character or of 
#' class nifti and read in the header
#' @return nifti object or character
#' 
#' @param x character path of image or 
#' an object of class nifti, or array
#' @export 
#' @author John Muschelli \email{muschellij2@@gmail.com} 
setGeneric("check_nifti_header", 
           function(x) {
             standardGeneric("check_nifti_header")
           })

#' @rdname check_nifti_header-methods
#' @aliases check_nifti_header,nifti-method
#' @export
setMethod("check_nifti_header", "nifti", 
          function(x) { 
            return(x)
          })



#' @rdname check_nifti_header-methods
#' @aliases check_nifti_header,character-method
#'  
#' @export
setMethod("check_nifti_header", "character", 
          function(x) { 
            ### add vector capability
            if (length(x) > 1) {
              file = lapply(x, check_nifti_header)
              return(file)
            } else {
              file = oro.nifti::nifti_header(x)
              return(file)
            }
          })

#' @rdname check_nifti_header-methods
#' @aliases check_nifti_header,factor-method
#'  
#' @export
setMethod("check_nifti_header", "factor", function(
  x) { 
  x = as.character(x)
  return(check_nifti_header(x))
})


#' @rdname check_nifti_header-methods
#' @aliases check_nifti_header,list-method
#' @export
setMethod("check_nifti_header", "list", 
          function(x) { 
            ### add vector capability
            file = lapply(x, check_nifti_header)
            return(file)
          })


#' @rdname check_nifti_header-methods
#' @aliases check_nifti_header,array-method
#' @export
setMethod("check_nifti_header", "array", 
          function(x) { 
            stop("x is array, has no nifti header")
            return(x)
          })


#' @rdname check_nifti_header-methods
#' @aliases check_nifti_header,anlz-method
#' @export
setMethod("check_nifti_header", "anlz", 
          function(x) { 
            x = as.nifti(x)
            return(x)
          })

#' @rdname check_nifti_header-methods
#' @aliases check_nifti_header,ANY-method
#' @export
setMethod("check_nifti_header", "ANY", 
          function(x) {
            # workaround because can't get class
            if (inherits(x, "niftiImage")) {
              x = oro.nifti::nii2oro(x)
            } else {
              stop("Not implemented for this type!")
            }
            return(x)
          })