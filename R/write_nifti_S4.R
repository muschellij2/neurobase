#' @name write_nifti-methods
#' @docType methods 
#' @aliases write_nifti 
#' @title General NIfTI Writer
#' @description Writes out NIfTI files for multiple formats.  Currently,
#' for \code{nifti} objects and \code{niftiImage} objects from \code{RNifti}
#' @seealso \link{readnii}
#' @param nim Container for NIfTI Image
#' @param filename Filename of image to be written out
#' @param ... additional arguments, to be passed to \code{\link{writeNifti}} or
#' \code{\link{writenii}}
#' 
#' @return Output from NIfTI writer
#' @export 
#' @importFrom RNifti writeNifti
#' @importClassesFrom RNifti niftiImage
setGeneric("write_nifti", function(nim, 
                                   filename,
                                   ...) {
  standardGeneric("write_nifti")
})

#' @rdname write_nifti-methods
#' @aliases write_nifti,nifti-method
#' @export
setMethod("write_nifti", "nifti", function(nim, 
                                           filename,
                                           ...){
  res = writenii(nim = nim, filename = filename, ...)
  return(res)
})

#' @rdname write_nifti-methods
#' @aliases write_nifti,anlz-method
#' @export
setMethod("write_nifti", "anlz", function(nim, 
                                           filename,
                                           ...){
  nim = as.nifti(nim)
  res = writenii(nim = nim, filename = filename, ...)
  return(res)
})

#' @rdname write_nifti-methods
#' @aliases write_nifti,niftiImage-method
#' @export
setMethod("write_nifti", "niftiImage", function(nim, 
                                           filename,
                                           ...){
  res = writeNifti(image = nim, file = filename, ...)
  return(res)
})