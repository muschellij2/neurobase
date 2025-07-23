#' @title General NIfTI Writer
#' @description Writes out NIfTI files for multiple formats.  Currently,
#' for \code{nifti} objects and \code{niftiImage} objects from \code{RNifti}
#' 
#' @param nim Container for NIfTI Image
#' @param filename Filename of image to be written out
#' @param ... additional arguments, to be passed to \code{\link[RNifti]{writeNifti}} or
#' \code{\link{writenii}}
#' 
#' @return Output from NIfTI writer
#' @export 
#' @examples 
#' set.seed(5)
#' dims = rep(10, 4)
#' arr = array(rpois(prod(dims), lambda = 2), dim = dims)
#' nim = oro.nifti::nifti(arr)
#' tfile = tempfile(fileext = ".nii.gz")
#' write_nifti(nim, tfile)
#' rimg = RNifti::readNifti(tfile)
#' write_nifti(rimg, tfile)
write_nifti <- function(nim, 
                        filename,
                        ...) {
  UseMethod("write_nifti")
}


#' @export
#' @method write_nifti nifti
write_nifti.nifti = function(nim, 
                             filename,
                             ...){
  writenii(nim = nim, filename = filename, ...)
}

#' @export
#' @method write_nifti anlz
write_nifti.anlz = function(nim, 
                            filename,
                            ...){
  nim = as.nifti(nim)
  writenii(nim = nim, filename = filename, ...)
}

#' @export
#' @method write_nifti niftiImage
write_nifti.niftiImage = function(nim, 
                                  filename,
                                  ...){
  writeNifti(image = nim, file = filename, ...)
}

#' @export
#' @method write_nifti default
write_nifti.default <- function(nim, 
                                filename,
                                ...) {
  stop("write_nifti not implemented for class yet!")
}
