#' @title readNIfTI with default non-reorientation
#' 
#' @description  This function calls the \code{\link[oro.nifti]{readNIfTI}} function from the 
#' \code{oro.nifti} package, but sets the reorientation to \code{FALSE} by default
#' @param ... Arguments to pass to \code{\link[oro.nifti]{readNIfTI}}
#' @param reorient Reorientation argument to pass to \code{\link[oro.nifti]{readNIfTI}}
#' @return \code{nifti} object
#' @rdname readNIfTI2
#' @export
readNIfTI2 <- function(..., reorient = FALSE){
  oro.nifti::readNIfTI(..., reorient = reorient)
}

#' @rdname readNIfTI2
#' @param dtype Should \code{\link{datatyper}} be run after reading?
#' @param drop_dim Should \code{\link[oro.nifti]{drop_img_dim}} be run after reading?
#' @param warn Should warnings from \code{\link[oro.nifti]{readNIfTI}} be 
#' printed?  If not, \code{\link{suppressWarnings}} is called.  Also passed to 
#' \code{\link{datatyper}}
#' @param reset_slope Reset slope/intercept of image
#' @param rm_extensions should niftiExtensions be converted to simple
#' nifti objects?
#' @export
#' @importFrom oro.nifti is.niftiExtension
readnii <- function(..., reorient = FALSE, dtype = TRUE, 
                    drop_dim = TRUE,
                    reset_slope = FALSE,
                    warn = FALSE,
                    rm_extensions = TRUE){
  if (warn) {
    nim = oro.nifti::readNIfTI(..., reorient = reorient)
  } else {
    suppressWarnings({
      nim = oro.nifti::readNIfTI(..., reorient = reorient)
    })
  }
  if (drop_dim) {
    nim = oro.nifti::drop_img_dim(nim)
  }
  if (dtype) {
    nim = datatyper(nim, warn = warn)
  }
  if (rm_extensions) {
    if (is.niftiExtension(nim)) {
      nim = as.nifti(nim)
    } 
  }
  if (reset_slope) {
    nim = oro.nifti::resetSlopeIntercept(nim)
  }
  
  return(nim)
}
