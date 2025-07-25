#' @title writeNIfTI with default non-reorientation
#' 
#' @description  This function calls the \code{\link[oro.nifti]{writeNIfTI}} function from the 
#' \code{oro.nifti} package, but makes sure to remove \code{.nii} extension and
#' warnings can be suppressed.
#' @param nim object of class \code{nifti}, passed to \code{\link[oro.nifti]{writeNIfTI}}
#' @param filename path to save the NIfTI file.  Suffix will be removed
#' @param dtype Should \code{\link{datatyper}} be run before writing?
#' @param compression compression level for gzipped files. 
#' @param ... Additional arguments passed to \code{\link[oro.nifti]{writeNIfTI}}
#' @note While \code{writeNIfTI2} does not run \code{\link{datatyper}} as default, 
#' \code{writenii} does.  Additional functionality will be added to \code{writenii} likely
#' but will not to \code{writeNIfTI2}
#' @return Nothing
#' @rdname writeNIfTI2
#' @export
#' @importFrom RNifti writeNifti
#' @examples 
#' set.seed(5)
#' dims = rep(10, 3)
#' arr = array(rnorm(prod(dims)), dim = dims)
#' nim = oro.nifti::nifti(arr)
#' rnifti = RNifti::asNifti(nim)
#' tfile = tempfile(fileext = ".nii.gz")
#' timg = writenii(nim, tfile, rm_extensions = TRUE, warn = TRUE)
#' timg = writeNIfTI2(nim, tfile, dtype = TRUE)
writeNIfTI2 <- function(nim, filename, dtype = FALSE, 
                        compression = 9,
                        ...){
  if (dtype) {
    nim = datatyper(nim)
  }
  oro.nifti::writeNIfTI(nim, nii.stub(filename), 
                        compression = compression, ...)
}

#' @rdname writeNIfTI2
#' @param drop_dim Should \code{\link[oro.nifti]{drop_img_dim}} be run before writing?
#' @param warn Should warnings from \code{\link[oro.nifti]{writeNIfTI}} be 
#' printed?  If not, \code{\link{suppressWarnings}} is called 
#' @param rm_extensions should niftiExtensions be converted to simple
#' nifti objects before writing?
#' @export
#' @importFrom oro.nifti as.nifti is.niftiExtension
writenii <- function(nim, filename, 
                     dtype = TRUE, 
                     drop_dim = TRUE, 
                     warn = FALSE, 
                     compression = 9,
                     rm_extensions = TRUE,
                     ...){
  if (drop_dim) {
    nim = oro.nifti::drop_img_dim(nim)
  }
  if (dtype) {
    nim = datatyper(nim, warn = warn)
  }
  if (oro.nifti::is.niftiExtension(nim)) {
    if (rm_extensions) {
      nim = as.nifti(nim)
    }
  }
  if (warn) {
    x = oro.nifti::writeNIfTI(nim, nii.stub(filename), 
                              compression = compression, ...)
  } else {
    suppressWarnings({
      x = oro.nifti::writeNIfTI(nim, nii.stub(filename), 
                                compression = compression,  ...)
    })
  }
  return(invisible(x))
}
