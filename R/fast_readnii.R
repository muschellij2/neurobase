#' @title Reading NIfTI images through RNifti
#' 
#' @description  This function calls the \code{\link{readNifti}} function from the 
#' \code{RNifti} package, and then converts the image to a \code{nifti} object
#' @param fname file name of the NIfTI file.
#' @param dtype Should \code{\link{datatyper}} be run after reading?
#' @param drop_dim Should \code{\link{drop_img_dim}} be run after reading?
#' 
#' @return A \code{nifti} object
#' @export
#' @importFrom RNifti readNifti
fast_readnii <- function(
  fname, 
  dtype = TRUE, 
  drop_dim = TRUE){
  # nii = RNifti::retrieveNifti(fname)
  nim = RNifti::readNifti(file = fname)
  nim = oro.nifti::nii2oro(nim)
  
  if (drop_dim) {
    nim = oro.nifti::drop_img_dim(nim)
  }
  if (dtype) {
    nim = datatyper(nim)
  }
  return(nim)
}