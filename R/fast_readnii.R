#' @title Reading NIfTI images through RNifti
#' 
#' @description  This function calls the \code{\link[RNifti]{readNifti}} function from the 
#' \code{RNifti} package, and then converts the image to a \code{nifti} object
#' @param fname file name of the NIfTI file.
#' @param dtype Should \code{\link{datatyper}} be run after reading?
#' @param drop_dim Should \code{\link[oro.nifti]{drop_img_dim}} be run after reading?
#' 
#' @return A \code{nifti} object
#' @export
#' @importFrom RNifti readNifti
#' @examples 
#' set.seed(5)
#' dims = rep(10, 4)
#' arr = array(rpois(prod(dims), lambda = 2), dim = dims)
#' nim = oro.nifti::nifti(arr)
#' tfile = tempfile(fileext = ".nii.gz")
#' write_nifti(nim, tfile)
#' rimg = fast_readnii(tfile)
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
