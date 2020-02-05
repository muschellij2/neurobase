#' @title Retrieve Image Indices
#' @description Extract image xyz indices (in voxels or millimeters), with the
#' option to append the values
#' 
#' @param img Object of class \code{nifti}
#' @param mask Mask to be applied for indices the index
#' @param add_values Should the value be column-bound to the matrix
#' @param units Should the indices be in xyz-coordinates or millimeters. 
#'
#' @return Matrix of 3 columns if \code{add_values = FALSE} or 4 columns, 
#' otherwise.
#' @export
#' @examples 
#' set.seed(5)
#' dims = rep(10, 4)
#' arr = array(rpois(prod(dims), lambda = 2), dim = dims)
#' nim = oro.nifti::nifti(arr)
#' ind = img_indices(nim)
#' ind2 = img_indices(nim, mask = nim > 2)
#' # 3d example
#' set.seed(5)
#' dims = rep(10, 3)
#' arr = array(rpois(prod(dims), lambda = 2), dim = dims)
#' nim = oro.nifti::nifti(arr)
#' ind = img_indices(nim)
#' ind2 = img_indices(nim, mask = nim > 2)
#' testthat::expect_equal(colnames(ind2), c("x", "y", "z"))
#' ind2 = img_indices(nim, mask = nim > 2, add_values = TRUE)
#' testthat::expect_equal(colnames(ind2), c("x", "y", "z", "value"))
img_indices <- function(img, mask = NULL, 
                        add_values = FALSE,
                        units = c("index", "mm")){
  units = match.arg(units)
  if (is.null(mask)) {
    img = check_nifti(img, allow.array = TRUE)
    dimg = dim(img)
    df <- expand.grid(x = seq(dimg[1]),
                      y = seq(dimg[2]),
                      y = seq(dimg[3]))
    df = as.matrix(df)
  } else {
    check_mask_fail(mask, allow.array = TRUE)
    df <- which(mask == 1, arr.ind = TRUE)
  }
  
  vals = NULL
  if (add_values) {
    vals = img[df]
  }
  
  if (units == "mm") {
    vdim = voxdim(img)
    if (length(vdim) != 3) {
      stop("Number of voxel dimensions are not equal to 3")
    }
    df = t( t(df) * vdim )
  }
  colnames(df) <- c("x", "y", "z", "t")[ 1:ncol(df) ]
  df = cbind(df, value = vals)
  df
}
