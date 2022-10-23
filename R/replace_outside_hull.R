#' @title Replace Values Outside Surface of image
#' @description Determines values outside the surface of an image and gives a 
#' mask back with those values set to a replacement.
#'  
#' @param img nifti object or array
#' @param value Value to check against.  If zero, then 
#' \code{replace_outside_surface} will include any dimension that has 
#' fewer than \code{threshold} zeroes. May be a vector of values, matched with 
#' \code{\link{match}}
#' @param threshold Include dimension if fewer than \code{threshold} voxels
#' are in the slice
#' @param replace_value Value to replace those outside the surface.  
#' @param reorient Should image be reoriented if a filename?  
#' Passed to \code{\link{check_nifti}}
#' @return Creates an array of binary values.  If \code{img} is a \code{nifti}
#' object, then a \code{nifti} is returned
#' @export
#' @importFrom matrixStats rowCumsums colCumsums
#' @examples 
#' set.seed(5)
#' dims = rep(10, 3)
#' arr = array(0, dim = dims)
#' 
#' arr[ 3:5, 4:6, c(2, 6:8, 5)] = 1
#' nim = oro.nifti::nifti(arr)

#' out = replace_outside_surface(nim, replace_value = 0)
#' out_arr = replace_outside_surface(arr, replace_value = 0)
#' testthat::expect_equal(out_arr, array(out, dim = dim(out)))
replace_outside_surface <- function(
  img, 
  value = 0, 
  threshold = 0,
  replace_value = NA,
  reorient = FALSE) {
  
  img = check_nifti(img, reorient = reorient, allow.array = TRUE)
  ximg = img
  dimg = dim(img)
  if (length(dimg) > 3) {
    stop(paste0("Only images with 3 dimensions supported, ", 
                "as checked by length(dim(img))"))
  }
  ############################
  # Set NAs to 0
  ############################
  img = as.array(img)
  img[is.na(img)] = 0
  
  
  ############################
  # Get indices for slices with all zeros (or of certain value)
  ############################
  # inds = vector(mode = "list", length = 3)
  bin_img = array(!(c(img) %in% value), dim = dim(img))
  rev_col = function(x) {
    x = x[, seq(ncol(x), 1)]
  }
  rev_row = function(x) {
    x = x[seq(nrow(x), 1), ]
  }  
  rev_both = function(x) {
    rev_col(rev_row(x))
  }
  for ( i in seq(dim(bin_img)[3])) {
    x = bin_img[,,i]
    class(x) = "numeric" 
    # dzero_x = colCumsums(x) <= threshold | 
    #   rev_col(colCumsums(rev_col(x)) <= threshold)
    # dzero_y = rowCumsums(x) <= threshold | 
    #   rev_row(rowCumsums(rev_row(x)) <= threshold)    
    dzero_x = colCumsums(x) <= threshold
    dzero_y = rowCumsums(x) <= threshold
    x = rev_both(x)
    dzero_x = dzero_x | rev_both(colCumsums(x) <= threshold)
    dzero_y = dzero_y | rev_both(rowCumsums(x) <= threshold)
    mask = !(dzero_y | dzero_x)
    x = rev_both(x)
    if (any(x[!mask] == 1)) {
      stop("mask is throwing out voxels!  Please report this bug")
    }
    x = bin_img[,,i]
    x[!mask] = replace_value
    bin_img[,,i] = x    
  }
  if (is.nifti(ximg)) {
    bin_img = copyNIfTIHeader(bin_img, img = ximg)
  } 
  return(bin_img)
}
