#' @title Remake Image from Vector
#' @description Wrapper function to take a vector of values and result in a 
#' \code{\link[oro.nifti]{nifti}} object
#' @param vec vector of values to be in resulting image
#' @param img object of class \code{\link[oro.nifti]{nifti}} to put vector into
#' @param mask binary array/ \code{\link[oro.nifti]{nifti}} object to denote where
#' vector values are to be.
#' @param warn Should a warning be issued if defaulting to FLOAT32?
#' @param ... additional arguments passed to \code{\link{datatyper}}
#' @seealso \code{\link{niftiarr}}
#' @return Object of class \code{\link[oro.nifti]{nifti}}
#' @export
#' @examples 
#' set.seed(5)
#' dims = rep(10, 3)
#' arr = array(rnorm(prod(dims)), dim = dims)
#' arr[,,10] = 0
#' nim = oro.nifti::nifti(arr)
#' remake_img(c(nim), nim)
#' mask = nim > 5
#' vals = nim[mask]
#' vals = sqrt(vals)
#' remake_img(vals, nim, mask = mask)
#' 
remake_img = function(vec, img, mask = NULL, warn = FALSE, ...){
  if (is.null(mask)) {
    mask = array(1, dim = dim(img))
  }
  img2 = niftiarr(img, 0)
  # check_mask_fail(mask, allow.array = TRUE)
  # stopifnot(same_dims(mask, img))
  mask_vals(object = img2, mask = mask) = vec
  
  # arr = array(0, dim = dim(img2))
  # arr[ mask == 1 ] = vec
  # img_data(img2) = arr
  # img2[mask == 1] = vec
  img2 = datatyper(img2, warn = warn, ...)
  img2 = cal_img(img2)
  return(img2)
}
