#' @title Mask nifti Image
#'
#' @description Takes an image, masks it by 
#' \code{mask}, and returns an object of class \code{nifti}
#' @param img object of class \code{nifti}
#' @param mask array or object of class \code{nifti}, same dimensions as img
#' @param allow.NA allow NAs in the mask
#' @export
#' @return Object of class nifti with values outside mask set to 0 if mask is
#' 0 and NA if mask is NA and img if mask == 1
#' @examples 
#'   set.seed(5)
#'   dims = rep(10, 3)
#'   arr = array(rnorm(prod(dims)), dim = dims)
#'   nim = oro.nifti::nifti(arr)
#'   mask = abs(nim) > 1
#'   masked = mask_img(nim, mask)
#'   mask[mask == 0] = NA
#'   na_masked = mask_img(nim, mask, allow.NA = TRUE)
#'   
#'   
mask_img <- function(img, # object of class \code{nifti}
                    mask, # array or object of class \code{nifti}
                    allow.NA = TRUE # allow NAs in the mask
){
  img = check_nifti(img)
  stopifnot(inherits(img, "nifti"))
  check_mask_fail(mask = mask, allow.NA = allow.NA, allow.array = TRUE)
  
  niftiarr(img, img * mask)
}