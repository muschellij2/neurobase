#' @title Mask Image
#'
#' @description Takes an image, masks it by 
#' \code{mask}, and returns an object of class \code{nifti}
#' @param img object of class \code{nifti}
#' @param mask array or object of class \code{nifti}, same dimensions as img
#' @param allow.NA allow NAs in the mask
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
#' @export
mask_img <- function(img, # object of class \code{nifti}
                     mask, # array or object of class \code{nifti}
                     allow.NA = TRUE # allow NAs in the mask
){
  UseMethod("mask_img")
}


#' @export
#' @method mask_img default
mask_img.default = function(img, mask, allow.NA = TRUE){
  
  img = check_nifti(img)
  stopifnot(inherits(img, "nifti"))
  check_mask_fail(mask = mask, allow.NA = allow.NA, allow.array = TRUE)
  
  niftiarr(img, img * mask)
}


#' @export
#' @method mask_img nifti
mask_img.nifti = function(img, mask, allow.NA = TRUE){
  mask = ensure_array(mask)
  mask_img.default(img = img, mask = mask, allow.NA = allow.NA)
}

#' @export
#' @method mask_img anlz
mask_img.anlz = function(img, mask, allow.NA = TRUE){
  img = as.nifti(img)
  mask_img.nifti(img = img, mask = mask, allow.NA = allow.NA)
}

#' @export
#' @method mask_img array
mask_img.array = function(img, mask, allow.NA = TRUE){
  mask = ensure_array(mask)
  check_mask_fail(mask = mask, allow.NA = allow.NA, allow.array = TRUE)
  img * mask
}

#' @export
#' @method mask_img niftiImage
mask_img.niftiImage = function(img, mask, allow.NA = TRUE){
  mask = ensure_array(mask)
  check_mask_fail(mask = mask, allow.NA = allow.NA, allow.array = TRUE)
  img * mask
}

