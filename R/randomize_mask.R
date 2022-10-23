#' @title Randomize Image based on Mask
#' @description Randomize the values within a mask
#' @param img Object of class nifti with values to be randomized
#' @param mask Binary mask indicating which values should be randomized.
#'
#' @return Object of class nifti
#' @export
#'
#' @examples
#'   set.seed(5)
#'   dims = rep(10, 3)
#'   arr = array(rnorm(prod(dims)), dim = dims)
#'   nim = oro.nifti::nifti(arr)
#'   mask = abs(nim) > 1
#'   randomize_mask(nim, mask)
randomize_mask = function(img, mask){
  img = check_nifti(img)
  check_mask_fail(mask, allow.NA = FALSE)
  ind = as.integer(mask) %in% 1
  vals = img[ ind ]
  vals = sample(vals)
  img[ ind ] = vals
  return(img)
}