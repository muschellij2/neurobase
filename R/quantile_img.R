#' @title Create Quantile Image
#' @description Creates output image of the quantiles that each voxel is in, 
#' after applying the mask
#' @param img Character vector, or object of class \code{nifti}
#' @param mask Mask to determine cumulative distribution function (cdf) from
#' @param ... Additional arguments to pass to \code{\link{ecdf}}
#' @return Object of class \code{nifti}
#' @export
#' @examples 
#' set.seed(5)
#' dims = rep(10, 3)
#' arr = array(rnorm(prod(dims)), dim = dims)
#' nim = oro.nifti::nifti(arr)
#' qimg = quantile_img(nim)
#' qarr = quantile_img(arr)
#' testthat::expect_equal(qarr, array(qimg, dim = dim(qarr)))
#' qimg = quantile_img(nim, mask = nim > 0)
#' 
quantile_img = function(img, 
                        mask = NULL,
                        ...)
{
  img = check_nifti(img, allow.array = TRUE)
  
  if (!is.null(mask)) {
    mask = check_nifti(mask, allow.array = TRUE)
  } else { 
    mask = array(1, dim = dim(img))
  }
  
  check_mask_fail(mask, allow.NA = FALSE)
  
  vals = img[mask == 1]
  e = ecdf(vals)
  if (is.nifti(img)) {
    qimg = niftiarr(img, e(c(img)))
  } else {
    qimg = array(e(c(img)), dim = dim(img))
  }
  
  return(qimg)
}
