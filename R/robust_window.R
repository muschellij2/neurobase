#' @title Window image based on quantiles of Image
#'
#' @description Takes an image, finds the quantiles given by 
#' \code{probs}, then sets values outside these values to other 
#' values, as determined by \code{replace} argument passed to
#' to \code{\link{window_img}}
#' @param img object of class nifti
#' @param probs quantiles to constrain the image these define the window sent to \code{\link{window_img}}
#' @param ... additional arguments sent to \code{\link{window_img}}
#' @param mask binary image to use to to calculate quantiles
#' @param non_zero Should zeroes be excluded from the calculation
#' of quantiles?
#' @export
#' @return Object of class nifti with values outside quantiles replaced
#' by values depending on replace argument passed to \code{\link{window_img}}
robust_window <- function(img, # object of class nifti
                          non_zero = FALSE,                          
                          probs = c(0, 0.999), # quantiles to constrain the image, these define the window sent to \code{\link{window_img}}
                          ..., # additional arguments sent to \code{\link{window_img}}
                          mask = NULL
){
  if (is.null(mask)) {
    cc = c(img)
  } else {
    cc = mask_vals(img, mask)
  }
  if (non_zero) {
    cc = cc[ cc != 0 ]
  }
  quant = quantile(cc, probs = probs, ...)
  img = window_img(img, window = quant, ...)
  if (oro.nifti::is.nifti(img)) {
    img = cal_img(img)
  }
  img
}
