#' Fast Dice Tabulation
#'
#' @param x A \code{nifti} image, filename, or \code{niftiImage}
#' @param y A \code{nifti} image, filename, or \code{niftiImage}
#' @param verbose A logical indicating output 
#'
#' @return A table object
#' @export
#' @importFrom RNifti retrieveNifti
#'
#' @examples
#' library(oro.nifti)
#' set.seed(20161007)
#' dims = rep(10, 3)
#' arr = array(rnorm(10*10*10), dim = dims)
#' nim = oro.nifti::nifti(arr) > -1
#' fast_dice_tab(nim, nim)
#' fast_dice(nim, nim) == 1
fast_dice_tab = function(x, y) {
  get_array = function(img) {
    if (is.nifti(img)) {
      img = oro.nifti::zero_trans(img)
    }
    as(retrieveNifti(img), "array")
  }
  
  x = c(get_array(x) > 0)
  y = c(get_array(y) > 0)
  tt = sum(x & y)
  t1 = sum(x)
  t2 = sum(y)
  tab = matrix(c(length(x) - t1 - t2 + tt,  t1 - tt, t2 - tt, tt), 2, 2)
  n = list(c("FALSE", "TRUE"), c("FALSE", "TRUE"))
  names(n) = c("x", "y")
  dimnames(tab) = n
  tab = as.table(tab)
  tab
}

#' @export
#' @rdname fast_dice_tab
fast_dice = function(x, y, verbose = FALSE) {
  tab = fast_dice_tab(x, y)
  dicer(tab, verbose = verbose)
}
