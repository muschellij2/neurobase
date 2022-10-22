#' @rdname mask_vals-methods
#' @docType methods 
#' @title Extract or Replace Values Inside a Mask
#' @description Methods that act on the \code{.Data} field in a
#' NIfTI/ANALYZE image but only on values inside a mask.
#'  
#' @param object is an object of class \code{nifti} or \code{anlz}.
#' @param mask is an object of class \code{nifti} or \code{anlz}.
#' @param value is the value to assign to the \code{.Data} field.  
#' @aliases mask_vals-methods, mask_vals
#'
#' @export
#' @examples 
#' set.seed(2022)
#' img = nifti(array(rnorm(10^3), dim = rep(10, 3)))
#' mask = img > 1.5
#' mask_vals(img, mask)
#'testthat::expect_equal(sum(mask_vals(img, mask)), 117.628200302518)
#' mask_vals(img, mask) = rep(4, sum(mask))
#' mask_vals(img, as(mask, "array")) = rep(4, sum(mask))
#' mask_vals(as(img, "array"), 
#'     as(mask, "array")) = rep(4, sum(mask))
mask_vals =  function(object, mask) {
  object = check_nifti(object, allow.array = TRUE)
  mask = check_nifti(mask, allow.array = TRUE)
  check_mask_fail(mask, 
                  allow.NA = TRUE, 
                  allow.array = TRUE)
  same_dim = same_dims(object, mask)
  if (!same_dim) {
    stop("Dimensions of Mask and Image are not the same")
  }
  vals = object[as.integer(mask) %in% 1]
  return(vals)  
}




#' @rdname mask_vals-methods
#' @aliases mask_vals<- 
#' @export
setGeneric("mask_vals<-", function(object, mask, value) standardGeneric("mask_vals<-") )

.quick_check = function(mask, value) {
  check_mask_fail(mask, allow.NA = TRUE, allow.array = TRUE)
  ind = as.integer(mask) %in% 1
  n_ones = sum(ind)
  value = c(value)
  n_value = length(value)
  if (n_value == 1) {
    value = rep(value, n_ones)
  } 
  n_value = length(value)
  if (n_value != n_ones) {
    stop(paste0("Number of replacement values ", 
                "do not match the number of indices in ",
                "mask"))
  }
  return(list(ind = ind, value = value))
}

.mask_vals <- function(object, mask, value) { 
  L = .quick_check(mask, value)
  value = L$value
  ind = L$ind
  object[ind] = value
  return(object)
}

.cal_mask_vals = function(object, mask, value) {
  object = .mask_vals(object, mask, value)
  object = oro.nifti::calibrateImage(object)
  return(object)
}

#' @rdname mask_vals-methods
#' @aliases mask_vals<-,nifti,ANY,ANY-method
#' @export
setMethod("mask_vals<-", 
          signature(
            object = "nifti",
            mask = "ANY",
            value = "ANY"), 
          .cal_mask_vals)

#' @rdname mask_vals-methods
#' @aliases mask_vals<-,anlz,ANY,ANY-method
#' @export
setMethod("mask_vals<-", 
          signature(
            object = "anlz",
            mask = "ANY",
            value = "ANY"), 
          .cal_mask_vals
)

#' @rdname mask_vals-methods
#' @aliases mask_vals<-,array,ANY,ANY-method
#' @export
setMethod("mask_vals<-", 
          signature(
            object = "array",
            mask = "ANY",
            value = "ANY"), 
          .mask_vals)
