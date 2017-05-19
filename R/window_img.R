#' @title nifti image windower
#' @description Windows an image to min and max values and also changes
#' cal_max and cal_min parameters
#' @return Object of class nifti
#' @seealso \link{readnii}
#' @param x is either a character name for the image or an object of class nifti
#' @param window numeric of length 2 that gives min and max for window
#' @param replace either "window" if the any values outside of c(min, max) are
#' set to the min or max or "missing" for these to be set to NA
#' @param ... not used
#' @export
#' @examples
#'   set.seed(5)
#'   dims = rep(10, 3)
#'   arr = array(rnorm(prod(dims)), dim = dims)
#'   nim = oro.nifti::nifti(arr)
#'   window_img(nim, window = c(1, 5))
#'   window_img(nim, window = c(1, 5), replace = "missing")
#'   tfile = tempimg(nim)
#'   window_img(tfile)
#'   window_img(as.factor(tfile))
#'   arr = window_img(img_data(nim))
#'   rnim = RNifti::readNifti(tfile)
#'   window_img(rnim, window = c(1, 5))
#'   range(window_img(rnim, window = c(1, 5)))
#'   window_img(rnim, window = c(1, 5), replace = "missing")
#'   range(window_img(rnim, window = c(1, 5), replace = "missing"))
#'    
#'
window_img = function(x,
                      window = c(0, 100),
                      replace = c("window", "missing", "zero"),
                      ...) {
  UseMethod("window_img")
}


#' @export
#' @method window_img default
window_img.default = function(x,
                              window = c(0, 100),
                              replace = c("window", "missing", "zero"),
                              ...) {
  if (is.null(window)) {
    return(x)
  }
  stopifnot(length(window) == 2)
  
  repper = match.arg(replace)
  low = which(x < window[1])
  high = which(x > window[2])
  if (repper == "window") {
    x[low] = window[1]
    x[high] = window[2]
  }
  if (repper == "missing") {
    x[c(low, high)] = NA
  }
  if (repper == "zero") {
    x[c(low, high)] = 0
  }
  return(x)
}


#' @export
#' @method window_img nifti
window_img.nifti = function(x,
                            window = c(0, 100),
                            ...) {
  
  cal_min(x) = window[1]
  cal_max(x) = window[2]
  x = window_img.default(
    x = x, window = window, ...)
  return(x)
}

#' @export
#' @method window_img character
window_img.character = function(x,
                            ...) {
  x = readnii(x)
  x = window_img(
    x = x, ...)
  return(x)
}

#' @export
#' @method window_img factor
window_img.factor = function(x,
                                ...) {
  x = as.character(x)
  x = window_img(
    x = x, ...)
  return(x)
}

#' @export
#' @method window_img anlz
window_img.anlz =  function(x,
                            ...) {
  x = as.nifti(x)
  x = window_img.nifti(
    x = x, ...)
  return(x)
}

#' @export
#' @method window_img array
window_img.array = window_img.default

#' @export
#' @method window_img niftiImage
#' @importFrom RNifti dumpNifti 
window_img.niftiImage = function(x,
                                 window = c(0, 100),                                 
                                 ...) {
  hdr =  RNifti::dumpNifti(x)
  hdr$cal_min = window[1]
  hdr$cal_max = window[2]
  x = window_img.default(x = x, window = window, ...)
  x = RNifti::updateNifti(image = x, template = hdr)
  return(x)
}
