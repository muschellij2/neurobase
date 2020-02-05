#' @title Convert Image to Data.frame with Colors
#' @description Takes in an image and a color scheme, converts that image
#' into a \code{data.frame} with the data and a color mapping.
#'
#' @param img an object to be coerced to \code{nifti} using 
#' \code{\link{check_nifti}}
#' @param zlim Limits for the domain of the intensities
#' @param breaks Breaks for the intensities to map to colors
#' @param col Colors to map intensities
#'
#' @return A \code{data.frame} with the first columns being the x,y,z (maybe t)
#' coordinates (named \code{dim} and the dimension number), a \code{value} 
#' column that contains the intensity information, and a \code{colour} column
#' representing the color that voxel maps to
#' @export
#'
#' @examples
#' img = nifti(array(rnorm(10^3), dim = rep(10, 3)))
#' df = img_colour_df(img)
#' df = img_color_df(img)
img_colour_df = function(
  img,
  zlim = NULL,
  breaks = NULL,
  col = gray(0:64/64)) {
  
  img = check_nifti(img, allow.array = TRUE)
  dimg = dim(img)
  
  zlim = zlimmer(img, zlim = zlim)
  img = c(img)

  if (is.null(breaks)) {
    r = range(img, na.rm = TRUE)
    breaks <- c(min(r[1], zlim, na.rm = TRUE),
                seq(min(zlim, na.rm = TRUE),
                    max(zlim, na.rm = TRUE),
                    length = length(col) - 1),
                max(r[2], zlim, na.rm = TRUE))
  }
  
  
  L = lapply(dimg, seq)
  eg = expand.grid(L)
  colnames(eg) = paste0("dim", seq(ncol(eg)))
  # eg$value = img[as.matrix(eg)]
  eg$value = c(img)
  eg$zi = .bincode(eg$value, breaks, TRUE, TRUE) - 1L
  eg$colour = col[ eg$zi + 1]
  
  return(eg)
}

#' @rdname img_colour_df
#' @param ... not used
#' @note \code{img_color_df} is a duplicate of \code{img_colour_df}
#' @export
img_color_df = function(...) {
  img_colour_df(...)
}