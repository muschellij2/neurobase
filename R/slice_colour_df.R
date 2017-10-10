#' @title Slice a Image Color Data.frame 
#' @description Slices a image color \code{data.frame} along the 3 planes (axial,
#' coronal, sagittal) and returns it in a ggplot-ready format for faceting.
#'
#' @param img_df an image \code{data.frame}, usually from 
#' \code{\link{img_colour_df}}.  Must have the columns: \code{dim1},
#' \code{dim2}, \code{dim3}, \code{colour}, and \code{value}.
#' @param xyz coordinates to slice the \code{data.frame} in x, y, and z - domains
#'
#' @return A \code{data.frame} with x and y coordinates, colour, and intensity
#' values, along with the associated planes that were sliced.
#' @export
#'
#' @examples
#' img = nifti(array(rnorm(10^3), dim = rep(10, 3)))
#' df = img_colour_df(img)
#' sliced = slice_colour_df(df, c(5, 5, 4))
slice_colour_df = function(
  img_df, 
  xyz = NULL) {
  
  if (is.null(xyz)) {
    xyz = floor(colMeans(img_df[, c("dim1", "dim2", "dim3")]))
  }
  
  v3 = img_df[ img_df$dim3 == xyz[3],
               c("dim1", "dim2", "colour", "value")]
  v2 = img_df[ img_df$dim2 == xyz[2],
               c("dim1", "dim3", "colour", "value")]
  v1 = img_df[ img_df$dim1 == xyz[1],
               c("dim2", "dim3", "colour", "value")]
  v1$plane = 1
  v2$plane = 2
  v3$plane = 3
  
  L = list(v1, v2, v3)
  L = lapply(L, function(x){
    colnames(x) = c("x", "y", "colour", "value", "plane")
    x
  })
  L = mapply(function(x, ind) {
    xx = xyz[-ind]
    x$xintercept = xx[1]
    x$yintercept = xx[2]
    x
  }, L, 1:3, SIMPLIFY = FALSE)
  
  L = do.call("rbind", L)
  L = as.data.frame(L)
  L$plane2 = L$plane
  L$plane2[ L$plane == 2 ] = "coronal"
  L$plane2[ L$plane == 3 ] = "axial"
  L$plane2[ L$plane == 1 ] = "sagittal"
  
  L$plane2 = factor(
    L$plane2,
    levels = c("coronal", "sagittal", "axial"))
  return(L)
}

