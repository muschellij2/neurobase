#' @title Orthographic Display, added options
#' @description Copy of \code{oro.nifti}'s \code{\link{orthographic}} function 
#' with some tweaks such as adding L/R designations for left and right
#' @return NULL
#' @seealso \link{orthographic}
#' @param x is an object of class nifti or similar.
#' @param y is an object of class nifti or similar for the overlay.
#' @param xyz is the coordinate for the center of the crosshairs.
#' @param w is the time point to be displayed (4D arrays only).
#' @param col is grayscale (by default).
#' @param col.y is hotmetal (by default).
#' @param zlim is the minimum and maximum `z' values passed into image.
#' @param zlim.y is the minimum and maximum `z' values passed into image 
#' for the overlay.
#' @param crosshairs is a logical value for the presence of crosshairs 
#' in all three orthogonal planes (default = TRUE).
#' @param NA.x Set any values of 0 in \code{x} to \code{NA}
#' @param NA.y Set any values of 0 in \code{y} to \code{NA}
#' @param col.crosshairs is the color of the crosshairs (default = red).
#' @param xlab is set to "" since all margins are set to zero.
#' @param ylab is set to "" since all margins are set to zero.
#' @param axes is set to FALSE since all margins are set to zero.
#' @param oma is the size of the outer margins in the par function.
#' @param mar is the number of lines of margin in the par function.
#' @param bg is the background color in the par function.
#' @param text allows the user to specify text to appear in 
#' the fourth (unused) pane.
#' @param text.color is the color of the user-specified text 
#' (default = ``white").
#' @param text.cex is the size of the user-specified text (default = 2).
#' @param text.x x coordinate for text 
#' @param text.y y coordinate for text
#' @param add.orient (logical) Add left/right, A/P, etc. orientation
#' @param mfrow (numeric) layout of the 3 slices
#' @param breaks (numeric) breaks for x to passed to 
#' \code{\link[graphics]{image}}
#' @param ybreaks (numeric) breaks for y to passed to 
#' \code{\link[graphics]{image}}
#' @param addlegend (logical) add legend?
#' @param leg.x (numeric) x coord for legend
#' @param leg.y (numeric) y coord for legend
#' @param legend (character) legend text
#' @param leg.col (character) Colors for legend 
#' @param leg.title (character) title for legend 
#' @param leg.cex (numeric) \code{cex} for \code{\link{legend}}
#' @param window (vector) Length-2 vector to limit image to certain range
#' @param ycolorbar (logical) Should a colorbar for \code{y} be plotted
#' @param clabels Label for colorbar (see \code{\link{colorbar}})
#' @param add Should the y-plot be added or its own plot?  Used
#' in \code{double_ortho}
#' @param pdim Pixel dimensions if passing in arrays.  Will be overridden if 
#' \code{x} is a \code{nifti} object
#' @param useRaster logical; if TRUE a bitmap raster is used to 
#' plot the image instead of polygons.  Passed to 
#' \code{\link[graphics]{image}}.
#' @param mask If a mask is passed, \code{drop_empty_dim} is applied 
#' to both \code{x} and \code{y}
#' @param ... other arguments to the image function may be provided here.
#' @export
#' @import graphics
#' @importFrom grDevices col2rgb gray rgb
#' @examples 
#' x = oro.nifti::nifti(array(rnorm(1000), dim = rep(10, 3)))
#' ortho2(x)
#' y = x > 2
#' ortho2(x, y)
#' arr_x = as.array(x)
#' arr_y = as.array(y)
#' ortho2( arr_x)
#' ortho2( arr_x, arr_y)
ortho3 = function(x, y = NULL, xyz = NULL, w = 1, col = gray(0:64/64), 
                  col.y = hotmetal(), zlim = NULL, zlim.y = NULL, 
                  NA.x = FALSE,
                  NA.y = TRUE,
                  crosshairs = TRUE, 
                  col.crosshairs = "red", xlab = "", ylab = "", axes = FALSE, 
                  oma = c(0, 0, 0, ifelse(ycolorbar, 5, 0)), 
                  mar = rep(0, 4), bg = "black", text = NULL, 
                  text.color = "white", text.cex = 2, 
                  text.x=32,
                  text.y=32,
                  add.orient=TRUE,
                  mfrow=c(2,2), ybreaks = NULL, breaks=NULL,
                  addlegend = FALSE,
                  leg.x=32,
                  leg.y=32,
                  legend,
                  leg.col,
                  leg.title = NULL,
                  leg.cex,
                  window=NULL,
                  ycolorbar = FALSE,
                  clabels = TRUE,
                  add = TRUE,
                  pdim = NULL,
                  useRaster = TRUE,
                  mask = NULL,
                  ...) 
{
  if (!is.null(mask)) {
    mask = check_nifti(mask, allow.array = TRUE)
    dd = dropEmptyImageDimensions(mask, keep_ind = TRUE)
    keep_inds = dd$inds
    rm(list = "dd");
  }
  x = check_nifti(x, allow.array = TRUE)
  if (!is.null(y)) {
    if (!all(dim(x)[1:3] == dim(y)[1:3])) {
      stop("dimensions of \"x\" and \"y\" must be equal")
    }
  }
  x_is_nifti = FALSE
  if (inherits(x, "nifti")) {
    x_is_nifti = TRUE
    if (!is.null(mask)) {
      x = apply_empty_dim(img = x, inds = keep_inds)
    }
    if (!is.null(window)) {
      x = window_img(x, window = window, replace = "window")
      #       x@cal_min = window[1]
      #       x@cal_max = window[2]
      #       x[ x < window[1] ] = window[1]
      #       x[ x >= window[2] ] = window[2]
    }
  } else {
    if (!is.null(mask)) {
      x = x[keep_inds[[1]], keep_inds[[2]], keep_inds[[3]]]    
    }
  }
  
  X <- nrow(x)
  Y <- ncol(x)
  Z <- nsli(x)
  W <- ntim(x)
  # mXY = max(X, Y)
  lr.shift = 4
  ud.shift = 6
  if (!is.null(y)) {
    y = check_nifti(y, allow.array = TRUE)
    y_is_nifti = FALSE
    if (inherits(y, "nifti")) {
      y_is_nifti = TRUE  
      if (!is.null(mask)) {
        y = apply_empty_dim(img = y, inds = keep_inds)
      }
    } else {
      if (!is.null(mask)) {
        y = y[keep_inds[[1]], keep_inds[[2]], keep_inds[[3]]]
      }
    }
    if (NA.y) {
      y[ y == 0 ] = NA
      if (all(is.na(y))) {
        stop(paste0("y has no non-zero values and NA.y = TRUE.  ", 
                    "Either remove the overlay, or set NA.y = FALSE"))
      }
    }
    if (y_is_nifti) {
      y = cal_img(y)
      glmax(y) = cal.max(y)
      glmin(y) = cal.min(y)       
    }
  }
  if (NA.x) {
    x[ x == 0 ] = NA
  }
  if (x_is_nifti) {
    x = cal_img(x)
  }  
  if (is.null(xyz)) {
    xyz <- ceiling(c(X, Y, Z)/2)
  }
  
  if (X == 0 || Y == 0 || Z == 0) {
    stop("size of NIfTI volume is zero, nothing to plot")
  }  
  zlim = zlimmer(x, zlim = zlim)

  if (is.null(breaks)) {
    breaks <- c(min(x, zlim, na.rm = TRUE), 
                seq(min(zlim, na.rm = TRUE),
                    max(zlim, na.rm = TRUE), 
                    length = length(col) - 1),
                max(x, zlim, na.rm = TRUE))
  }
  
  zlim.y = zlimmer(y, zlim = zlim.y)
  
  
  oldpar <- par(no.readonly = TRUE)
  par(mfrow = mfrow, oma = oma, mar = mar, bg = bg)
  
  if (x_is_nifti) {
    pdim = pixdim(x)
  } else {
    if (is.null(pdim)) {
      pdim = rep(1, 4)
    } 
  }
  stopifnot(length(pdim) >= 4)
  xyz_mm = xyz * pdim[2:4]
  
  
  if (!is.null(y)) {
    y = as.array(y)
  }  
  if (!all(is.na(W))) {
    if (any(w < 1 || w > W)) {
      stop("volume \"w\" out of range")
    }
    x = x[, , , w]
    if (!is.null(y)) {
      y = y[, , , w]
    }    
  }
  

  
  xxaxes = axes
  xxuseRaster = useRaster
  plot_xmat = function(x_inds, y_inds, mat, 
                       breaks, 
                       col, 
                       zlim, ...) {
    args = list(x = x_inds, y = y_inds, 
                z = mat, 
                asp = 1, 
                col = col, 
                zlim = zlim, 
                breaks = breaks, xlab = ylab, 
                ylab = xlab, axes = xxaxes, 
                useRaster = xxuseRaster, ... = ...)
    # if (is.null(args$breaks)) {
    #   args$breaks = NULL
    # }
    do.call(graphics::image.default, args)
  }
  plot_ymat = function(x_inds, 
                       y_inds, 
                       mat, 
                       run_breaks = ybreaks, 
                       run_col = col.y,
                       run_zlim = zlim.y, ...) {
      plot_xmat(x_inds, y_inds, mat, 
                breaks = run_breaks, 
                col = run_col, 
                zlim = run_zlim, ...)
  }
  xpdim = pdim[2]
  ypdim = pdim[4]
  x_inds = 1:X * xpdim
  y_inds = 1:Z * ypdim
  
  make_mat = function(r) {
    r[, xyz[2], ]
  }
  mat = make_mat(x)
  
  plot_xmat(x_inds = x_inds, y_inds = y_inds, 
            mat = mat, breaks = breaks,
            col = col, 
            zlim = zlim,
            ...)
  chairs = function() {
    abline(h = xyz_mm[3], v = xyz_mm[1], col = col.crosshairs)
  }
  annotate = function(){
    text("L", x = (X + lr.shift) * xpdim, y = (Z/2) * ypdim, 
         las = 1, col = "white")
    text("R", x = (-lr.shift) * xpdim, y = (Z/2) * ypdim, 
         las = 1, col = "white")
    text("S", x = (X/2 - .5) * xpdim, y = (Z - ud.shift) * ypdim, 
         las = 1, col = "white")
    text("I", x = (X/2 - .5) * xpdim, y = (ud.shift) * ypdim, 
         las = 1, col = "white")
  }  
  if (!add & crosshairs) {
    chairs()
  }
  if (!add & add.orient) {
    annotate()
  }  

  if (!is.null(y)) {
    plot_ymat(x_inds, y_inds, make_mat(y), ...)
  }
  if (crosshairs) {
    chairs()
  }
  if (add.orient) {
    annotate()
  }
  
  xpdim = pdim[3]
  ypdim = pdim[4]  
  x_inds = 1:Y * xpdim
  y_inds = 1:Z * ypdim
  make_mat = function(r) {
    r[ xyz[1], , ]
  }
  mat = make_mat(x)
  
  plot_xmat(x_inds = x_inds, y_inds = y_inds, 
            mat = mat, breaks = breaks,
            col = col, 
            zlim = zlim,
            ...)
  chairs = function() {
    abline(h = xyz_mm[3], v = xyz_mm[2], col = col.crosshairs)
  }
  annotate = function(){
    text("A", x = (Y - 1) * xpdim, y = (Z/2) * ypdim, las = 1, col = "white")
    text("P", x = (0 + 1) * xpdim, y = (Z/2) * ypdim, las = 1, col = "white")
    text("S", x = (Y/2 - .5) * xpdim, y = (Z - ud.shift) * ypdim, las = 1, col = "white")
    text("I", x = (Y/2 - .5) * xpdim, y = (ud.shift) * ypdim, las = 1, col = "white")
  }  
  if (!add & crosshairs) {
    chairs()
  }
  if (!add & add.orient) {
    annotate()
  }  
  
  if (!is.null(y)) {
    plot_ymat(x_inds, y_inds, make_mat(y), ...)
  }
  if (crosshairs) {
    chairs()
  }
  if (add.orient) {
    annotate()
  }   
  
  xpdim = pdim[2]
  ypdim = pdim[3]  
  x_inds = 1:X * xpdim
  y_inds = 1:Y * ypdim
  make_mat = function(r) {
    r[, , xyz[3]]
  }
  mat = make_mat(x)
  plot_xmat(x_inds = x_inds, y_inds = y_inds, 
            mat = mat, breaks = breaks,
            col = col, 
            zlim = zlim,
            ...)
  
  chairs = function() {
    abline(h = xyz_mm[2], v = xyz_mm[1], col = col.crosshairs)
  }
  annotate = function(){
    text("L", x = (X + lr.shift) * xpdim, y = (Y/2) * ypdim, las = 1, col = "white")
    text("R", x = (-lr.shift) * xpdim, y = (Y/2) * ypdim, las = 1, col = "white")
    text("A", x = (X/2 - .5) * xpdim, y = (Y - ud.shift) * ypdim, las = 1, col = "white")
    text("P", x = (X/2 - .5) * xpdim, y = (ud.shift) * ypdim, las = 1, col = "white")
  }
  if (!add & crosshairs) {
    chairs()
  }
  if (!add & add.orient) {
    annotate()
  }    
  if (!is.null(y)) {
    plot_ymat(x_inds, y_inds, make_mat(y), ...)
  }
  if (crosshairs) {
    chairs()
  }
  if (add.orient) {
    annotate()
  }   
  
  
  if (!is.null(text) | addlegend) {
    suppressWarnings({
      graphics::image(1:64, 1:64, matrix(NA, 64, 64), xlab = "", 
                      ylab = "", axes = FALSE,
                      useRaster = useRaster,
                      ...)
    })
    if (addlegend) {
      legend(x = leg.x, y = leg.y, 
             legend = legend, 
             pch = rep(15, length(leg.col)),
             col = leg.col, 
             cex = leg.cex, 
             text.col = "white",
             title = leg.title)    
    }
    if (!is.null(text)) {
      text(labels = text, x = text.x, y = text.y, col = 
             text.color, cex = text.cex)
    }    
  }
  
  par(oldpar)
  if (!is.null(y)) {
    if (is.null(ybreaks)) {
      if (ycolorbar) {
        warning("colorbar not supported if ybreaks unspecified")
      }
    } else {
      if (ycolorbar) {
        alpha = function(col, alpha = 1) {
          cols = t(col2rgb(col, alpha = FALSE)/255)
          rgb(cols, alpha = alpha)
        }        
        colorbar(breaks = ybreaks, col = alpha(col.y, 1), 
                 text.col = "white", 
                 labels = clabels)
      }
    }
  }
  invisible()
}
